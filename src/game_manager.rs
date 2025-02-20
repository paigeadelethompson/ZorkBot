use crate::zmachine::UiMessage;
use anyhow::{Result, anyhow};
use std::collections::HashMap;
use walkdir::WalkDir;
use std::path::PathBuf;
use crate::zmachine::MemeZMachine;
use std::fs;
use std::time::SystemTime;
use std::sync::Arc;
use crate::irc::IrcClient;

pub struct GameManager {
    games: Vec<PathBuf>,
    active_games: HashMap<String, MemeZMachine>,
    games_dir: String,
    saves_dir: String,
    pagination_offset: usize,
    games_per_page: usize,
    irc_client: Arc<IrcClient>,
}

impl GameManager {
    pub fn new(games_dir: String, saves_dir: String, irc_client: Arc<IrcClient>) -> Self {
        Self {
            games: Vec::new(),
            active_games: HashMap::new(),
            games_dir,
            saves_dir,
            pagination_offset: 0,
            games_per_page: 10,
            irc_client,
        }
    } 

    pub fn list_games(&mut self) -> Result<(Vec<String>, bool)> {
        self.scan_games()?;
        
        let start = self.pagination_offset;
        let end = (start + self.games_per_page).min(self.games.len());
        let has_more = end < self.games.len();

        let games = self.games[start..end].iter().enumerate()
            .map(|(i, path)| format!("{}: {}", i + start, path.file_name().unwrap().to_string_lossy()))
            .collect();

        Ok((games, has_more))
    }

    pub fn next_page(&mut self) -> Result<()> {
        self.scan_games()?;
        let next_offset = self.pagination_offset + self.games_per_page;
        if next_offset < self.games.len() {
            self.pagination_offset = next_offset;
            Ok(())
        } else {
            Err(anyhow!("No more games to show"))
        }
    }

    pub fn reset_pagination(&mut self) {
        self.pagination_offset = 0;
    }

    fn scan_games(&mut self) -> Result<()> {
        self.games.clear();
        for entry in WalkDir::new(&self.games_dir) {
            let entry = entry?;
            if entry.file_type().is_file() {
                if let Some(ext) = entry.path().extension() {
                    if let Some(ext_str) = ext.to_str() {
                        // Check for Z-machine file extensions (z3, z4, z5, z6, z7, z8)
                        if ext_str.starts_with('z') && ext_str.len() == 2 {
                            if let Ok(version) = ext_str[1..].parse::<u8>() {
                                if (3..=8).contains(&version) {
                                    self.games.push(entry.path().to_owned());
                                }
                            }
                        }
                    }
                }
            }
        }
        self.games.sort();
        Ok(())
    }

    pub fn start_game(&mut self, channel: &str, game_num: usize) -> Result<(String, Vec<UiMessage>)> {
        println!("[DEBUG] Starting game for channel {} with game number {}", channel, game_num);
        
        if self.active_games.contains_key(channel) {
            println!("[DEBUG] Error: Game already running in channel {}", channel);
            return Err(anyhow!("A game is already running in this channel"));
        }

        println!("[DEBUG] Scanning games directory");
        self.scan_games()?;
        
        println!("[DEBUG] Found {} games", self.games.len());
        let game_path = self.games.get(game_num)
            .ok_or_else(|| anyhow!("Invalid game number"))?;
        println!("[DEBUG] Selected game path: {:?}", game_path);

        // Create and initialize the Z-machine with the IRC client
        println!("[DEBUG] Creating new Z-machine instance");
        let mut zmachine = MemeZMachine::new(game_path, self.irc_client.clone(), channel.to_string())?;
        
        // Get initial output by sending an empty command
        let (output, messages) = zmachine.send_command("")?;
        
        // Store the game instance
        println!("[DEBUG] Storing game instance");
        self.active_games.insert(channel.to_string(), zmachine);
        
        // Reset pagination when starting a game
        self.reset_pagination();
        
        println!("[DEBUG] Game started successfully");
        Ok((output, messages))
    }

    pub fn end_game(&mut self, channel: &str) -> Result<()> {
        self.active_games.remove(channel)
            .ok_or_else(|| anyhow!("No active game in this channel"))?;
        Ok(())
    }

    pub fn reset_game(&mut self, channel: &str) -> Result<()> {
        if let Some(game) = self.active_games.get_mut(channel) {
            game.reset()?;
            Ok(())
        } else {
            Err(anyhow!("No active game in this channel"))
        }
    }

    pub fn send_command(&mut self, channel: &str, command: &str) -> Result<(String, Vec<UiMessage>)> {
        let game = self.active_games.get_mut(channel)
            .ok_or_else(|| anyhow!("No active game in this channel"))?;

        if command == "debug" {
            Ok((game.get_debug_info()?, Vec::new()))
        } else {
            game.send_command(command)
        }
    }

    pub fn save_state(&mut self, channel: &str) -> Result<String> {
        if let Some(game) = self.active_games.get_mut(channel) {
            // Create save_states directory if it doesn't exist
            fs::create_dir_all(&self.saves_dir)?;
            
            let timestamp = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)?
                .as_secs();
            
            let save_path = format!("{}/save_{}.sav", self.saves_dir, timestamp);
            game.save_state(&save_path)?;
            
            Ok(format!("Game state saved as '{}'", save_path))
        } else {
            Err(anyhow!("No active game in this channel"))
        }
    }

    pub fn list_states(&self) -> Result<Vec<String>> {
        let mut saves = Vec::new();
        
        fs::create_dir_all(&self.saves_dir)?;
        
        for entry in fs::read_dir(&self.saves_dir)? {
            let entry = entry?;
            if entry.path().extension().and_then(|s| s.to_str()) == Some("sav") {
                let filename = entry.file_name();
                let modified = entry.metadata()?.modified()?;
                saves.push((entry.path(), modified));
            }
        }
        
        // Sort by modification time
        saves.sort_by_key(|(_path, time)| *time);
        
        Ok(saves.iter().enumerate()
            .map(|(i, (path, time))| {
                format!("{}: {} ({})", 
                    i,
                    path.file_name().unwrap().to_string_lossy(),
                    humantime::format_rfc3339(*time)
                )
            })
            .collect())
    }

    pub fn load_state(&mut self, channel: &str, save_num: usize) -> Result<()> {
        let mut saves = Vec::new();
        for entry in fs::read_dir(&self.saves_dir)? {
            let entry = entry?;
            if entry.path().extension().and_then(|s| s.to_str()) == Some("sav") {
                let modified = entry.metadata()?.modified()?;
                saves.push((entry.path(), modified));
            }
        }
        
        saves.sort_by_key(|(_path, time)| *time);
        
        if let Some((save_path, _)) = saves.get(save_num) {
            if let Some(game) = self.active_games.get_mut(channel) {
                game.load_state(save_path)?;
                Ok(())
            } else {
                Err(anyhow!("No active game in this channel"))
            }
        } else {
            Err(anyhow!("Invalid save state number"))
        }
    }

    pub fn get_active_game(&self, channel: &str) -> Option<&MemeZMachine> {
        self.active_games.get(channel)
    }
} 