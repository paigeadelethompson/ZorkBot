use anyhow::{Result, anyhow};
use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::fs;
use crate::z::zmachine::Zmachine;
use crate::z::traits::UI;
use crate::z::options::Options;
use std::sync::mpsc::{self, Sender, Receiver};
use std::sync::Arc;
use crate::irc::IrcClient;

// Add status bar message type
#[derive(Clone, Debug)]
pub enum UiMessage {
    StatusBar { left: String, right: String },
    Message { mtype: String, text: String },
}

// Simple UI implementation that collects output
#[derive(Clone)]
pub(crate) struct CollectingUI {
    pub(crate) output: String,
    pub(crate) message_sender: Option<Sender<UiMessage>>,
    irc_client: Arc<IrcClient>,
    channel: String,
    buffer: String,  // Add a buffer for collecting output
}

impl CollectingUI {
    fn new_instance() -> Self {
        panic!("CollectingUI must be created with an IRC client");
    }

    fn with_sender_and_irc(sender: Sender<UiMessage>, irc_client: Arc<IrcClient>, channel: String) -> Self {
        Self {
            output: String::new(),
            message_sender: Some(sender),
            irc_client,
            channel,
            buffer: String::new(),
        }
    }

    pub(crate) fn take_output(&mut self) -> String {
        // Flush any remaining buffered content
        self.flush();
        std::mem::take(&mut self.output)
    }

    fn send_buffered_output(&mut self) {
        if !self.buffer.trim().is_empty() {
            // Split into lines and clean up each line
            let lines: Vec<String> = self.buffer
                .split('\n')
                .map(|line| line.trim().to_string())
                .filter(|line| !line.is_empty())
                .collect();

            // Send each line as a separate message to avoid truncation
            for line in lines {
                if let Err(e) = self.irc_client.send_privmsg(&self.channel, &line) {
                    eprintln!("Error sending to IRC: {}", e);
                }
                self.output.push_str(&line);
                self.output.push('\n');
            }
            self.buffer.clear();
        }
    }
}

unsafe impl Send for CollectingUI {}
unsafe impl Sync for CollectingUI {}

impl UI for CollectingUI {
    fn new() -> Box<Self> where Self: Sized {
        panic!("CollectingUI must be created with an IRC client");
    }

    fn clear(&self) {
        // No-op for IRC bot - we handle clearing via take_output
    }

    fn print(&mut self, text: &str) {
        println!("[DEBUG] UI print: {}", text);
        self.buffer.push_str(text);
        
        // Send buffer on any of these conditions:
        // 1. Contains newline
        // 2. Ends with sentence punctuation
        // 3. Contains prompt characters ('>' or ']')
        // 4. Reaches a reasonable length
        if text.contains('\n') || 
           text.ends_with('.') || 
           text.ends_with('?') || 
           text.ends_with('!') ||
           text.contains('>') ||
           text.contains(']') ||
           self.buffer.len() > 200 {
            self.send_buffered_output();
        }
    }

    fn debug(&mut self, text: &str) {
        println!("[DEBUG] UI debug: {}", text);
        // Debug messages are not sent to IRC
    }

    fn print_object(&mut self, object: &str) {
        println!("[DEBUG] UI print_object: {}", object);
        self.buffer.push_str(object);
        self.send_buffered_output();
    }

    fn set_status_bar(&self, left: &str, right: &str) {
        println!("[DEBUG] UI status_bar: {} | {}", left, right);
        if let Err(e) = self.irc_client.send_privmsg(&self.channel, &format!("Status: {} | {}", left, right)) {
            eprintln!("Error sending status to IRC: {}", e);
        }
    }

    fn reset(&self) {
        // No-op - we handle reset via the ZMachine struct
    }

    fn get_user_input(&self) -> String {
        // For IRC bot, we don't actually get input here
        // Instead, we return empty string and let the command handler
        // process input through send_command
        String::new()
    }

    fn flush(&mut self) {
        self.send_buffered_output();
    }

    fn message(&self, mtype: &str, msg: &str) {
        println!("[DEBUG] UI message: {} - {}", mtype, msg);
        if let Err(e) = self.irc_client.send_privmsg(&self.channel, &format!("[{}] {}", mtype, msg)) {
            eprintln!("Error sending message to IRC: {}", e);
        }
    }
}

pub struct MemeZMachine {
    machine: Zmachine,
    game_data: Vec<u8>,
    pub(crate) ui: Box<CollectingUI>,
    pub(crate) message_receiver: Option<std::sync::mpsc::Receiver<UiMessage>>,
    pagination_state: Option<String>,
}

impl MemeZMachine {
    pub fn new(game_path: &Path, irc_client: Arc<IrcClient>, irc_channel: String) -> Result<MemeZMachine> {
        println!("[DEBUG] Creating new ZMachine from path: {:?}", game_path);
        let mut game_data = Vec::new();
        File::open(game_path)?.read_to_end(&mut game_data)?;
        println!("[DEBUG] Read {} bytes of game data", game_data.len());
        
        // Create message channel
        println!("[DEBUG] Creating message channel");
        let (sender, receiver) = mpsc::channel();
        let ui = Box::new(CollectingUI::with_sender_and_irc(sender, irc_client, irc_channel));
        
        println!("[DEBUG] Creating Z-machine with options");
        let options = Options::default();
        let mut machine = Zmachine::new(game_data.clone(), ui.clone(), options);
        
        // Initialize the machine
        println!("[DEBUG] Running initial machine setup");
        let mut needs_input = false;
        while !needs_input {
            needs_input = machine.step();
        }
        println!("[DEBUG] Initial machine setup complete");
        
        Ok(MemeZMachine {
            machine,
            game_data,
            ui,
            message_receiver: Some(receiver),
            pagination_state: None,
        })
    }

    pub fn reset(&mut self) -> Result<()> {
        let sender = self.message_receiver.take().map(|_r| {
            let (s, r) = mpsc::channel();
            self.message_receiver = Some(r);
            s
        }).unwrap_or_else(|| {
            let (s, r) = mpsc::channel();
            self.message_receiver = Some(r);
            s
        });

        // Create a dummy UI since we'll replace it when we get a new command
        let ui = Box::new(CollectingUI::with_sender_and_irc(
            sender,
            self.ui.irc_client.clone(),
            self.ui.channel.clone()
        ));
        let options = Options::default();
        
        self.machine = Zmachine::new(self.game_data.clone(), ui.clone(), options);
        self.ui = ui;
        Ok(())
    }

    pub fn send_command(&mut self, command: &str) -> Result<(String, Vec<UiMessage>)> {
        println!("[DEBUG] send_command called with: '{}'", command);
        
        // Clear any existing output before processing new command
        println!("[DEBUG] Clearing existing output");
        self.ui.as_mut().take_output();

        // Special case for help command - handled locally
        if command.trim().eq_ignore_ascii_case("help") || command == "?" {
            println!("[DEBUG] Processing help command locally");
            self.ui.as_mut().print(
                "Common text adventure commands:\n\
                LOOK (L) - Look around\n\
                INVENTORY (I) - Check inventory\n\
                EXAMINE/X [THING] - Look at something\n\
                TAKE/GET [THING] - Pick up object\n\
                DROP [THING] - Put down object\n\
                N,S,E,W,NE,NW,SE,SW - Move\n\
                SAVE/RESTORE - Save/load game\n\
                QUIT - End game"
            );
            self.ui.as_mut().flush();
            return Ok((self.ui.as_mut().take_output(), Vec::new()));
        }

        // Process command in Z-machine
        if !command.is_empty() {
            println!("[DEBUG] Sending command to Z-machine: '{}'", command);
            // Send command to Z-machine
            self.machine.handle_input(command.to_string());
            
            println!("[DEBUG] Running Z-machine until input needed");
            // Run until machine needs input
            let mut steps = 0;
            while !self.machine.step() {
                steps += 1;
                if steps % 1000 == 0 {
                    println!("[DEBUG] Z-machine executed {} steps", steps);
                }
            }
            println!("[DEBUG] Z-machine completed after {} steps", steps);
            
            // Ensure all output is flushed
            println!("[DEBUG] Flushing UI output");
            self.ui.as_mut().flush();
        }

        // Get output and format it
        let output = self.ui.as_mut().take_output();
        println!("[DEBUG] Raw output from Z-machine: '{}'", output);
        
        let formatted_output = if output.trim().is_empty() {
            println!("[DEBUG] No output received, showing current location");
            // If no output, show current location
            let (room_num, room_name) = self.machine.get_current_room();
            println!("[DEBUG] Current room: {} ({})", room_name, room_num);
            format!("You are in {} ({})", room_name, room_num)
        } else {
            println!("[DEBUG] Formatting output");
            output.lines()
                .map(|line| line.trim())
                .filter(|line| !line.is_empty())
                .collect::<Vec<_>>()
                .join("\n")
        };

        // Collect any UI messages
        let mut messages = Vec::new();
        if let Some(receiver) = &self.message_receiver {
            println!("[DEBUG] Collecting UI messages");
            while let Ok(msg) = receiver.try_recv() {
                println!("[DEBUG] Received UI message: {:?}", msg);
                messages.push(msg);
            }
        }

        println!("[DEBUG] Returning formatted output: '{}'", formatted_output);
        Ok((formatted_output, messages))
    }

    pub fn save_state<P: AsRef<Path>>(&mut self, _save_path: P) -> Result<()> {
        // For now, just return Ok since we don't have direct access to save functionality
        Ok(())
    }

    pub fn load_state<P: AsRef<Path>>(&mut self, _save_path: P) -> Result<()> {
        // For now, just return Ok since we don't have direct access to load functionality
        Ok(())
    }

    pub fn get_debug_info(&self) -> Result<String> {
        let (room_num, room_name) = self.machine.get_current_room();
        let mut info = String::new();
        info.push_str("Z-Machine Debug Info:\n");
        info.push_str(&format!("Current Room: {} ({})\n", room_name, room_num));
        
        // Add object tree for current room
        if room_num > 0 {
            info.push_str("\nRoom Contents:\n");
            let room_obj = self.machine.get_object_tree();
            info.push_str(&room_obj.to_string());
        }
        
        Ok(info)
    }

    pub fn get_memory_dump(&self, addr: u16, size: u16) -> Result<String> {
        let mut dump = String::new();
        let end_addr = addr.saturating_add(size);
        
        for i in addr..end_addr {
            if i % 16 == 0 {
                if !dump.is_empty() {
                    dump.push('\n');
                }
                dump.push_str(&format!("{:04x}:", i));
            }
            dump.push_str(" ??"); // We don't have direct memory access
        }
        
        Ok(dump)
    }

    pub fn get_stack_trace(&self) -> Result<String> {
        let mut trace = String::new();
        trace.push_str("Call stack:\n");
        trace.push_str("(Stack trace not available - using debug info instead)\n\n");
        
        // Add current room info as it's the most useful debug info we have
        let (room_num, room_name) = self.machine.get_current_room();
        trace.push_str(&format!("Current location: {} ({})\n", room_name, room_num));
        
        Ok(trace)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::fs;

    fn create_test_game() -> PathBuf {
        let temp_dir = std::env::temp_dir();
        let game_path = temp_dir.join("test_game.z5");
        
        // Create a minimal Z-machine game file with enough data
        let mut header = vec![
            0x05, 0x00, 0x00, 0x00, 0x00, 0x00, // Version 5
            0x00, 0x00,                         // Flags
            0x00, 0x00,                         // Release number
            0x00, 0x00,                         // High memory
            0x00, 0x00,                         // Initial PC
            0x00, 0x00,                         // Dictionary
            0x00, 0x00,                         // Object table
            0x00, 0x00,                         // Global variables
            0x00, 0x00,                         // Static memory
            0x00, 0x00,                         // Abbreviations table
        ];
        
        // Add enough padding to satisfy memory access requirements
        header.extend(vec![0; 0x4000]); // 16KB of memory
        
        // Set some header fields to point to valid memory locations
        header[0x0E] = 0x40; // Static memory starts at 0x4000
        header[0x04] = 0x40; // High memory starts at 0x4000
        
        fs::write(&game_path, header).expect("Failed to create test game file");
        game_path
    }

    #[test]
    fn test_zmachine_creation() {
        let game_path = create_test_game();
        let result = MemeZMachine::new(&game_path, Arc::new(IrcClient::new()), String::new());
        assert!(result.is_ok());
        fs::remove_file(game_path).expect("Failed to clean up test game file");
    }

    #[test]
    fn test_send_command() {
        let game_path = create_test_game();
        let mut zm = MemeZMachine::new(&game_path, Arc::new(IrcClient::new()), String::new()).expect("Failed to create ZMachine");
        
        // Since this is a test game with no real content, we expect empty output
        let (output, messages) = zm.send_command("look").expect("Failed to send command");
        assert!(output.is_empty());
        
        fs::remove_file(game_path).expect("Failed to clean up test game file");
    }

    #[test]
    fn test_reset() {
        let game_path = create_test_game();
        let mut zm = MemeZMachine::new(&game_path, Arc::new(IrcClient::new()), String::new()).expect("Failed to create ZMachine");
        
        // Reset should succeed
        assert!(zm.reset().is_ok());
        
        fs::remove_file(game_path).expect("Failed to clean up test game file");
    }

    #[test]
    fn test_debug_info() {
        let game_path = create_test_game();
        let zm = MemeZMachine::new(&game_path, Arc::new(IrcClient::new()), String::new()).expect("Failed to create ZMachine");
        
        let debug_info = zm.get_debug_info().expect("Failed to get debug info");
        assert!(debug_info.contains("Current Room:"));
        
        fs::remove_file(game_path).expect("Failed to clean up test game file");
    }

    #[test]
    fn test_memory_dump() {
        let game_path = create_test_game();
        let zm = MemeZMachine::new(&game_path, Arc::new(IrcClient::new()), String::new()).expect("Failed to create ZMachine");
        
        let dump = zm.get_memory_dump(0, 16).expect("Failed to get memory dump");
        assert!(dump.contains("0000:"));
        assert!(dump.lines().count() == 1); // Should have one line for 16 bytes
        
        fs::remove_file(game_path).expect("Failed to clean up test game file");
    }

    #[test]
    fn test_stack_trace() {
        let game_path = create_test_game();
        let zm = MemeZMachine::new(&game_path, Arc::new(IrcClient::new()), String::new()).expect("Failed to create ZMachine");
        
        let trace = zm.get_stack_trace().expect("Failed to get stack trace");
        assert!(!trace.is_empty());
        
        fs::remove_file(game_path).expect("Failed to clean up test game file");
    }
} 