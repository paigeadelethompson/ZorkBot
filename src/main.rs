use crate::irc::IrcClient;
use crate::zmachine::UiMessage;
use anyhow::Result;
use tokio::sync::Mutex;
use std::sync::Arc;
use futures_util::stream::TryStreamExt;
use ::irc::client::prelude::{Config, Command, Message};

mod zmachine;
mod game_manager;
mod config;
mod z;
mod irc;


#[tokio::main]
async fn main() -> Result<()> {
    println!("Starting IRC bot...");
    let config = Arc::new(config::Config::load("config.toml")?);
    println!("Loaded config: server={}, nickname={}", config.irc.server, config.irc.nickname);
    
    let irc_config = Config {
        nickname: Some(config.irc.nickname.clone()),
        server: Some(config.irc.server.clone()),
        channels: config.irc.channels.clone(),
        password: config.irc.password.clone(),
        port: Some(config.irc.port),
        use_tls: Some(config.irc.use_tls),
        ..Config::default()
    };

    // Create a single client for both sending and receiving messages
    println!("Connecting to IRC server...");
    let mut client: IrcClient = IrcClient::from_config(irc_config).await?;
    println!("Connected to server successfully");

    // Get the stream before wrapping in Arc
    println!("Starting message stream...");
    let mut stream = client.stream()?;
    println!("Stream created successfully");

    // Create the game manager
    let client_arc = Arc::new(client);
    let game_manager = Arc::new(Mutex::new(game_manager::GameManager::new(
        config.paths.games_dir.clone(),
        config.paths.saves_dir.clone(),
        client_arc.clone(),
    )));

    println!("Identifying with server...");
    client_arc.identify()?;
    println!("Successfully identified with server");

    // Join channels
    println!("Joining channels: {:?}", config.irc.channels);
    for channel in &config.irc.channels {
        println!("Joining channel: {}", channel);
        if let Err(e) = client_arc.send_join(channel) {
            eprintln!("Error joining channel {}: {}", channel, e);
        } else {
            println!("Successfully joined channel: {}", channel);
        }
    }

    // Spawn a task to handle incoming messages
    let message_handler = tokio::spawn(async move {
        println!("Entering main message loop...");
        
        while let Some(message) = stream.try_next().await? {
            println!("Processing message: {:?}", message);
            if let Err(e) = handle_message(
                client_arc.clone(),
                game_manager.clone(),
                config.clone(),
                message
            ).await {
                eprintln!("Error handling message: {}", e);
            }
        }
        Ok::<(), anyhow::Error>(())
    });

    // Wait for the message handler to complete
    message_handler.await??;
    
    Ok(())
}

async fn handle_message(
    client: Arc<IrcClient>,
    game_manager: Arc<Mutex<game_manager::GameManager>>,
    config: Arc<config::Config>,
    message: Message,
) -> Result<()> {
    println!("Handling message: {:?}", message);
    if let Command::PRIVMSG(channel, content) = message.command {
        println!("Received PRIVMSG in channel {} with content: {}", channel, content);
        let sender = message.prefix
            .map(|p| p.to_string())
            .and_then(|p| p.split('!').next().map(|s| s.to_string()))
            .unwrap_or_else(|| "".to_string());
        println!("Message from sender: {}", sender);
        
        // Check if message starts with command prefix
        if !content.starts_with(&config.commands.prefix) {
            println!("Message does not start with command prefix: {}", config.commands.prefix);
            return Ok(());
        }
        
        // Remove prefix and split into words
        let content = content[config.commands.prefix.len()..].trim();
        println!("Processing command: {}", content);
        match content.split_whitespace().collect::<Vec<_>>().as_slice() {
            [cmd] if *cmd == config.commands.help => {
                println!("Executing help command");
                let help_text = vec![
                    format!("Available commands (prefix: {})", config.commands.prefix),
                    format!("{}{} - List available games", config.commands.prefix, config.commands.games),
                    format!("{}{} <number> - Start a specific game", config.commands.prefix, config.commands.play),
                    format!("{}{} - End current game", config.commands.prefix, config.commands.end),
                    format!("{}{} - Reset current game", config.commands.prefix, config.commands.reset),
                    format!("{}{} <text> - Send command to game", config.commands.prefix, config.commands.command),
                    format!("{}{} - Save game state", config.commands.prefix, config.commands.save),
                    format!("{}{} <number> - Load saved state", config.commands.prefix, config.commands.load),
                    format!("{}{} - List saved states", config.commands.prefix, config.commands.list_states),
                    format!("{}{} - Show next page of games", config.commands.prefix, config.commands.next),
                    format!("{}{} - Reset game listing to first page", config.commands.prefix, config.commands.pagination_reset),
                    format!("{}{} - Show this help", config.commands.prefix, config.commands.help),
                    format!("\nDebug Commands:"),
                    format!("{}{} - Show Z-machine debug info", config.commands.prefix, config.commands.debug),
                    format!("{}{} <hex_addr> <size> - Dump memory region", config.commands.prefix, config.commands.memory),
                    format!("{}{} - Show stack trace", config.commands.prefix, config.commands.stack),
                ];
                
                println!("Sending help text to channel: {}", channel);
                for line in help_text {
                    println!("Sending line: {}", line);
                    match client.send_privmsg(&channel, &line) {
                        Ok(_) => println!("Successfully sent line to channel"),
                        Err(e) => eprintln!("Error sending help text: {}", e),
                    }
                }
                println!("Help command completed");
            }
            [cmd] if *cmd == config.commands.games => {
                let mut manager = game_manager.lock().await;
                let (games, has_more) = manager.list_games()?;
                
                if games.is_empty() {
                    client.send_privmsg(&channel, "No games found")?;
                } else {
                    for game in games {
                        client.send_privmsg(&channel, &game)?;
                    }
                    if has_more {
                        client.send_privmsg(&channel, &format!("Use {}{} to see more games", 
                            config.commands.prefix, config.commands.next))?;
                    }
                }
            }
            [cmd, num_str] if *cmd == config.commands.play => {
                let num = num_str.parse().unwrap_or(0);
                let mut manager = game_manager.lock().await;
                if let Err(e) = manager.start_game(&channel, num) {
                    client.send_privmsg(&channel, &format!("Error starting game: {}", e))?;
                }
            }
            [cmd, rest @ ..] if *cmd == config.commands.command => {
                let command = rest.join(" ");
                let mut manager = game_manager.lock().await;
                
                match manager.send_command(&channel, &command) {
                    Ok((output, messages)) => {
                        // Send command output if present
                        if !output.trim().is_empty() {
                            client.send_privmsg(&channel, &output)?;
                        }
                        
                        // Process UI messages
                        for msg in messages {
                            match msg {
                                UiMessage::StatusBar { left, right } => {
                                    client.send_privmsg(&channel, &format!("Location: {} | {}", left, right))?;
                                }
                                UiMessage::Message { mtype, text } => {
                                    client.send_privmsg(&channel, &format!("{}: {}", mtype, text))?;
                                }
                            }
                        }
                    }
                    Err(e) => {
                        client.send_privmsg(&channel, &format!("Game error: {}", e))?;
                    }
                }
            }
            [cmd] if *cmd == config.commands.end => {
                let mut manager = game_manager.lock().await;
                if let Err(e) = manager.end_game(&channel) {
                    client.send_privmsg(&channel, &format!("Error: {}", e))?;
                } else {
                    client.send_privmsg(&channel, "Game ended")?;
                }
            }
            [cmd] if *cmd == config.commands.reset => {
                let mut manager = game_manager.lock().await;
                if let Err(e) = manager.reset_game(&channel) {
                    client.send_privmsg(&channel, &format!("Error: {}", e))?;
                } else {
                    client.send_privmsg(&channel, "Game reset")?;
                }
            }
            [cmd] if *cmd == config.commands.save => {
                let mut manager = game_manager.lock().await;
                match manager.save_state(&channel) {
                    Ok(msg) => {
                        client.send_privmsg(&channel, &msg)?;
                    }
                    Err(e) => {
                        client.send_privmsg(&channel, &format!("Error saving state: {}", e))?;
                    }
                }
            }
            [cmd] if *cmd == config.commands.list_states => {
                let manager = game_manager.lock().await;
                match manager.list_states() {
                    Ok(states) => {
                        if states.is_empty() {
                            client.send_privmsg(&channel, "No saved states found")?;
                        } else {
                            for state in states {
                                client.send_privmsg(&channel, &state)?;
                            }
                        }
                    }
                    Err(e) => {
                        client.send_privmsg(&channel, &format!("Error listing states: {}", e))?;
                    }
                }
            }
            [cmd, num] if *cmd == config.commands.load => {
                if let Ok(save_num) = num.parse::<usize>() {
                    let mut manager = game_manager.lock().await;
                    if let Err(e) = manager.load_state(&channel, save_num) {
                        client.send_privmsg(&channel, &format!("Error loading state: {}", e))?;
                    } else {
                        client.send_privmsg(&channel, "Game state loaded successfully")?;
                    }
                } else {
                    client.send_privmsg(&channel, "Invalid save state number")?;
                }
            }
            [cmd] if *cmd == config.commands.next => {
                let mut manager = game_manager.lock().await;
                match manager.next_page() {
                    Ok(()) => {
                        let (games, has_more) = manager.list_games()?;
                        for game in games {
                            client.send_privmsg(&channel, &game)?;
                        }
                        if has_more {
                            client.send_privmsg(&channel, &format!("Use {}{} to see more games", 
                                config.commands.prefix, config.commands.next))?;
                        }
                    }
                    Err(e) => {
                        client.send_privmsg(&channel, &format!("Error: {}", e))?;
                    }
                }
            }
            [cmd] if *cmd == config.commands.pagination_reset => {
                let mut manager = game_manager.lock().await;
                manager.reset_pagination();
                client.send_privmsg(&channel, "Game listing reset to first page")?;
            }
            [cmd] if *cmd == config.commands.debug => {
                let manager = game_manager.lock().await;
                if let Some(game) = manager.get_active_game(&channel) {
                    match game.get_debug_info() {
                        Ok(info) => {
                            for line in info.lines() {
                                client.send_privmsg(&channel, line)?;
                            }
                        }
                        Err(e) => {
                            client.send_privmsg(&channel, &format!("Error getting debug info: {}", e))?;
                        }
                    }
                } else {
                    client.send_privmsg(&channel, "No active game in this channel")?;
                }
            }
            [cmd, addr, size] if *cmd == config.commands.memory => {
                if let (Ok(addr), Ok(size)) = (u16::from_str_radix(addr, 16), size.parse::<u16>()) {
                    let manager = game_manager.lock().await;
                    if let Some(game) = manager.get_active_game(&channel) {
                        match game.get_memory_dump(addr, size) {
                            Ok(dump) => {
                                for line in dump.lines() {
                                    client.send_privmsg(&channel, line)?;
                                }
                            }
                            Err(e) => {
                                client.send_privmsg(&channel, &format!("Error dumping memory: {}", e))?;
                            }
                        }
                    } else {
                        client.send_privmsg(&channel, "No active game in this channel")?;
                    }
                } else {
                    client.send_privmsg(&channel, "Usage: !memory <hex_addr> <size>")?;
                }
            }
            [cmd] if *cmd == config.commands.stack => {
                let manager = game_manager.lock().await;
                if let Some(game) = manager.get_active_game(&channel) {
                    match game.get_stack_trace() {
                        Ok(trace) => {
                            for line in trace.lines() {
                                client.send_privmsg(&channel, line)?;
                            }
                        }
                        Err(e) => {
                            client.send_privmsg(&channel, &format!("Error getting stack trace: {}", e))?;
                        }
                    }
                } else {
                    client.send_privmsg(&channel, "No active game in this channel")?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

