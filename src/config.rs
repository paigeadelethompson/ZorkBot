use anyhow::Result;
use serde::Deserialize;
use std::fs;
use std::path::Path;

#[derive(Deserialize)]
pub struct Config {
    pub irc: IrcConfig,
    pub paths: PathConfig,
    #[serde(default)]
    pub commands: CommandConfig,
}

#[derive(Deserialize)]
pub struct IrcConfig {
    pub nickname: String,
    pub server: String,
    pub channels: Vec<String>,
    #[serde(default)]
    pub password: Option<String>,
    #[serde(default = "default_port")]
    pub port: u16,
    #[serde(default = "default_use_tls")]
    pub use_tls: bool,
}

#[derive(Deserialize)]
pub struct PathConfig {
    #[serde(default = "default_games_dir")]
    pub games_dir: String,
    #[serde(default = "default_saves_dir")]
    pub saves_dir: String,
}

#[derive(Deserialize)]
#[derive(Default)]
pub struct CommandConfig {
    #[serde(default = "default_prefix")]
    pub prefix: String,
    #[serde(default = "default_cmd_games")]
    pub games: String,
    #[serde(default = "default_cmd_play")]
    pub play: String,
    #[serde(default = "default_cmd_end")]
    pub end: String,
    #[serde(default = "default_cmd_reset")]
    pub reset: String,
    #[serde(default = "default_cmd_command")]
    pub command: String,
    #[serde(default = "default_cmd_save")]
    pub save: String,
    #[serde(default = "default_cmd_load")]
    pub load: String,
    #[serde(default = "default_cmd_list_states")]
    pub list_states: String,
    #[serde(default = "default_cmd_next")]
    pub next: String,
    #[serde(default = "default_cmd_pagination_reset")]
    pub pagination_reset: String,
    #[serde(default = "default_cmd_help")]
    pub help: String,
    // Debug commands
    #[serde(default = "default_cmd_debug")]
    pub debug: String,
    #[serde(default = "default_cmd_memory")]
    pub memory: String,
    #[serde(default = "default_cmd_stack")]
    pub stack: String,
}

fn default_port() -> u16 { 6667 }
fn default_use_tls() -> bool { false }
fn default_games_dir() -> String { "games".to_string() }
fn default_saves_dir() -> String { "save_states".to_string() }
fn default_prefix() -> String { "!".to_string() }
fn default_cmd_games() -> String { "games".to_string() }
fn default_cmd_play() -> String { "play".to_string() }
fn default_cmd_end() -> String { "end".to_string() }
fn default_cmd_reset() -> String { "reset".to_string() }
fn default_cmd_command() -> String { "cmd".to_string() }
fn default_cmd_save() -> String { "save".to_string() }
fn default_cmd_load() -> String { "load".to_string() }
fn default_cmd_list_states() -> String { "list_states".to_string() }
fn default_cmd_next() -> String { "next".to_string() }
fn default_cmd_pagination_reset() -> String { "pagination_reset".to_string() }
fn default_cmd_help() -> String { "help".to_string() }
fn default_cmd_debug() -> String { "debug".to_string() }
fn default_cmd_memory() -> String { "memory".to_string() }
fn default_cmd_stack() -> String { "stack".to_string() }

impl Config {
    pub fn load<P: AsRef<Path>>(path: P) -> Result<Self> {
        let content = fs::read_to_string(path)?;
        let config = toml::from_str(&content)?;
        Ok(config)
    }
} 