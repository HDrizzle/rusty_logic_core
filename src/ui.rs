use crate::prelude::*;
use eframe::{egui, egui::{Rgba, Ui}, Frame};
use serde::{Deserialize, Serialize};
use serde_json;
use crate::resource_interface;

/// Style for the UI, loaded from /resources/styles.json
#[derive(Deserialize)]
pub struct Styles {
    /// Show a grid in the background
	show_grid: bool,
	/// Fraction of grid size that lines are drawn, 0.1 is probably good
	line_size_grid: f32,
    color_wire_floating: [u8; 3],
    color_wire_contested: [u8; 3],
    color_wire_low: [u8; 3],
    color_wire_high: [u8; 3],
    color_background: [u8; 3],
    color_foreground: [u8; 3],
    color_grid: [u8; 3]
}

impl Styles {
    pub fn load() -> Result<Self, String> {
        let raw_string: String = load_file_with_better_error(resource_interface::STYLES_FILE)?;
        let styles: Self = to_string_err(serde_json::from_str(&raw_string))?;
        Ok(styles)
    }
}

pub struct LogicCircuitToplevelView {
	/// The top-level circuit of this view, its pins are rendered as interactive I/O pins
	circuit: LogicCircuit,
	/// Location of center of screen with respect to the grid, it is this way so that it will not have to adjusted when the grid is zoomed in/out
	screen_center_wrt_grid: V2,
	/// Pixels per grid increment
	grid_size: f32
}

impl LogicCircuitToplevelView {
	pub fn new(circuit: LogicCircuit) -> Self {
		Self {
			circuit,
			screen_center_wrt_grid: V2::zeros(),
			grid_size: 20.0
		}
	}
	pub fn draw(&self, ui: &mut Ui, styles: &Styles) {
		// My favorite part of drawing graphics...
		let grid_to_px = |grid: V2| -> V2 {
			grid// TODO
		};
		// First, detect user unput
		// TODO
        // graphics, help from https://github.com/emilk/egui/blob/main/crates/egui_demo_lib/src/demo/painting.rs
		// Draw circuit's inputs/outputs
		for (_, pin) in &self.circuit.get_generic().pins.items {
			
		}
		// Draw circuit
		// TODO
	}
}

enum AppState {
	Home {
		
	},
	Editor {
		circuit_tabs: Vec<LogicCircuitToplevelView>,
        current_tab: usize
	}
}

impl AppState {
	// TODO
}

impl Default for AppState {
	fn default() -> Self {
		Self::Home{}
	}
}

pub struct App {
	state: AppState
}

impl App {
	pub fn new() -> Self {
		Self {
			state: AppState::default()
		}
	}
}

impl eframe::App for App {
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		match &self.state {
            AppState::Home{} => {
                egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
					ui.label("Rusty Logic");
				});
            },
            AppState::Editor{circuit_tabs, current_tab} => {
                egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
					ui.label("Rusty Logic");
				});
            }
        }
	}
}