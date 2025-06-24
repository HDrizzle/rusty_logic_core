use crate::prelude::*;
use eframe::{egui::{self, Color32, Frame, Painter, Pos2, Sense, Shape, Ui}, epaint::PathStroke};
use serde::Deserialize;
use serde_json;
use crate::resource_interface;

/// Style for the UI, loaded from /resources/styles.json
#[derive(Clone, Deserialize)]
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

impl Default for Styles {
	fn default() -> Self {
		Self {
			show_grid: true,
			line_size_grid: 0.1,
			color_wire_floating: [128, 128, 128],
			color_wire_contested: [255, 0, 0],
			color_wire_low: [0, 0, 255],
			color_wire_high: [0, 255, 0],
			color_background: [0, 0, 0],
			color_foreground: [255, 255, 255],
			color_grid: [64, 64, 64]
		}
	}
}

pub struct ComponentDrawInfo<'a> {
	/// Location of center of screen with respect to the grid, it is this way so that it will not have to adjusted when the grid is zoomed in/out
	pub screen_center_wrt_grid: V2,
	/// Pixels per grid increment
	pub grid_size: f32,
	pub painter: &'a Painter,
	/// Includes component's own position, 
	pub offset_grid: IntV2,
	pub styles: &'a Styles
}

impl<'a> ComponentDrawInfo<'a> {
	pub fn new(
		screen_center_wrt_grid: V2,
		grid_size: f32,
		painter: &'a Painter,
		offset_grid: IntV2,
		styles: &'a Styles
	) -> Self {
		Self {
			screen_center_wrt_grid,
			grid_size,
			painter,
			offset_grid,
			styles
		}
	}
	pub fn draw_polyline(&self, points: Vec<V2>, stroke: [u8; 3]) {
		let px_points = points.iter().map(|p| self.grid_to_px(*p)).collect();
		self.painter.add(Shape::line(px_points, PathStroke::new(self.grid_size * self.styles.line_size_grid, u8_3_to_color32(stroke))));
	}
	pub fn grid_to_px(&self, grid: V2) -> egui::Pos2 {
		// TODO
		let nalgebra_v2 = grid * self.grid_size;
		egui::Pos2{x: nalgebra_v2.x, y: nalgebra_v2.y}
	}
	pub fn add_child_grid_pos(&'a self, new_grid_pos: IntV2) -> Self {
		Self {
			screen_center_wrt_grid: self.screen_center_wrt_grid,
			grid_size: self.grid_size,
			painter: self.painter,
			offset_grid: self.offset_grid + new_grid_pos,
			styles: self.styles
		}
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
		// First, detect user unput
		// TODO
		let (mut _response, painter) = ui.allocate_painter(ui.available_size_before_wrap(), Sense::drag());
		let draw_info = ComponentDrawInfo::new(
			self.screen_center_wrt_grid,
			self.grid_size,
			&painter,
			IntV2(0, 0),
			styles
		);
		draw_info.painter.add(Shape::line(vec![Pos2::new(-100.0, -100.0), Pos2::new(-100.0, -100.0)], PathStroke::new(2.0, Color32::from_rgb(0, 255, 0))));
        // graphics, help from https://github.com/emilk/egui/blob/main/crates/egui_demo_lib/src/demo/painting.rs
		// Draw circuit's inputs/outputs
		for (_, pin) in &self.circuit.get_generic().pins.items {
			draw_info.draw_polyline(
				vec![
					V2::new(-0.9, -0.9),
					V2::new(-0.9, 0.9),
					V2::new(0.9, 0.9),
					V2::new(0.9, -0.9)
				].iter().map(|p| p + pin.relative_end_grid.to_v2()).collect(),
				styles.color_foreground
			);
		}
		// Draw circuit
		self.circuit.draw(draw_info.add_child_grid_pos(self.circuit.generic_device.position_grid));
	}
}

enum AppState {
	Home {
		
	},
	Editor {
		circuit_tabs: Vec<LogicCircuitToplevelView>,
        current_tab_index: usize
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
	state: AppState,
	styles: Styles
}

impl App {
	pub fn new() -> Self {
		Self {
			state: AppState::default(),
			styles: Styles::default()
		}
	}
}

impl eframe::App for App {
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		let mut new_state_opt: Option<AppState> = None;
		match &mut self.state {
            AppState::Home{} => {
                egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
					ui.label("Rusty Logic");
					if ui.button("Test circuit").clicked() {
						new_state_opt = Some(AppState::Editor{circuit_tabs: vec![LogicCircuitToplevelView::new(create_simple_circuit())], current_tab_index: 0});
					}
				});
            },
            AppState::Editor{circuit_tabs, current_tab_index} => {
				let circuit_toplevel: &mut LogicCircuitToplevelView = &mut circuit_tabs[*current_tab_index];
                egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
					ui.label(&circuit_toplevel.circuit.generic_device.unique_name);
					Frame::canvas(ui.style()).show(ui, |ui| {
						circuit_toplevel.draw(ui, &self.styles);
					});
				});
            }
        }
		if let Some(new_state) = new_state_opt {
			self.state = new_state;
		}
	}
}