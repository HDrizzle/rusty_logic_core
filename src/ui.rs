use crate::{prelude::*, resource_interface, simulator::AncestryStack};
use eframe::{egui::{self, Color32, Frame, Painter, Pos2, Sense, Shape, Stroke, Ui, Vec2}, epaint::{CubicBezierShape, PathStroke}};
use serde::Deserialize;
use serde_json;
use std::f32::consts::TAU;

/// Style for the UI, loaded from /resources/styles.json
#[derive(Clone, Deserialize)]
pub struct Styles {
    /// Show a grid in the background
	pub show_grid: bool,
	/// Fraction of grid size that lines are drawn, 0.1 is probably good
	pub line_size_grid: f32,
    pub color_wire_floating: [u8; 3],
    pub color_wire_contested: [u8; 3],
    pub color_wire_low: [u8; 3],
    pub color_wire_high: [u8; 3],
    pub color_background: [u8; 3],
    pub color_foreground: [u8; 3],
    pub color_grid: [u8; 3]
}

impl Styles {
    pub fn load() -> Result<Self, String> {
        let raw_string: String = load_file_with_better_error(resource_interface::STYLES_FILE)?;
        let styles: Self = to_string_err(serde_json::from_str(&raw_string))?;
        Ok(styles)
    }
	pub fn color_from_logic_state(&self, state: LogicState) -> [u8; 3] {
		match state {
			LogicState::Driven(value) => match value {
				true => self.color_wire_high,
				false => self.color_wire_low
			},
			LogicState::Floating => self.color_wire_floating,
			LogicState::Contested => self.color_wire_contested
		}
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

pub trait GraphicSelectableItem {
	fn draw<'a>(&self, draw: ComponentDrawInfo<'a>);
	fn get_selected(&self) -> bool;
	fn set_selected(&mut self, selected: bool);
	/// Relative to Grid
	fn bounding_box(&self) -> (V2, V2);
	/// Relative to Grid
	/// Things with complicated shapes should override this with something better
	fn is_click_hit(&self, mouse: V2) -> bool {
		let bb: (V2, V2) = self.bounding_box();
		mouse.x >= bb.0.x && mouse.x <= bb.1.x && mouse.y >= bb.0.y && mouse.y <= bb.1.y
	}
	/// Relative to Grid
	fn dragging_to(&mut self, change: V2);
}

pub struct ComponentDrawInfo<'a> {
	/// Location of center of screen with respect to the grid, it is this way so that it will not have to adjusted when the grid is zoomed in/out
	pub screen_center_wrt_grid: V2,
	/// Pixels per grid increment
	pub grid_size: f32,
	pub painter: &'a Painter,
	/// Includes component's own position, 
	pub offset_grid: IntV2,
	pub styles: &'a Styles,
	pub rect_center: V2
}

impl<'a> ComponentDrawInfo<'a> {
	pub fn new(
		screen_center_wrt_grid: V2,
		grid_size: f32,
		painter: &'a Painter,
		offset_grid: IntV2,
		styles: &'a Styles,
		rect_center: V2
	) -> Self {
		Self {
			screen_center_wrt_grid,
			grid_size,
			painter,
			offset_grid,
			styles,
			rect_center
		}
	}
	pub fn draw_polyline(&self, points: Vec<V2>, stroke: [u8; 3]) {
		let px_points = points.iter().map(|p| self.grid_to_px(*p)).collect();
		self.painter.add(Shape::line(px_points, PathStroke::new(self.grid_size * self.styles.line_size_grid, u8_3_to_color32(stroke))));
	}
	pub fn draw_circle(&self, center: V2, radius: f32, stroke: [u8; 3]) {
		self.painter.circle_stroke(self.grid_to_px(center), radius * self.grid_size, Stroke::new(self.grid_size * self.styles.line_size_grid, u8_3_to_color32(stroke)));
	}
	/// With help from https://github.com/emilk/egui/issues/4188
	pub fn draw_arc(&self, center_grid: V2, radius_grid: f32, start_deg: f32, end_deg: f32, stroke: [u8; 3]) {
		let center = self.grid_to_px(center_grid);
		let radius = radius_grid * self.grid_size;
		let start_rad = start_deg * TAU / 360.0;
		let end_rad = end_deg * TAU / 360.0;
		// Center of the circle
		let xc = center.x;
		let yc = center.y;

		// First control point
		let p1 = center + radius * Vec2::new(start_rad.cos(), -start_rad.sin());

		// Last control point
		let p4 = center + radius * Vec2::new(end_rad.cos(), -end_rad.sin());

		let a = p1 - center;
		let b = p4 - center;
		let q1 = a.length_sq();
		let q2 = q1 + a.dot(b);
		let k2 = (4.0 / 3.0) * ((2.0 * q1 * q2).sqrt() - q2) / (a.x * b.y - a.y * b.x);

		let p2 = Pos2::new(xc + a.x - k2 * a.y, yc + a.y + k2 * a.x);
		let p3 = Pos2::new(xc + b.x + k2 * b.y, yc + b.y - k2 * b.x);
		self.painter.add(Shape::CubicBezier(CubicBezierShape{
			points: [p1, p2, p3, p4],
			closed: false,
			fill: Color32::TRANSPARENT,
			stroke: PathStroke::new(self.grid_size * self.styles.line_size_grid, u8_3_to_color32(stroke))
		}));
	}
	pub fn grid_to_px(&self, grid: V2) -> egui::Pos2 {
		// TODO
		let nalgebra_v2 = ((grid + self.offset_grid.to_v2()) * self.grid_size) + self.rect_center;
		egui::Pos2{x: nalgebra_v2.x, y: nalgebra_v2.y}
	}
	pub fn add_child_grid_pos(&'a self, new_grid_pos: IntV2) -> Self {
		Self {
			screen_center_wrt_grid: self.screen_center_wrt_grid,
			grid_size: self.grid_size,
			painter: self.painter,
			offset_grid: self.offset_grid + new_grid_pos,
			styles: self.styles,
			rect_center: self.rect_center
		}
	}
}

pub struct LogicCircuitToplevelView {
	/// The top-level circuit of this view, its pins are rendered as interactive I/O pins
	circuit: LogicCircuit,
	/// Location of center of screen with respect to the grid, it is this way so that it will not have to adjusted when the grid is zoomed in/out
	screen_center_wrt_grid: V2,
	/// Pixels per grid increment
	grid_size: f32,
	logic_loop_error: bool
}

impl LogicCircuitToplevelView {
	pub fn new(circuit: LogicCircuit) -> Self {
		Self {
			circuit,
			screen_center_wrt_grid: V2::zeros(),
			grid_size: 10.0,
			logic_loop_error: false
		}
	}
	pub fn draw(&mut self, ui: &mut Ui, styles: &Styles) {
		let mut recompute_connections = false;
		let mut propagate = true;// TODO: Change to false when rest of logic is implemented
		// First, detect user unput
		// TODO
		// Update
		if recompute_connections || propagate {
			self.logic_loop_error = self.propagate_until_stable(PROPAGATION_LIMIT);
		}
		// Graphics
		let (response, painter) = ui.allocate_painter(ui.available_size_before_wrap(), Sense::drag());
		let draw_info = ComponentDrawInfo::new(
			self.screen_center_wrt_grid,
			self.grid_size,
			&painter,
			IntV2(0, 0),
			styles,
			emath_pos2_to_v2(response.rect.center())
		);
		//draw_info.painter.add(Shape::line(vec![Pos2::new(-100.0, -100.0), Pos2::new(-100.0, -100.0)], PathStroke::new(2.0, Color32::from_rgb(0, 255, 0))));
        // graphics help from https://github.com/emilk/egui/blob/main/crates/egui_demo_lib/src/demo/painting.rs
		// Draw circuit's inputs/outputs
		for (_, pin) in &self.circuit.get_generic().pins.items {
			draw_info.draw_polyline(
				vec![
					V2::new(-0.9, -0.9),
					V2::new(-0.9, 0.9),
					V2::new(0.9, 0.9),
					V2::new(0.9, -0.9),
					V2::new(-0.9, -0.9)
				].iter().map(|p| p + pin.relative_end_grid.to_v2() + pin.direction.to_unit()).collect(),
				styles.color_from_logic_state(pin.state())
			);
			if let Some(ext_source) = &pin.external_source {
				if let LogicConnectionPinExternalSource::Clock = ext_source {
					let clk_scale = 0.7;
					draw_info.draw_polyline(
						vec![
							V2::new(-clk_scale, 0.0),
							V2::new(-clk_scale, clk_scale),
							V2::new(0.0, clk_scale),
							V2::new(0.0, -clk_scale),
							V2::new(clk_scale, -clk_scale),
							V2::new(clk_scale, 0.0)
						].iter().map(|p| p + pin.relative_end_grid.to_v2() + pin.direction.to_unit()).collect(),
						styles.color_foreground
					);
				}
			}
		}
		// Draw circuit
		self.circuit.draw_toplevel(draw_info.add_child_grid_pos(self.circuit.generic_device.position_grid));
	}
	/// Runs `compute_step()` repeatedly on the circuit until there are no changes, there must be a limit because there are circuits (ex. NOT gate connected to itself) where this would otherwise never end
	pub fn propagate_until_stable(&mut self, propagation_limit: usize) -> bool {
		let mut count: usize = 0;
		// TODO: keep track of state
		while count < propagation_limit {
			self.circuit.compute(&AncestryStack::new());
			count += 1;
		}
		return true;
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
						new_state_opt = Some(AppState::Editor{circuit_tabs: vec![LogicCircuitToplevelView::new(create_simple_circuit(true))], current_tab_index: 0});
					}
				});
            },
            AppState::Editor{circuit_tabs, current_tab_index} => {
				let circuit_toplevel: &mut LogicCircuitToplevelView = &mut circuit_tabs[*current_tab_index];
                egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
					// This function by default is only run upon user interaction, so copied this from https://users.rust-lang.org/t/issues-while-writing-a-clock-with-egui/102752
					ui.ctx().request_repaint();
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