use crate::{builtin_components, prelude::*, resource_interface, simulator::{AncestryStack, GraphicSelectableItemRef, SelectionState, Tool}};
use eframe::{egui::{self, containers::Popup, scroll_area::ScrollBarVisibility, text::LayoutJob, Align2, Button, Color32, DragValue, FontFamily, FontId, Frame, Galley, Painter, PopupCloseBehavior, Pos2, Rect, RectAlign, ScrollArea, Sense, Shape, Stroke, StrokeKind, TextEdit, TextFormat, Ui, Vec2, Window}, emath, epaint::{PathStroke, TextShape}};
use egui_commonmark::{CommonMarkCache, CommonMarkViewer};
use nalgebra::ComplexField;
use serde::{Serialize, Deserialize};
use serde_json;
use std::{collections::HashSet, f32::consts::TAU, ops::{AddAssign, RangeInclusive, SubAssign}, sync::Arc};
use mouse_rs;

/// Style for the UI, loaded from /resources/styles.json
#[derive(Clone, Deserialize)]
pub struct Styles {
	/// Show a grid in the background
	pub show_grid: bool,
	pub default_grid_size: f32,
	pub min_grid_size: f32,
	pub max_grid_size: f32,
	pub grid_scale_factor: f32,
	/// Fraction of grid size that lines are drawn, 0.1 is probably good
	pub line_size_grid: f32,
	pub connection_dot_grid_size: f32,
	pub color_wire_floating: [u8; 3],
	pub color_wire_contested: [u8; 3],
	pub color_wire_low: [u8; 3],
	pub color_wire_high: [u8; 3],
	pub color_bus: [u8; 3],
	pub color_background: [u8; 3],
	pub color_foreground: [u8; 3],
	pub color_grid: [u8; 3],
	pub select_rect_color: [u8; 4],
	pub select_rect_edge_color: [u8; 3],
	pub color_wire_in_progress: [u8; 3],
	pub text_size_grid: f32,
	pub text_color: [u8; 3],
	pub wire_start_point_outline_color: [u8; 3],
	pub timing_diagram_resolution_px: usize
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
	/// If any states contested then contested color, else bus color unless all are floating then floating color
	pub fn color_from_logic_states(&self, states: &Vec<LogicState>) -> [u8; 3] {
		if states.len() == 1 {
			return self.color_from_logic_state(states[0]);
		}
		let mut contested = false;
		let mut driven = false;
		for state in states {
			contested |= state.is_contested();
			driven |= state.is_valid()
		}
		if contested {
			self.color_wire_contested
		}
		else {
			if driven {
				self.color_bus
			}
			else {
				self.color_wire_floating
			}
		}
	}
}

impl Default for Styles {
	fn default() -> Self {
		Self {
			show_grid: true,
			default_grid_size: 15.0,
			min_grid_size: 1.0,
			max_grid_size: 1000.0,
			grid_scale_factor: 1.012,
			line_size_grid: 0.15,
			connection_dot_grid_size: 0.3,
			color_wire_floating: [128, 128, 128],
			color_wire_contested: [255, 0, 0],
			color_wire_low: [0, 0, 255],
			color_wire_high: [0, 255, 0],
			color_bus: [67, 240, 249],
			color_background: [0, 0, 0],
			color_foreground: [255, 255, 255],
			color_grid: [64, 64, 64],
			select_rect_color: [7, 252, 244, 128],
			select_rect_edge_color: [252, 7, 7],
			color_wire_in_progress: [2, 156, 99],
			text_size_grid: 0.9,
			text_color: [243, 118, 252],
			wire_start_point_outline_color: [93, 252, 167],
			timing_diagram_resolution_px: 20
		}
	}
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct UIData {
	pub selected: bool,
	pub position: IntV2,
	/// Relative to parent, NOT global
	pub direction: FourWayDir,
	pub position_before_dragging: IntV2,
	pub dragging_offset: V2,
	pub local_bb: (V2, V2)
}

impl UIData {
	pub fn new(position: IntV2, direction: FourWayDir, local_bb: (V2, V2)) -> Self {
		Self {
			position,
			direction,
			local_bb,
			..Default::default()
		}
	}
	pub fn pos_to_parent_coords(&self, position: IntV2) -> IntV2 {
		self.direction.rotate_intv2(position) + self.position
	}
	pub fn parent_pos_to_local_coords(&self, position: IntV2) -> IntV2 {
		self.direction.rotate_intv2_reverse(position - self.position)
	}
	pub fn pos_to_parent_coords_float(&self, position: V2) -> V2 {
		self.direction.rotate_v2(position) + self.position.to_v2()
	}
	pub fn parent_pos_to_local_coords_float(&self, position: V2) -> V2 {
		self.direction.rotate_v2_reverse(position - self.position.to_v2())
	}
}

pub trait GraphicSelectableItem {
	/// The implementation of this is responsible for adding it's own position to the offset
	fn draw<'a>(&self, draw: &ComponentDrawInfo<'a>);
	fn get_ui_data(&self) -> &UIData;
	fn get_ui_data_mut(&mut self) -> &mut UIData;
	/// Used for the net highlight feature
	#[allow(unused)]
	fn is_connected_to_net(&self, net_id: u64) -> bool {false}
	fn get_properties(&self) -> Vec<SelectProperty>;
	fn set_property(&mut self, property: SelectProperty);
	fn copy(&self) -> CopiedGraphicItem;
	/// Meant for external connections so that clicks can do something special instead of just selecting them
	/// Returns: Whether the click was "used", if so then it won't be selected normally but can still be command-clicked and included in a dragged rectangle
	#[allow(unused)]
	fn accept_click(&mut self, local_pos: V2) -> bool {
		false
	}
	fn get_selected(&self) -> bool {
		self.get_ui_data().selected
	}
	fn set_selected(&mut self, selected: bool) {
		self.get_ui_data_mut().selected = selected;
	}
	/// Relative to Grid and self, return must be wrt global grid, hence why grid offset must be provided
	/// grid_offset will only correct for positions of nested sub-circuits, not the position of the object that it itself "knows about"
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let ui_data = self.get_ui_data();
		let local_bb = ui_data.local_bb;
		merge_points_to_bb(vec![grid_offset + ui_data.pos_to_parent_coords_float(local_bb.0), grid_offset + ui_data.pos_to_parent_coords_float(local_bb.1)])
	}
	/// Mouse is relative to Grid
	/// Things with complicated shapes should override this with something better
	fn is_click_hit(&self, mouse: V2, grid_offset: V2) -> bool {
		let bb: (V2, V2) = self.bounding_box(grid_offset);
		mouse.x >= bb.0.x && mouse.x <= bb.1.x && mouse.y >= bb.0.y && mouse.y <= bb.1.y
	}
	fn start_dragging(&mut self, current_mouse_pos: V2) {
		let current_pos = self.get_ui_data().position;
		self.get_ui_data_mut().dragging_offset = current_pos.to_v2() - current_mouse_pos;
	}
	fn dragging_to(&mut self, current_mouse_pos: V2) {
		let dragging_offset = self.get_ui_data().dragging_offset;
		self.get_ui_data_mut().position = round_v2_to_intv2(current_mouse_pos + dragging_offset);
	}
	fn stop_dragging(&mut self, final_mouse_pos: V2) {
		self.dragging_to(final_mouse_pos);
	}
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CopiedGraphicItem {
	Component(EnumAllLogicDevices),
	Wire((IntV2, FourWayDir, u32)),
	/// (Position, Direction, Name, Show name, Bit width)
	ExternalConnection(IntV2, FourWayDir, String, bool, u16),
	Splitter(SplitterSave),
	GraphicLabel(GraphicLabelSave),
	Probe(ProbeSave)
}

/// This is what will be serialized as JSON and put onto the clipboard
#[derive(Debug, Serialize, Deserialize)]
pub struct CopiedItemSet {
	pub items: Vec<CopiedGraphicItem>,
	/// So that everything will be shown at the same displacement wrt the mouse when paste is hit
	pub bb_center: IntV2
}

impl CopiedItemSet {
	pub fn new(
		items: Vec<CopiedGraphicItem>,
		bb_center: IntV2
	) -> Self {
		Self {
			items,
			bb_center
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum SelectProperty {
	BitWidth(u16),
	PositionX(i32),
	PositionY(i32),
	GlobalConnectionState(Vec<Option<bool>>),
	Direction(FourWayDir),
	DisplayCircuitAsBlock(bool),
	ClockEnabled(bool),
	ClockFreq(f32),
	ClockState(bool),
	Name(String),
	FixedSourceState(bool),
	/// Address size, Max
	AddressWidth(u8, u8),
	MemoryProperties(builtin_components::MemoryPropertiesUI),
	EncoderOrDecoder(bool),
	/// Whether to reload, optional error message
	ReloadCircuit(bool, Option<String>),
	SplitterSplits(Vec<u16>),
	BusLayout(String, bool, bool),
	/// Namee of pin (ex: "Output Enable"), whether it is enabled
	HasPin(String, bool)
}

impl SelectProperty {
	pub fn ui_name(&self) -> String {
		match self {
			Self::BitWidth(_) => "Bit Width".to_owned(),
			Self::PositionX(_) => "X".to_owned(),
			Self::PositionY(_) => "Y".to_owned(),
			Self::GlobalConnectionState(_) => "I/O State".to_owned(),
			Self::Direction(_) => "Direction".to_owned(),
			Self::DisplayCircuitAsBlock(_) => "Display circuit as block".to_owned(),
			Self::ClockEnabled(_) => "Clock Enable".to_owned(),
			Self::ClockFreq(_) => "Clock Frequency".to_owned(),
			Self::ClockState(_) => "Clock State".to_owned(),
			Self::Name(_) => "Name".to_owned(),
			Self::FixedSourceState(_) => "State".to_owned(),
			Self::AddressWidth(_, _) => "Address Width".to_owned(),
			Self::MemoryProperties(_) => "Memory Properties".to_owned(),
			Self::EncoderOrDecoder(_) => "Encoder/Decoder".to_owned(),
			Self::ReloadCircuit(_, _) => "Reload circuit".to_owned(),
			Self::SplitterSplits(_) => "Splits".to_owned(),
			Self::BusLayout(name, _, _) => format!("{} bus config", name),
			Self::HasPin(name, _) => format!("Has {} pin", name)
		}
	}
	/// Add this property to a list on the UI
	/// Returns: Whether to update the property and/or recalculate circuit connections
	pub fn add_to_ui(&mut self, ui: &mut Ui) -> bool {
		match self {
			Self::BitWidth(n) => {
				ui.add(DragValue::new(n).range(RangeInclusive::new(1, 256)).clamp_existing_to_range(true)).changed()
			},
			Self::PositionX(x) => {
				ui_drag_value_with_arrows(ui, x, None)
			},
			Self::PositionY(y) => {
				ui_drag_value_with_arrows(ui, y, None)
			},
			Self::GlobalConnectionState(state_opts) => {
				let mut return_update_all = false;
				let mut return_update_single = false;
				let mut all_drive: Option<bool> = state_opts[0];
				ui.vertical(|ui| {
					Popup::menu(&ui.button("I/O State")).align(RectAlign::RIGHT_START).show(|ui| {
						if ui.button("All 0").clicked() {
							all_drive = Some(false);
							return_update_all = true;
						}
						if ui.button("All 1").clicked() {
							all_drive = Some(true);
							return_update_all = true;
						}
						if ui.button("Floating").clicked() {
							all_drive = None;
							return_update_all = true;
						}
					});
					for opt in state_opts {
						if return_update_all {
							*opt = all_drive;
						}
						if ui.add_enabled(opt.is_some(), Button::new(match opt.unwrap_or(false) {true => "1", false => "0"})).clicked() {
							if let Some(state) = opt {
								*state = !*state;
								return_update_single = true;
							}
						}
					}
				});
				return_update_all || return_update_single
			},
			Self::Direction(dir) => {
				if ui.button("↶").clicked() {
					*dir = dir.turn_ccw();
					return true;
				}
				if ui.button("↷").clicked() {
					*dir = dir.turn_cw();
					return true;
				}
				return false;
			},
			Self::DisplayCircuitAsBlock(block) => {
				ui.checkbox(block, "").changed()
			},
			Self::ClockEnabled(enable) => {
				ui.checkbox(enable, match enable {
					true => "Enabled",
					false => "Disabled"
				}).changed()
			},
			Self::ClockFreq(freq) => {
				ui.add(DragValue::new(freq).range(RangeInclusive::new(0.01, 1000000.0)).clamp_existing_to_range(false)).changed()
			},
			Self::ClockState(state) => {
				if ui.button(match state {
					true => "1",
					false => "0"
				}).clicked() {
					*state = !(*state);
					true
				}
				else {
					false
				}
			}
			Self::Name(name) => {
				ui.text_edit_singleline(name).changed()
			},
			Self::FixedSourceState(state) => {
				if ui.button(match state {
					true => "1",
					false => "0"
				}).clicked() {
					*state = !(*state);
					true
				}
				else {
					false
				}
			},
			Self::AddressWidth(n, max) => {
				ui_drag_value_with_arrows(ui, n, Some((1, *max)))
			}
			Self::MemoryProperties(props) => {
				props.show_ui(ui)
			}
			Self::EncoderOrDecoder(encoder) => {
				if ui.button(match encoder {
					true => "Encoder",
					false => "Decoder"
				}).clicked() {
					*encoder = !*encoder;
					true
				}
				else {
					false
				}
			},
			Self::ReloadCircuit(reload, err_opt) => {
				*reload = ui.button("Reload").clicked();
				if let Some(err) = err_opt {
					ui.colored_label(u8_3_to_color32([255, 0, 0]), err);
				}
				*reload
			}
			Self::SplitterSplits(splits) => {
				let mut changed = false;
				ui.vertical(|ui| {
					let mut total_bw = 0;
					for s in &mut *splits {
						ui.horizontal(|ui| {
							changed |= ui_drag_value_with_arrows(ui, s, Some((1, 256)));
						});
						total_bw += *s;
					}
					ui.horizontal(|ui| {
						if ui.button("-").clicked() {
							splits.pop();
							changed = true;
						}
						if ui.button("+").clicked() {
							splits.push(1);
							changed = true;
						}
					});
					ui.label(format!("Total BW: {}", total_bw));
					if total_bw > 256 {
						ui.colored_label(u8_3_to_color32([255, 0, 0]), "Bit width cannot exceed 256");
					}
				});
				changed
			},
			Self::BusLayout(_, single_pin, forward) => {
				let mut changed = ui.checkbox(single_pin, "Single graphic pin").changed();
				if !*single_pin {
					if ui.button("Reverse").clicked() {
						*forward = !*forward;
						changed = true;
					}
				}
				changed
			}
			Self::HasPin(_, state) => {
				ui.checkbox(state, "").changed()
			}
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
	pub direction: FourWayDir,
	pub styles: &'a Styles,
	pub rect_center: V2,
	pub rect_size_px: V2,
	pub mouse_pos: Option<Pos2>
}

impl<'a> ComponentDrawInfo<'a> {
	pub fn new(
		screen_center_wrt_grid: V2,
		grid_size: f32,
		painter: &'a Painter,
		offset_grid: IntV2,
		direction: FourWayDir,
		styles: &'a Styles,
		rect_center: V2,
		rect_size_px: V2,
		mouse_pos: Option<Pos2>
	) -> Self {
		Self {
			screen_center_wrt_grid,
			grid_size,
			painter,
			offset_grid,
			direction,
			styles,
			rect_center,
			rect_size_px,
			mouse_pos
		}
	}
	pub fn draw_polyline(&self, points: Vec<V2>, stroke: [u8; 3]) {
		let px_points = points.iter().map(|p| self.grid_to_px(*p)).collect();
		self.painter.add(Shape::line(px_points, PathStroke::new(self.grid_size * self.styles.line_size_grid, u8_3_to_color32(stroke))));
	}
	pub fn draw_rect(&self, start_grid: V2, end_grid: V2, inside_stroke: [u8; 4], border_stroke: [u8; 3]) {
		let rectified_bb: (V2, V2) = merge_points_to_bb(vec![start_grid, end_grid]);
		let (start, end) = (self.grid_to_px(rectified_bb.0), self.grid_to_px(rectified_bb.1));
		let px_rectified_bb = Rect{min: Pos2::new(start.x, end.y), max: Pos2::new(end.x, start.y)};// After Y is flipped, Y needs to be swapped so that the bb is correct in pixel coordinates, Gemini helped find this problem
		self.painter.rect_filled(px_rectified_bb, 0, u8_4_to_color32(inside_stroke));
		self.painter.rect_stroke(px_rectified_bb, 0, Stroke::new(1.0, u8_3_to_color32(border_stroke)), StrokeKind::Outside);
	}
	pub fn draw_circle(&self, center: V2, radius: f32, stroke: [u8; 3]) {
		self.painter.circle_stroke(self.grid_to_px(center), radius * self.grid_size, Stroke::new(self.grid_size * self.styles.line_size_grid, u8_3_to_color32(stroke)));
	}
	pub fn draw_circle_filled(&self, center: V2, radius: f32, stroke: [u8; 3]) {
		self.painter.circle_filled(self.grid_to_px(center), radius * self.grid_size, u8_3_to_color32(stroke));
	}
	/// Egui doen't have an arc feature so I will use a polyline :(
	/// The end angle must be a larger number then the start angle
	pub fn draw_arc(&self, center_grid: V2, radius_grid: f32, start_deg: f32, end_deg: f32, stroke: [u8; 3]) {
		let mut polyline = Vec::<V2>::new();
		let seg_size: f32 = 5.0;
		let n_segs = ((end_deg - start_deg) / seg_size) as usize;
		for i in 0..n_segs+1 {
			let angle_rad = ((i as f32 * seg_size) + start_deg) * TAU / 360.0;
			polyline.push(V2::new(angle_rad.cos() * radius_grid, angle_rad.sin() * radius_grid) + center_grid);
		}
		self.draw_polyline(polyline, stroke);
	}
	pub fn text(&self, text: String, pos: V2, align: Align2, color: [u8; 3], size_grid: f32, vertical: bool) {
		let color32 = u8_3_to_color32(color);
		let pos_px = self.grid_to_px(pos);
		let font_id = FontId::new(self.grid_size * size_grid, FontFamily::Monospace);
		if vertical {
			let galley = self.painter.fonts::<Arc<Galley>>(|fonts| {
				let mut job: LayoutJob = LayoutJob::default();
				job.append(&text, 0.0, TextFormat::simple(font_id, color32));
				fonts.layout_job(job)
			});
			let y_offet = if align == Align2::CENTER_BOTTOM {
				galley.size().x
			}
			else {
				0.0
			};
			let mut shape = TextShape::new(pos_px + Vec2::new(galley.size().y / 2.0, -y_offet), galley, color32);
			shape.angle = std::f32::consts::FRAC_PI_2;
			self.painter.add(shape);
		}
		else {
			self.painter.text(pos_px, align, text, font_id, color32);
		}
	}
	/// gets text size in grid without actually drawing it
	pub fn text_size(&self, text: String, size_grid: f32) -> V2 {
		let font_id = FontId::new(self.grid_size * size_grid, FontFamily::Monospace);
		let galley = self.painter.fonts::<Arc<Galley>>(|fonts| {
			let mut job: LayoutJob = LayoutJob::default();
			job.append(&text, 0.0, TextFormat::simple(font_id, Color32::default()));
			fonts.layout_job(job)
		});
		emath_vec2_to_v2(galley.size()) / self.grid_size
	}
	pub fn grid_to_px(&self, grid: V2) -> egui::Pos2 {
		let nalgebra_v2 = ((self.direction.rotate_v2(grid) + self.offset_grid.to_v2() - self.screen_center_wrt_grid) * self.grid_size) + self.rect_center;
		if cfg!(feature = "reverse_y") {
			egui::Pos2{x: nalgebra_v2.x, y: self.rect_size_px.y - nalgebra_v2.y}
		} else {
			egui::Pos2{x: nalgebra_v2.x, y: nalgebra_v2.y}
		}
	}
	pub fn mouse_pos2_to_grid(&self, mouse_pos_y_backwards: Pos2) -> V2 {
		Self::mouse_pos2_to_grid_unattached(mouse_pos_y_backwards, self.direction, self.rect_center, self.offset_grid, self.screen_center_wrt_grid, self.rect_size_px, self.grid_size)
	}
	pub fn mouse_pos2_to_grid_unattached(mouse_pos_y_backwards: Pos2, direction: FourWayDir, rect_center: V2, offset_grid: IntV2, screen_center_wrt_grid: V2, rect_size_px: V2, grid_size: f32) -> V2 {
		#[cfg(feature = "reverse_y")]
		let mouse_pos = V2::new(mouse_pos_y_backwards.x, rect_size_px.y - mouse_pos_y_backwards.y);
		#[cfg(not(feature = "reverse_y"))]
		let mouse_pos = emath_pos2_to_v2(mouse_pos_y_backwards);
		direction.rotate_v2_reverse(((mouse_pos - rect_center) / grid_size) - offset_grid.to_v2()) + screen_center_wrt_grid
	}
	/// `offset_unrotated` is wrt parent coordinates, dir_ is the direction of the local coordinates of whatever this new drawer is being created for
	pub fn add_grid_pos_and_direction(&'a self, offset_unrotated: IntV2, dir_: FourWayDir) -> Self {
		let offset = self.direction.rotate_intv2(offset_unrotated);
		Self {
			screen_center_wrt_grid: self.screen_center_wrt_grid,
			grid_size: self.grid_size,
			painter: self.painter,
			offset_grid: self.offset_grid + offset,
			direction: self.direction.rotate_intv2(dir_.to_unit_int()).is_along_axis().unwrap(),
			styles: self.styles,
			rect_center: self.rect_center,
			rect_size_px: self.rect_size_px,
			mouse_pos: self.mouse_pos
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
	logic_loop_error: bool,
	showing_component_popup: bool,
	showing_block_edit_popup: bool,
	component_search_text: String,
	all_logic_devices_search: Vec<EnumAllLogicDevices>,
	saved: bool,
	new_sub_cycles_entry: String,
	recompute_conns_next_frame: bool,
	/// 1 Less than actual number of times the compute function was called because the last call doesn't change anything and ends the loop
	frame_compute_cycles: usize,
	showing_flatten_opoup: bool,
	flatten_error: Option<String>
}

impl LogicCircuitToplevelView {
	pub fn new(circuit: LogicCircuit, saved: bool, styles: &Styles) -> Self {
		Self {
			circuit,
			screen_center_wrt_grid: V2::zeros(),
			grid_size: styles.default_grid_size,
			logic_loop_error: false,
			showing_component_popup: false,
			showing_block_edit_popup: false,
			component_search_text: String::new(),
			all_logic_devices_search: Vec::new(),
			saved,
			new_sub_cycles_entry: String::new(),
			recompute_conns_next_frame: false,
			frame_compute_cycles: 0,
			showing_flatten_opoup: false,
			flatten_error: None
		}
	}
	/// Returns: (Optional position to set the mouse to, Optional new circuit tab to open)
	pub fn draw(&mut self, ui: &mut Ui, styles: &mut Styles, screen_top_left: Pos2) -> (Option<Pos2>, Option<String>) {
		let mut return_new_mouse_pos = Option::<Pos2>::None;
		let mut return_new_circuit_tab = Option::<String>::None;
		let canvas_size: Vec2 = ui.available_size_before_wrap();
		let anything_in_focus: bool = ui.memory(|memory| memory.focused().is_some());
		let inner_response = Frame::canvas(ui.style()).show::<(Vec2, V2)>(ui, |ui| {
			let propagate = true;// TODO: Change to false when rest of logic is implemented
			// First, detect user unput
			let input_state = ui.ctx().input(|i| i.clone());
			let (response, painter) = ui.allocate_painter(canvas_size, Sense::all());
			let rect_center: V2 = emath_pos2_to_v2(response.rect.center());
			let draw_info = ComponentDrawInfo::new(
				self.screen_center_wrt_grid,
				self.grid_size,
				&painter,
				IntV2(0, 0),
				FourWayDir::default(),
				styles,
				rect_center,
				emath_vec2_to_v2(canvas_size),
				response.hover_pos()
			);
			// Scrolling
			let scroll = input_state.raw_scroll_delta.y;
			if scroll != 0.0 {
				// Set mouse position to center of screen and move grid offset along with it, inspired by KiCad
				if let Some(og_mouse_pos) = response.hover_pos() {
					// Make sure its rounded
					let shift_grid: IntV2 = round_v2_to_intv2(emath_vec2_to_v2(response.rect.center() - og_mouse_pos) / self.grid_size);
					return_new_mouse_pos = Some(response.rect.center() + screen_top_left.to_vec2());
					self.screen_center_wrt_grid -= V2::new(shift_grid.0 as f32, -shift_grid.1 as f32);
				};
				// Scroll
				let scale = styles.grid_scale_factor.powf(scroll);
				let new_grid_unclamped = self.grid_size.scale(scale);
				self.grid_size = if new_grid_unclamped < styles.max_grid_size {
					if new_grid_unclamped > styles.min_grid_size {
						new_grid_unclamped
					}
					else {
						styles.min_grid_size
					}
				}
				else {
					styles.max_grid_size
				};
			}
			//let recompute_connections: bool = self.circuit.toplevel_ui_interact(response, ui.ctx(), &draw_info, input_state);
			if !anything_in_focus {
				self.recompute_conns_next_frame |= self.circuit.toplevel_ui_interact(
					response,
					ui.ctx(),
					input_state,
					self.grid_size,
					|pos_px: Pos2| -> V2 {
						ComponentDrawInfo::mouse_pos2_to_grid_unattached(pos_px, FourWayDir::default(), rect_center, IntV2(0, 0), self.screen_center_wrt_grid, emath_vec2_to_v2(canvas_size), self.grid_size)
					}
				);
			}
			// Reconnect wires and thingies if anything was changed
			if self.recompute_conns_next_frame {
				self.saved = false;
				self.circuit.check_wire_geometry_and_connections();
			}
			// Update
			if self.recompute_conns_next_frame || propagate {
				self.logic_loop_error = self.propagate_until_stable(CIRCUIT_MAX_COMPUTE_CYCLES);
				self.recompute_conns_next_frame = false;
			}
			// graphics help from https://github.com/emilk/egui/blob/main/crates/egui_demo_lib/src/demo/painting.rs
			// Draw circuit
			self.circuit.draw(&draw_info);
			// Right side toolbar
			self.circuit.tool.borrow().tool_select_ui(&draw_info);
			(canvas_size, rect_center)
		});
		// Top: general controls
		Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::LEFT_TOP, child: Align2::LEFT_TOP}).id("top-left controls".into()).show(|ui| {
			if ui.button("Save").clicked() {
				self.circuit.save_circuit().unwrap();
				self.saved = true;
			}
			if self.circuit.tool.borrow().tool_select_allowed() {
				if ui.button("+ Component / Subcircuit").clicked() {
					// Update component search list
					self.all_logic_devices_search = builtin_components::list_all_basic_components();
					for file_path in resource_interface::list_all_circuit_files().unwrap() {
						self.all_logic_devices_search.push(EnumAllLogicDevices::SubCircuit(file_path, false, IntV2(0, 0), FourWayDir::default(), String::new()));
					}
					self.showing_component_popup = true;
				}
				if ui.button("+ I/O Pin").clicked() {
					let graphic_pin_id = self.circuit.insert_graphic_pin(IntV2(0, 0), FourWayDir::default(), String::new(), true, 1);
					self.circuit.set_graphic_item_following_mouse(GraphicSelectableItemRef::Pin(graphic_pin_id));
					self.circuit.update_pin_block_positions();
				}
				if ui.button("+ Splitter").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_splitter(Splitter::new().save()));
				}
				if ui.button("+ Label").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_label(GraphicLabel::new().save()));
				}
				if ui.button("+ Probe").clicked() {
					self.circuit.set_graphic_item_following_mouse(self.circuit.insert_probe(Probe::default().save()));
				}
			}
			// Compute cycles text
			match self.frame_compute_cycles == CIRCUIT_MAX_COMPUTE_CYCLES {
				true => {ui.colored_label(u8_3_to_color32([255, 0, 0]), format!("Compute cycles: {}", self.frame_compute_cycles));}
				false => {ui.label(format!("Compute cycles: {}", self.frame_compute_cycles));}
			}
			// Circuit settings (clock speed, etc)
			ui.collapsing("Circuit Settings", |ui| {
				ui.horizontal(|ui| {
					ui.label("Name");
					ui.add(TextEdit::singleline(&mut self.circuit.type_name).desired_width(100.0));
				});
				ui.horizontal(|ui| {// TODO: fix UI to make it optional
					ui.label("Fixed sub cycles");
					ui.add(TextEdit::singleline(&mut self.new_sub_cycles_entry).desired_width(50.0));
					let button = Button::new("Update");
					match self.new_sub_cycles_entry.parse::<usize>() {
						Ok(sub_cycles) => {
							if ui.add(button).clicked() {
								self.circuit.fixed_sub_cycles_opt = Some(sub_cycles);
							}
						},
						Err(_) => {
							ui.add_enabled(false, button);
						}
					}
				});
				// Clock
				ui.label("Clock");
				let mut clock = self.circuit.clock.borrow_mut();
				ui.horizontal(|ui| {
					ui.checkbox(&mut clock.enabled, "Enable");
					ui.label("Freq:");
					ui.add(DragValue::new(&mut clock.freq).range(RangeInclusive::new(0.01, 1000000.0)).clamp_existing_to_range(false));
					ui.label("State:");
					if ui.button(match &mut clock.state {
						true => "1",
						false => "0"
					}).clicked() {
						clock.state = !clock.state;
					}
				});
				if ui.button("Edit block layout...").clicked() {
					self.showing_block_edit_popup = true;
				}
				if ui.button("Flatten circuit...").clicked() {
					self.showing_flatten_opoup = true;
					self.flatten_error = None;
				}
			});
			// Active selection features
			if let Tool::Select{selected_graphics, selected_graphics_state: _} = &*self.circuit.tool.borrow() {
				if !selected_graphics.is_empty() {
					// Properties list
					ui.separator();
					ui.label("Properties");
					// All different variants of `SelectProperty`
					// Vec<(Property, whether they are all the same, Set of selected items to update when property is edited)>
					let mut unique_properties = Vec::<(SelectProperty, bool, HashSet<GraphicSelectableItemRef>)>::new();
					for graphic_handle in selected_graphics.iter() {
						if let Some(new_properties) = self.circuit.run_function_on_graphic_item(graphic_handle.clone(), |item_box| item_box.get_properties()) {
							for property in new_properties {
								// Check if property enum variant is already included in `unique_properties`
								let mut variant_included = false;// Optional index of `unique_properties`
								for (prop_test, are_all_same, graphic_item_set) in unique_properties.iter_mut() {
									if prop_test.ui_name() == property.ui_name() {
										variant_included = true;
										if *prop_test != property {
											*are_all_same = false;
										}
										graphic_item_set.insert(graphic_handle.clone());
									}
								}
								if !variant_included {
									unique_properties.push((
										property,
										true,
										HashSet::from_iter(vec![graphic_handle.clone()].into_iter())
									));
								}
							}
						}
					}
					// Display them
					for (prop, _, graphic_item_set) in unique_properties.iter_mut() {
						ui.horizontal(|ui| {
							ui.label(format!("{}:", prop.ui_name()));
							if prop.add_to_ui(ui) {
								self.saved = false;
								self.recompute_conns_next_frame = true;
								for graphic_handle in graphic_item_set.iter() {
									self.circuit.run_function_on_graphic_item_mut(graphic_handle.clone(), |item_box| {item_box.set_property(prop.clone());});
								}
							}
						});
					}
				}
			}
		});
		if self.showing_component_popup {
			Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				ui.horizontal(|ui| {
					ui.label("Add Component / Sub-circuit");
					if ui.button("Cancel").clicked() {
						self.showing_component_popup = false;
					}
				});
				ui.text_edit_singleline(&mut self.component_search_text).request_focus();
				ScrollArea::vertical().show(ui, |ui| {
					for device_save in &self.all_logic_devices_search {
						if device_save.type_name().to_ascii_lowercase().contains(&self.component_search_text.to_ascii_lowercase()) {
							if ui.button(device_save.type_name()).clicked() {
								self.showing_component_popup = false;
								let handle = self.circuit.insert_component(device_save);
								*self.circuit.tool.borrow_mut() = Tool::Select{
									selected_graphics: HashSet::from_iter(vec![handle].into_iter()),
									selected_graphics_state: SelectionState::FollowingMouse(V2::zeros())
								};
							}
						}
					}
				});
			});
		}
		else if self.showing_block_edit_popup {
			Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				self.edit_block_layout(ui, styles, inner_response.inner.0);
			});
		}
		else if self.showing_flatten_opoup {
			Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				ui.label("Recursively flatten sub circuits");
				ui.separator();
				ui.label(format!("This feature will create a copy of this circuit but all the wires and components of sub-circuits extracted into the \"top layer\". Sub-circuits with fixed compute cycles will not be extracted but flattened versions of them will still be created. The intended use-case of this is to make a circuit faster to simulate. The flattened copy file size will be approximately the sum of all sub-circuits plus the toplevel wires and components. Running this function might create large files in the {} directory.", resource_interface::CIRCUITS_DIR));
				ui.horizontal(|ui| {
					if ui.button("Flatten Circuit").clicked() {
						match self.circuit.flatten(true) {
							Ok(device_save) => if let EnumAllLogicDevices::SubCircuit(save_path, _, _, _, _) = device_save {
								return_new_circuit_tab = Some(save_path);
								self.showing_flatten_opoup = false;
							}
							else {
								panic!("Circuit flatten() should return the EnumAllLogicDevices::SubCircuit variant of EnumAllLogicDevices");
							},
							Err(e) => {
								self.flatten_error = Some(e)
							}
						}
					}
					if ui.button("Cancel").clicked() {
						self.showing_flatten_opoup = false;
					}
				});
				if let Some(flatten_error) = &self.flatten_error {
					ui.colored_label(u8_3_to_color32([255, 0, 0]), flatten_error);
				}
			});
		}
		else {
			/*Popup::from_response(&inner_response.response).align(RectAlign{parent: Align2::CENTER_CENTER, child: Align2::CENTER_CENTER}).show(|ui| {
				ScrollArea::both().show(ui, self.)
			});*/
			Window::new("Timing Diagram").anchor(Align2::RIGHT_TOP, Vec2::new(0.0, inner_response.response.rect.top())).collapsible(true).resizable(true).show(ui.ctx(), |ui| self.circuit.show_timing_diagram_ui(ui, styles));
		}
		(return_new_mouse_pos, return_new_circuit_tab)
	}
	/// Runs `compute_step()` repeatedly on the circuit until there are no changes, there must be a limit because there are circuits (ex. NOT gate connected to itself) where this would otherwise never end
	pub fn propagate_until_stable(&mut self, propagation_limit: usize) -> bool {
		let mut count: usize = 0;
		while count < propagation_limit {
			if !self.circuit.compute_immutable(&AncestryStack::new(), 0, count == 0) {
				if count > 0 {
					self.circuit.update_timing_diagram();
				}
				self.frame_compute_cycles = count;
				return false;
			}
			count += 1;
		}
		self.frame_compute_cycles = count;
		return true;
	}
	fn edit_block_layout(&mut self, ui: &mut Ui, styles: &Styles, maine_frame_size: Vec2) {
		let inner_response = Frame::canvas(ui.style()).show(ui, |ui| {
			//let canvas_size = ui.available_size_before_wrap();
			let (response, painter) = ui.allocate_painter(maine_frame_size, Sense::all());
			let draw_info = ComponentDrawInfo::new(
				self.screen_center_wrt_grid,
				self.grid_size,
				&painter,
				IntV2(0, 0),
				FourWayDir::default(),
				styles,
				emath_pos2_to_v2(response.rect.center()),
				emath_vec2_to_v2(maine_frame_size),
				None
			);
			self.circuit.draw_as_block(&draw_info, true);
		});
		//inner_response.response.ctx.input_mut(|input| input.events.clear());
		Popup::from_response(&inner_response.response).open(true).align(RectAlign{parent: Align2::LEFT_TOP, child: Align2::LEFT_TOP}).id("block edit controls".into()).show(|ui| {
			ScrollArea::vertical().scroll_bar_visibility(ScrollBarVisibility::AlwaysHidden).show(ui, |ui| {
				let mut pins = self.circuit.generic_device.graphic_pins.borrow_mut();
				for (pin_id, pin) in pins.iter_mut() {
					let pin_config: &mut (IntV2, FourWayDir, bool) = self.circuit.block_pin_positions.get_mut(pin_id).unwrap();
					ui.horizontal(|ui| {
						ui.add(TextEdit::singleline(&mut pin.name).desired_width(50.0));
						ui.checkbox(&mut pin.show_name, "");
						ui.separator();
						ui.label("X:");
						if ui.button("<").clicked() {
							pin_config.0.0 -= 1;
						}
						ui.add(DragValue::new(&mut pin_config.0.0));
						if ui.button(">").clicked() {
							pin_config.0.0 += 1;
						}
						ui.separator();
						ui.label("Y:");
						if ui.button("<").clicked() {
							pin_config.0.1 -= 1;
						}
						ui.add(DragValue::new(&mut pin_config.0.1));
						if ui.button(">").clicked() {
							pin_config.0.1 += 1;
						}
						ui.separator();
						ui.label("Dir:");
						if ui.button("↶").clicked() {
							pin_config.1 = pin_config.1.turn_ccw();
						}
						if ui.button("↷").clicked() {
							pin_config.1 = pin_config.1.turn_cw();
						}
					});
				}
				ui.horizontal(|ui| {
					let bb_float: (V2, V2) = self.circuit.generic_device.ui_data.local_bb;
					let mut bb: (IntV2, IntV2) = (round_v2_to_intv2(bb_float.0), round_v2_to_intv2(bb_float.1));
					ui.label("Box dimensions:");
					ui.separator();
					ui.label("X min:");
					ui.add(DragValue::new(&mut bb.0.0));
					ui.label("X max:");
					ui.add(DragValue::new(&mut bb.1.0));
					ui.label("Y min:");
					ui.add(DragValue::new(&mut bb.0.1));
					ui.label("Y max:");
					ui.add(DragValue::new(&mut bb.1.1));
					self.circuit.generic_device.ui_data.local_bb = (bb.0.to_v2(), bb.1.to_v2());
				});
			});
			if ui.button("Done").clicked() {
				self.showing_block_edit_popup = false;
			}
		});
	}
}


pub struct App {
	styles: Styles,
	circuit_tabs: Vec<LogicCircuitToplevelView>,
	current_tab_index: usize,
	new_circuit_name: String,
	new_circuit_path: String,
	load_circuit_err_opt: Option<String>,
	readme_file: String
}

impl App {
	pub fn new() -> Self {
		// Load styles
		let styles: Styles = match Styles::load() {
			Ok(styles) => styles,
			Err(e) => {
				println!("Could not load styles: {}, resorting to default", e);
				Styles::default()
			}
		};
		Self {
			styles,
			circuit_tabs: Vec::new(),//vec![LogicCircuitToplevelView::new(create_simple_circuit(), false)],
			current_tab_index: 0,
			new_circuit_name: String::new(),
			new_circuit_path: String::new(),
			load_circuit_err_opt: None,
			readme_file: resource_interface::load_file_with_better_error("README.md").unwrap()
		}
	}
	fn load_circuit_tab(&mut self, file_path: &str) {
		match resource_interface::load_circuit(file_path, false, true, IntV2(0, 0), FourWayDir::default(), String::new()) {
			Ok(circuit) => {
				self.circuit_tabs.push(LogicCircuitToplevelView::new(circuit, true, &self.styles));
				self.current_tab_index = self.circuit_tabs.len();// Not an OBOE
			},
			Err(e) => {
				self.load_circuit_err_opt = Some(e);
			}
		}
	}
}

impl eframe::App for App {
	fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
		let circuit_names: Vec<String> = self.circuit_tabs.iter().map(|toplevel| toplevel.circuit.type_name.clone()).collect();
		egui::CentralPanel::default().show(ctx, |ui: &mut Ui| {
			// This function by default is only run upon user interaction, so copied this from https://users.rust-lang.org/t/issues-while-writing-a-clock-with-egui/102752
			ui.ctx().request_repaint();
			ScrollArea::horizontal().scroll_bar_visibility(ScrollBarVisibility::AlwaysHidden).show(ui, |ui| {
				ui.horizontal(|ui| {
					if ui.button("Home").clicked() {
						if self.current_tab_index != 0 {
							self.current_tab_index = 0;
						}
					}
					for (i, circuit_name) in circuit_names.iter().enumerate() {
						let name_for_ui: &str = match self.circuit_tabs[i].saved {
							true => circuit_name,
							false => &format!("{} *", circuit_name)
						};
						if ui.button(name_for_ui).clicked() {
							if i + 1 != self.current_tab_index {
								self.current_tab_index = i + 1;
							}
						}
					}
					let new_button_response = ui.button("+");
					if new_button_response.clicked() {
						self.new_circuit_name = String::new();
						self.new_circuit_path = String::new();
						self.load_circuit_err_opt = None;
					}
					Popup::menu(&new_button_response).close_behavior(PopupCloseBehavior::CloseOnClickOutside).align(RectAlign::RIGHT_START).align_alternatives(&[RectAlign::LEFT_START]).show(|ui| {
						ui.menu_button("New Circuit", |ui| {
							ui.horizontal(|ui| {
								ui.label("Name: ");
								ui.text_edit_singleline(&mut self.new_circuit_name);
							});
							ui.horizontal(|ui| {
								ui.label("Save file path: ");
								ui.text_edit_singleline(&mut self.new_circuit_path);
								ui.label(".json");
							});
							if ui.button("Create Circuit").clicked() {
								self.circuit_tabs.push(LogicCircuitToplevelView::new(LogicCircuit::new_mostly_default(self.new_circuit_name.clone(), self.new_circuit_path.clone(), true), false, &self.styles));
								self.current_tab_index = self.circuit_tabs.len();// Not an OBOE
							}
						});
						ui.menu_button("Load Circuit", |ui| {
							let files_list = resource_interface::list_all_circuit_files().unwrap();
							if files_list.len() == 0 {
								ui.label(format!("No circuit files found in {}", resource_interface::CIRCUITS_DIR));
							}
							ScrollArea::vertical().show(ui, |ui| {
								for file_path in files_list {
									if ui.selectable_label(false, &file_path).clicked() {
										self.load_circuit_tab(&file_path);
									}
								}
							});
							if let Some(load_error) = &self.load_circuit_err_opt {
								ui.label(format!("Loading error: {}", load_error));
							}
						});
					});
				});
			});
			if self.current_tab_index == 0 {// Home tab
				ui.vertical(|ui| {
					ScrollArea::vertical().show(ui, |ui| {
						CommonMarkViewer::new().show(ui, &mut CommonMarkCache::default(), &self.readme_file);
					});
				});
			}
			else {
				let circuit_toplevel: &mut LogicCircuitToplevelView = &mut self.circuit_tabs[self.current_tab_index - 1];
				let (new_mouse_pos_opt, new_circuit_tab_opt): (Option<Pos2>, Option<String>) = circuit_toplevel.draw(ui, &mut self.styles, ctx.screen_rect().min);// TODO: Get actual window top-left position
				if let Some(new_pos) = new_mouse_pos_opt {
					let mouse = mouse_rs::Mouse::new();
					mouse.move_to(new_pos.x as i32, new_pos.y as i32).unwrap();
				}
				if let Some(new_circuit_tab) = new_circuit_tab_opt {
					self.load_circuit_tab(&new_circuit_tab);
				}
			}
		});
	}
}

fn ui_drag_value_with_arrows<N: emath::Numeric + SubAssign + AddAssign + From<u8> + Into<i32>>(ui: &mut Ui, value: &mut N, range_opt: Option<(N, N)>) -> bool {
	let mut down_enable = true;
	let mut up_enable = true;
	let value_copy = *value;
	if let Some(range) = range_opt {
		if value_copy <= range.0 {
			down_enable = false;
		}
		if value_copy >= range.1 {
			up_enable = false;
		}
	}
	if ui.add_enabled(down_enable, Button::new("<")).clicked() {
		*value -= 1_u8.into();
		return true;
	}
	let mut drag_value = DragValue::new(value);
	if let Some(range) = range_opt {
		drag_value = drag_value.range(RangeInclusive::new(range.0, range.1));
		drag_value = drag_value.clamp_existing_to_range(true);
	}
	if ui.add(drag_value).changed() {
		return true;
	}
	if ui.add_enabled(up_enable, Button::new(">")).clicked() {
		*value += 1_u8.into();
		return true;
	}
	false
}