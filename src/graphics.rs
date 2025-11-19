//! Graphics functionality that will be used regardless of whether egui is being used

use crate::{prelude::*, resource_interface};
use serde::{Serialize, Deserialize};
use serde_json;
use std::rc::Rc;

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
	pub color_error_x: [u8; 3],
	pub select_rect_color: [u8; 4],
	pub select_rect_edge_color: [u8; 3],
	pub color_wire_in_progress: [u8; 3],
	pub text_size_grid: f32,
	pub text_color: [u8; 3],
	pub wire_start_point_outline_color: [u8; 3],
	pub timing_diagram_event_resolution_px: f32,
	pub timing_diagram_prop_step_resolution_px: f32,
	/// Pixels/Second
	pub timing_diagram_real_time_resolution_px: f32,
	pub timing_diagram_bus_half_change_px: f32
}

impl Styles {
	#[cfg(feature = "using_filesystem")]
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
			color_error_x: [255, 0, 0],
			select_rect_color: [7, 252, 244, 128],
			select_rect_edge_color: [252, 7, 7],
			color_wire_in_progress: [2, 156, 99],
			text_size_grid: 0.9,
			text_color: [243, 118, 252],
			wire_start_point_outline_color: [93, 252, 167],
			timing_diagram_event_resolution_px: 30.0,
			timing_diagram_prop_step_resolution_px: 7.0,
			timing_diagram_real_time_resolution_px: 30.0,
			timing_diagram_bus_half_change_px: 2.0
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
	fn draw<'a>(&self, draw: &Box<dyn DrawInterface>);
	fn get_ui_data(&self) -> &UIData;
	fn get_ui_data_mut(&mut self) -> &mut UIData;
	/// Used for the net highlight feature
	#[allow(unused)]
	fn is_connected_to_net(&self, net_id: u64) -> bool {false}
	#[cfg(feature = "using_egui")]
	fn get_properties(&self) -> Vec<SelectProperty>;
	#[cfg(feature = "using_egui")]
	fn set_property(&mut self, property: SelectProperty);
	#[cfg(feature = "using_egui")]
	fn copy(&self) -> CopiedGraphicItem;
	/// Meant for external connections so that clicks can do something special instead of just selecting them
	/// Returns: Whether the click was "used", if so then it won't be selected normally but can still be command-clicked and included in a dragged rectangle
	#[allow(unused)]
	#[cfg(feature = "using_egui")]
	fn accept_click(&mut self, local_pos: V2) -> bool {
		false
	}
	#[cfg(feature = "using_egui")]
	fn get_selected(&self) -> bool {
		self.get_ui_data().selected
	}
	#[cfg(feature = "using_egui")]
	fn set_selected(&mut self, selected: bool) {
		self.get_ui_data_mut().selected = selected;
	}
	/// Relative to Grid and self, return must be wrt global grid, hence why grid offset must be provided
	/// grid_offset will only correct for positions of nested sub-circuits, not the position of the object that it itself "knows about"
	#[cfg(feature = "using_egui")]
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let ui_data = self.get_ui_data();
		let local_bb = ui_data.local_bb;
		merge_points_to_bb(vec![grid_offset + ui_data.pos_to_parent_coords_float(local_bb.0), grid_offset + ui_data.pos_to_parent_coords_float(local_bb.1)])
	}
	/// Mouse is relative to Grid
	/// Things with complicated shapes should override this with something better
	#[cfg(feature = "using_egui")]
	fn is_click_hit(&self, mouse: V2, grid_offset: V2) -> bool {
		let bb: (V2, V2) = self.bounding_box(grid_offset);
		mouse.x >= bb.0.x && mouse.x <= bb.1.x && mouse.y >= bb.0.y && mouse.y <= bb.1.y
	}
	#[cfg(feature = "using_egui")]
	fn start_dragging(&mut self, current_mouse_pos: V2) {
		let current_pos = self.get_ui_data().position;
		self.get_ui_data_mut().dragging_offset = current_pos.to_v2() - current_mouse_pos;
	}
	#[cfg(feature = "using_egui")]
	fn dragging_to(&mut self, current_mouse_pos: V2) {
		let dragging_offset = self.get_ui_data().dragging_offset;
		self.get_ui_data_mut().position = round_v2_to_intv2(current_mouse_pos + dragging_offset);
	}
	#[cfg(feature = "using_egui")]
	fn stop_dragging(&mut self, final_mouse_pos: V2) {
		self.dragging_to(final_mouse_pos);
	}
}

#[derive(Clone)]
/// Struct with basic data (grid size, etc) that anything implementing `DrawInterface` should have a field for
pub struct DrawData {
	/// Location of center of screen with respect to the grid, it is this way so that it will not have to adjusted when the grid is zoomed in/out
	pub screen_center_wrt_grid: V2,
	/// Pixels per grid increment
	pub grid_size: f32,
	/// Includes component's own position, 
	pub offset_grid: IntV2,
	pub direction: FourWayDir,
	pub styles: Rc<Styles>,
	pub rect_center: V2,
	pub rect_size_px: V2,
	pub mouse_pos: Option<V2>
}

impl DrawData {
	pub fn new(
		screen_center_wrt_grid: V2,
		grid_size: f32,
		offset_grid: IntV2,
		direction: FourWayDir,
		styles: Rc<Styles>,
		rect_center: V2,
		rect_size_px: V2,
		mouse_pos: Option<V2>
	) -> Self {
		Self {
			screen_center_wrt_grid,
			grid_size,
			offset_grid,
			direction,
			styles,
			rect_center,
			rect_size_px,
			mouse_pos
		}
	}
	pub fn grid_to_px(&self, grid: V2) -> V2 {
		let nalgebra_v2 = ((self.direction.rotate_v2(grid) + self.offset_grid.to_v2() - self.screen_center_wrt_grid) * self.grid_size) + self.rect_center;
		if cfg!(feature = "reverse_y") {
			V2::new(nalgebra_v2.x, self.rect_size_px.y - nalgebra_v2.y)
		} else {
			V2::new(nalgebra_v2.x, nalgebra_v2.y)
		}
	}
	pub fn mouse_pos2_to_grid(&self, mouse_pos_y_backwards: V2) -> V2 {
		Self::mouse_pos2_to_grid_unattached(mouse_pos_y_backwards, self.direction, self.rect_center, self.offset_grid, self.screen_center_wrt_grid, self.rect_size_px, self.grid_size)
	}
	pub fn mouse_pos2_to_grid_unattached(mouse_pos_y_backwards: V2, direction: FourWayDir, rect_center: V2, offset_grid: IntV2, screen_center_wrt_grid: V2, rect_size_px: V2, grid_size: f32) -> V2 {
		#[cfg(feature = "reverse_y")]
		let mouse_pos = V2::new(mouse_pos_y_backwards.x, rect_size_px.y - mouse_pos_y_backwards.y);
		#[cfg(not(feature = "reverse_y"))]
		let mouse_pos = emath_pos2_to_v2(mouse_pos_y_backwards);
		direction.rotate_v2_reverse(((mouse_pos - rect_center) / grid_size) - offset_grid.to_v2()) + screen_center_wrt_grid
	}
	pub fn add_grid_pos_and_direction(&self, offset_unrotated: IntV2, dir_: FourWayDir) -> DrawData {
		let offset = self.direction.rotate_intv2(offset_unrotated);
		Self {
			screen_center_wrt_grid: self.screen_center_wrt_grid,
			grid_size: self.grid_size,
			offset_grid: self.offset_grid + offset,
			direction: self.direction.rotate_intv2(dir_.to_unit_int()).is_along_axis().unwrap(),
			styles: Rc::clone(&self.styles),
			rect_center: self.rect_center,
			rect_size_px: self.rect_size_px,
			mouse_pos: self.mouse_pos
		}
	}
}
/*
pub trait DrawInterface<'a> {
	fn get_draw_data(&'a self) -> &'a DrawData;
	fn get_grid_size(&'a self) -> f32 {
		self.get_draw_data().grid_size
	}
	fn styles(&'a self) -> &'a Styles {
		&self.get_draw_data().styles
	}
	fn draw_polyline(&self, points: Vec<V2>, stroke: [u8; 3]);
	fn draw_rect(&self, start_grid: V2, end_grid: V2, inside_stroke: [u8; 4], border_stroke: [u8; 3]);
	fn draw_circle(&self, center: V2, radius: f32, stroke: [u8; 3]);
	fn draw_circle_filled(&self, center: V2, radius: f32, stroke: [u8; 3]);
	fn draw_arc(&self, center_grid: V2, radius_grid: f32, start_deg: f32, end_deg: f32, stroke: [u8; 3]);
	fn text(&self, text: String, pos: V2, align: GenericAlign2, color: [u8; 3], size_grid: f32, vertical: bool);
	fn text_size(&self, text: String, size_grid: f32) -> V2;
	/// Can't return something with Self so using a diferent trait that will just convert back to this
	fn add_grid_pos_and_direction(&self, offset_unrotated: IntV2, dir_: FourWayDir) -> Box<dyn DrawInterface<'a>>;
}*/
pub trait DrawInterface {
	fn get_draw_data(&self) -> &DrawData;
	fn get_grid_size(&self) -> f32 {
		self.get_draw_data().grid_size
	}
	fn styles(&self) -> &Styles {
		&self.get_draw_data().styles
	}
	fn draw_polyline(&self, points: Vec<V2>, stroke: [u8; 3]);
	fn draw_rect(&self, start_grid: V2, end_grid: V2, inside_stroke: [u8; 4], border_stroke: [u8; 3]);
	fn draw_circle(&self, center: V2, radius: f32, stroke: [u8; 3]);
	fn draw_circle_filled(&self, center: V2, radius: f32, stroke: [u8; 3]);
	fn draw_arc(&self, center_grid: V2, radius_grid: f32, start_deg: f32, end_deg: f32, stroke: [u8; 3]);
	fn text(&self, text: &str, pos: V2, align: GenericAlign2, color: [u8; 3], size_grid: f32, vertical: bool);
	fn text_size(&self, text: &str, size_grid: f32) -> V2;
	/// Can't return something with Self so using a diferent trait that will just convert back to this
	fn add_grid_pos_and_direction(&self, offset_unrotated: IntV2, dir_: FourWayDir) -> Box<dyn DrawInterface>;
}
/*
/// `DrawInterface` can't return itself so created this to convert to and back again
pub trait DrawInterface2<'a> {
	fn to_draw_interface(&self) -> Box<dyn DrawInterface<'a>>;
}*/