//! Graphical wire, can have 1 or more logical wires inside which are setup automatically based on the bit width of whatever it is graphically connected to

use std::{cell::RefCell, collections::HashSet, default::Default, fmt::Debug, rc::Rc};
use serde::{Deserialize, Serialize};
use crate::prelude::*;

/// Just a straight segment, either horizontal or vertical
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Wire {
	pub ui_data: UIData,
	pub length: u32,
	/// Vec<Net ID>, Each logical net/wire that is represented by this graphical wire. Length is equal to this wire's bit width
	pub nets: Vec<u64>,
	pub color: [u8; 3],
	pub start_connections: Rc<RefCell<HashSet<WireConnection>>>,
	pub end_connections: Rc<RefCell<HashSet<WireConnection>>>,
	start_selected: bool,
	end_selected: bool,
	position_before_dragging: IntV2
}

impl Wire {
	pub fn new(
		pos: IntV2,
		length: u32,
		direction: FourWayDir,
		nets: Vec<u64>,
		start_connections: Rc<RefCell<HashSet<WireConnection>>>,
		end_connections: Rc<RefCell<HashSet<WireConnection>>>
	) -> Self {
		Self {
			ui_data: UIData::new(pos, direction, Self::bb_from_len(length)),
			length,
			nets,
			color: [0, 0, 0],
			start_connections,
			end_connections,
			start_selected: false,
			end_selected: false,
			position_before_dragging: pos
		}
	}
	pub fn bit_width(&self) -> u16 {
		self.nets.len() as u16
	}
	fn bb_from_len(length: u32) -> (V2, V2) {
		(V2::new(0.25, -0.25), V2::new(length as f32 - 0.25, 0.25))
	}
	/// Returns: (Start, Middle, End)
	pub fn contains_point(&self, point: IntV2) -> ((bool, bool, bool), Option<Rc<RefCell<HashSet<WireConnection>>>>) {
		// Start
		if point == self.ui_data.position {
			return ((true, false, false), Some(Rc::clone(&self.start_connections)));
		}
		// End
		if point == self.ui_data.position + self.ui_data.direction.to_unit_int().mult(self.length as i32) {
			return ((false, false, true), Some(Rc::clone(&self.end_connections)));
		}
		// Middle
		let this_to_point_v = point - self.ui_data.position;
		if let Some(test_dir) = this_to_point_v.is_along_axis() {
			if test_dir == self.ui_data.direction && this_to_point_v.to_v2().magnitude() < self.length as f32 {
				return ((false, true, false), None);
			}
		}
		// None
		((false, false, false), None)
	}
	pub fn perpindicular_pair_to_segments(perp_pair: &(IntV2, FourWayDir), end_pos: IntV2) -> Vec<(IntV2, FourWayDir, u32)> {
		// Check if along straight line
		let v = end_pos - perp_pair.0;
		if v.taxicab() == 0 {
			return vec![];
		}
		if let Some(_) = v.is_along_axis() {
			return vec![(
				perp_pair.0,
				perp_pair.1,
				v.taxicab()
			)];
		}
		let pair: [(IntV2, FourWayDir, u32); 2] = match perp_pair.1.is_horizontal() {
			true => if v.0 > 0 {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, v.0 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.1 > 0 {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::N, v.1 as u32)
				}
				else {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::S, -v.1 as u32)
				};
				[first, second]
			}
			else {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, -v.0 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.1 > 0 {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::N, v.1 as u32)
				}
				else {
					(perp_pair.0 + IntV2(v.0, 0), FourWayDir::S, -v.1 as u32)
				};
				[first, second]
			},
			false => if v.1 > 0 {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, v.1 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.0 > 0 {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::E, v.0 as u32)
				}
				else {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::W, -v.0 as u32)
				};
				[first, second]
			}
			else {
				let first: (IntV2, FourWayDir, u32) = (perp_pair.0, perp_pair.1, -v.1 as u32);
				let second: (IntV2, FourWayDir, u32) = if v.0 > 0 {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::E, v.0 as u32)
				}
				else {
					(perp_pair.0 + IntV2(0, v.1), FourWayDir::W, -v.0 as u32)
				};
				[first, second]
			}
		};
		vec![pair[0], pair[1]]
	}
	pub fn end_pos(&self) -> IntV2 {
		self.ui_data.position + (self.ui_data.direction.to_unit_int().mult(self.length as i32))
	}
	pub fn set_length(&mut self, new_len: u32) {
		self.length = new_len;
		self.ui_data.local_bb = Self::bb_from_len(new_len);
	}
	pub fn get_len(&self) -> u32 {
		self.length
	}
}

impl GraphicSelectableItem for Wire {
	fn draw<'a>(&self, draw: &Box<dyn DrawInterface>) {
		let start_pos = self.ui_data.position.to_v2();
		let end_pos = self.end_pos().to_v2();
		draw.draw_polyline(
			vec![
				start_pos,
				end_pos
			],
			self.color
		);
		if self.start_connections.borrow().len() >= 3 {
			draw.draw_circle_filled(start_pos, draw.get_draw_data().styles.connection_dot_grid_size, self.color);
		}
		if self.end_connections.borrow().len() >= 3 {
			draw.draw_circle_filled(end_pos, draw.get_draw_data().styles.connection_dot_grid_size, self.color);
		}
	}
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	/// Excludes end BBs which are special and for dragging the ends around or extruding at right angles
	/*fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let local_bb_unrectified: (V2, V2) = match self.ui_data.direction {
			FourWayDir::E => (V2::new(0.25, -0.25), V2::new(self.length as f32 - 0.25, 0.25)),
			FourWayDir::N => (V2::new(-0.25, 0.25 - (self.length as f32)), V2::new(0.25, -0.25)),
			FourWayDir::W => (V2::new(0.25 - self.length as f32, -0.25), V2::new(-0.25, 0.25)),
			FourWayDir::S => (V2::new(-0.25, -0.25), V2::new(0.25, 0.25 - (self.length as f32)))
		};
		let local_bb: (V2, V2) = merge_points_to_bb(vec![local_bb_unrectified.0, local_bb_unrectified.1]);
		let offset = grid_offset + self.ui_data.position.to_v2();
		(local_bb.0 + offset, local_bb.1 + offset)
	}*/
	fn is_connected_to_net(&self, net_id: u64) -> bool {
		for test_net_id in &self.nets {
			if *test_net_id == net_id {
				return true;
			}
		}
		return false;
	}
	#[cfg(feature = "using_egui")]
	fn get_properties(&self) -> Vec<SelectProperty> {
		Vec::new()
	}
	#[cfg(feature = "using_egui")]
	fn set_property(&mut self, _property: SelectProperty) {}
	#[cfg(feature = "using_egui")]
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Wire((self.ui_data.position, self.ui_data.direction, self.length))
	}
}

/// What could the end of a be wire connected to?
/// Up to 3 of these
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum WireConnection {
	/// Component or external pin
	Pin(CircuitWideGraphicPinReference),
	/// Another straight wire segment
	Wire(u64),
	/// (Splitter ID, splitter graphic pin #)
	Splitter(u64, u16)
}