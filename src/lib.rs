//! Simulation inspired by CircuitVerse, UI based off of KiCad

use std::{cell::RefCell, ops, f32::consts::PI};
use serde::{Serialize, Deserialize};
use nalgebra::Vector2;
#[cfg(feature = "using_egui")]
use eframe::emath;
#[cfg(feature = "using_wasm")]
use wasm_bindgen::prelude::wasm_bindgen;

pub mod simulator;
#[cfg(feature = "using_egui")]
pub mod ui;
pub mod graphics;
pub mod resource_interface;
pub mod builtin_components;
pub mod circuit_net_computation;
#[cfg(test)]
pub mod tests;

#[allow(unused)]
pub mod prelude {
	use std::{clone, collections::{HashMap, HashSet}, fmt::Formatter, hash::Hash, rc::Rc, time::Instant};
	use super::*;
	// Name of this app
	pub const APP_NAME: &str = "Rusty Logic";
	pub const CIRCUIT_MAX_COMPUTE_CYCLES: usize = 100;
	pub const CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH: usize = 10;
	/// Square box shown around wire ends and unconnected pins to start a wire from
	pub const WIRE_START_POINT_HALF_WIDTH: f32 = 0.25;
	pub const UI_MAX_FRAME_SIMULATION_TIME: f32 = 0.033;
	pub type V2 = Vector2<f32>;
	#[cfg(feature = "using_egui")]
	use eframe::egui::{Align2, Align, Color32, CornerRadius};
	#[cfg(feature = "using_egui")]
	pub use ui::{LogicCircuitToplevelView, App, SelectProperty, CopiedGraphicItem, CopiedItemSet};
	pub use graphics::{UIData, GraphicSelectableItem, DrawInterface, Styles, DrawData};
	pub use simulator::{LogicDevice, LogicDeviceGeneric, Wire, LogicNet, LogicConnectionPin, GraphicPin, LogicCircuit, LogicState, LogicConnectionPinExternalSource, LogicConnectionPinInternalSource, WireConnection, LogicDeviceSave, GraphicLabel, GraphicLabelSave, Splitter, SplitterSave, Probe, ProbeSave};
	pub use resource_interface::EnumAllLogicDevices;
	#[cfg(feature = "using_filesystem")]
	pub use resource_interface::load_file_with_better_error;
	pub use circuit_net_computation::BitWidthError;
	#[cfg(feature = "using_egui")]
	pub fn u8_3_to_color32(in_: [u8; 3]) -> Color32 {
		Color32::from_rgb(in_[0], in_[1], in_[2])
	}
	#[cfg(feature = "using_egui")]
	pub fn u8_4_to_color32(in_: [u8; 4]) -> Color32 {
		Color32::from_rgba_unmultiplied(in_[0], in_[1], in_[2], in_[3])
	}
	#[cfg(feature = "using_egui")]
	pub fn emath_vec2_to_v2(in_: emath::Vec2) -> V2 {
		V2::new(in_.x, in_.y)
	}
	#[cfg(feature = "using_egui")]
	pub fn emath_pos2_to_v2(in_: emath::Pos2) -> V2 {
		V2::new(in_.x, in_.y)
	}
	#[cfg(feature = "using_egui")]
	pub fn v2_to_emath_vec2(in_: V2) -> emath::Vec2 {
		emath::Vec2{x: in_.x, y: in_.y}
	}
	#[cfg(feature = "using_egui")]
	pub fn v2_to_emath_pos2(in_: V2) -> emath::Pos2 {
		emath::Pos2{x: in_.x, y: in_.y}
	}
	pub fn round_v2_to_intv2(in_: V2) -> IntV2 {
		IntV2(in_.x.round() as i32, in_.y.round() as i32)
	}
	pub fn v2_reverse_y(v: V2) -> V2 {
		V2::new(v.x, -v.y)
	}
	pub fn angle_radius_to_v2(angle_deg: f32, radius: f32) -> V2 {
		let angle_rad = angle_deg * PI / 180.0;
		V2::new(angle_rad.cos(), angle_rad.sin()) * radius
	}
	pub fn vec_to_u64_keyed_hashmap<T>(vec_: Vec<T>) -> HashMap<u64, T> {
		let mut out = HashMap::<u64, T>::new();
		for (i, item) in vec_.into_iter().enumerate() {
			out.insert(i as u64, item);
		}
		out
	}
	pub fn hashmap_into_refcells<K: Eq + Hash, V>(map: HashMap<K, V>) -> HashMap<K, RefCell<V>> {
		let mut out = HashMap::<K, RefCell<V>>::new();
		for (k, v) in map.into_iter() {
			out.insert(k, RefCell::new(v));
		}
		out
	}
	pub fn hashmap_unwrap_refcells<K: Eq + Hash, V>(map: HashMap<K, RefCell<V>>) -> HashMap<K, V> {
		let mut out = HashMap::<K, V>::new();
		for (k, v) in map.into_iter() {
			out.insert(k, v.into_inner());
		}
		out
	}
	pub fn merge_points_to_bb(points: Vec<V2>) -> (V2, V2) {// From ChatGPT
		if points.is_empty() {
			// Return a degenerate bounding box at the origin if there are no points
			return (V2::new(0.0, 0.0), V2::new(0.0, 0.0));
		}

		let mut min_x = points[0].x;
		let mut min_y = points[0].y;
		let mut max_x = points[0].x;
		let mut max_y = points[0].y;

		for p in &points[1..] {
			if p.x < min_x { min_x = p.x; }
			if p.y < min_y { min_y = p.y; }
			if p.x > max_x { max_x = p.x; }
			if p.y > max_y { max_y = p.y; }
		}

		(V2::new(min_x, min_y), V2::new(max_x, max_y))
	}
	pub fn merge_points_to_bb_reversed_y(points: Vec<V2>) -> (V2, V2) {// From ChatGPT, modified
		if points.is_empty() {
			// Return a degenerate bounding box at the origin if there are no points
			return (V2::new(0.0, 0.0), V2::new(0.0, 0.0));
		}

		let mut min_x = points[0].x;
		let mut min_y = points[0].y;
		let mut max_x = points[0].x;
		let mut max_y = points[0].y;

		for p in &points[1..] {
			if p.x < min_x { min_x = p.x; }
			if p.y < min_y { min_y = p.y; }
			if p.x > max_x { max_x = p.x; }
			if p.y > max_y { max_y = p.y; }
		}

		(V2::new(min_x, max_y), V2::new(max_x, min_y))
	}
	pub fn bbs_overlap(a: (V2, V2), b: (V2, V2)) -> bool {// From ChatGPT
		let (a_min, a_max) = a;
		let (b_min, b_max) = b;

		!(a_max.x <= b_min.x || a_min.x >= b_max.x ||
		a_max.y <= b_min.y || a_min.y >= b_max.y)
	}
	pub fn bb_to_polyline(bb: (V2, V2)) -> Vec<V2> {
		vec![
			bb.0,
			V2::new(bb.1.x, bb.0.y),
			bb.1,
			V2::new(bb.0.x, bb.1.y),
			bb.0
		]
	}
	/// From Gemini
	/// Clips a line segment to an axis-aligned bounding box (AABB).
	///
	/// Implements the Liang-Barsky algorithm.
	///
	/// # Arguments
	/// * `line` - A tuple `(p0, p1)` representing the start and end points of the line segment.
	/// * `rect` - A tuple `(min, max)` representing the minimum (bottom-left) and
	///            maximum (top-right) corners of the rectangle.
	///
	/// # Returns
	/// * `Some((clip_p0, clip_p1))` - The new start and end points of the clipped segment if it
	///                               is inside the rectangle.
	/// * `None` - If the line segment is entirely outside the rectangle.
	pub fn clip_line_to_rect(line: (V2, V2), rect: (V2, V2)) -> Option<(V2, V2)> {
		let (p0, p1) = line;
		let (rect_min, rect_max) = rect;

		// Vector representing the line's direction and length
		let d = p1 - p0;

		// 't' values for entry (min) and exit (max) of the clip window
		let mut t_min: f32 = 0.0;
		let mut t_max: f32 = 1.0;

		// Pre-calculate p and q for all four boundaries (Left, Right, Bottom, Top)
		// p_k = dot(normal_k, d)
		// q_k = dot(normal_k, p0 - boundary_point_k)
		//
		// For AABB, this simplifies:
		// Left:   p = -d.x, q = p0.x - rect_min.x
		// Right:  p =  d.x, q = rect_max.x - p0.x
		// Bottom: p = -d.y, q = p0.y - rect_min.y
		// Top:    p =  d.y, q = rect_max.y - p0.y
		
		let p = [-d.x, d.x, -d.y, d.y];
		let q = [
			p0.x - rect_min.x,  // Left
			rect_max.x - p0.x,  // Right
			p0.y - rect_min.y,  // Bottom
			rect_max.y - p0.y,  // Top
		];

		// Loop through all four boundaries
		for i in 0..4 {
			let p_k = p[i];
			let q_k = q[i];

			// Use a small epsilon for floating point comparison
			if p_k.abs() < 1e-9 {
				// Case 1: Line is parallel to the boundary (p_k ≈ 0)
				if q_k < 0.0 {
					// Line is outside this boundary and parallel, so it's outside the box
					return None;
				}
				// If q_k >= 0, line is inside this parallel boundary, so we continue
			} else {
				// Case 2: Line is not parallel, calculate intersection 't'
				let t = q_k / p_k;

				if p_k < 0.0 {
					// Line is "entering" from outside this boundary
					// This is a potential new t_min
					t_min = t_min.max(t);
				} else { // p_k > 0.0
					// Line is "exiting" from inside this boundary
					// This is a potential new t_max
					t_max = t_max.min(t);
				}

				// Check if the 't' window has become invalid
				if t_min > t_max {
					// The "entry" point is after the "exit" point, so the line misses
					return None;
				}
			}
		}

		// If we get here, the line (or a part of it) is inside
		// Calculate the new clipped endpoints using the final t_min and t_max
		let clip_p0 = p0 + t_min * d;
		let clip_p1 = p0 + t_max * d;

		Some((clip_p0, clip_p1))
	}
	pub fn lowest_unused_key<V>(map: &HashMap<u64, V>) -> u64 {
		let mut i: u64 = 0;
		while map.contains_key(&i) {
			i += 1;
		}
		i
	}
	pub fn set_lowest_unused_key(set: &HashSet<u64>) -> u64 {
		let mut i: u64 = 0;
		while set.contains(&i) {
			i += 1;
		}
		i
	}
	pub fn batch_unused_keys<V>(map: &HashMap<u64, V>, n: usize) -> Vec<u64> {
		let mut out = Vec::<u64>::new();
		let mut i: u64 = 0;
		while out.len() < n {
			while map.contains_key(&i) {
				i += 1;
			}
			out.push(i);
			i += 1;
		}
		out
	}
	pub fn n_max<N: PartialOrd + Copy>(v: &Vec<N>) -> Option<N> {
		let mut out = Option::<N>::None;
		for n in v {
			if let Some(current_n) = out {
				if *n > current_n {
					out = Some(*n);
				}
			}
			else {
				out = Some(*n);
			}
		}
		out
	}
	pub fn n_min<N: PartialOrd + Copy>(v: &Vec<N>) -> Option<N> {
		let mut out = Option::<N>::None;
		for n in v {
			if let Some(current_n) = out {
				if *n < current_n {
					out = Some(*n);
				}
			}
			else {
				out = Some(*n);
			}
		}
		out
	}
	/*pub fn new_pin_name(pins: &HashMap<u64, RefCell<LogicConnectionPin>>) -> String {
		let mut i: u64 = 0;
		while pins.contains_key(&format!("pin_{}", i)) {
			i += 1;
		}
		format!("pin_{}", i)
	}*/
	pub fn clone_option_rc<T>(rc_opt: &Option<Rc<T>>) -> Option<Rc<T>> {
		match rc_opt {
			Some(rc) => Some(Rc::clone(rc)),
			None => None
		}
	}
	/// Adds all of `other` into `base`
	pub fn merge_wire_end_connection_sets(base_cell: &Rc<RefCell<HashSet<WireConnection>>>, other_cell: &Rc<RefCell<HashSet<WireConnection>>>) {
		if Rc::ptr_eq(base_cell, other_cell) {
			return;
		}
		let mut base = base_cell.borrow_mut();
		let other = other_cell.borrow();
		for other_item in other.iter() {
			base.insert(other_item.clone());
		}
	}
	#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
	pub enum FourWayDir {
		E,
		N,
		W,
		S
	}
	impl FourWayDir {
		pub fn to_unit(&self) -> V2 {
			match &self {
				Self::E => V2::new(1.0, 0.0),
				Self::N => V2::new(0.0, 1.0),
				Self::W => V2::new(-1.0, 0.0),
				Self::S => V2::new(0.0, -1.0)
			}
		}
		pub fn to_unit_int(&self) -> IntV2 {
			match &self {
				Self::E => IntV2(1, 0),
				Self::N => IntV2(0, 1),
				Self::W => IntV2(-1, 0),
				Self::S => IntV2(0, -1)
			}
		}
		pub fn to_dir_deg(&self) -> f32 {
			match &self {
				Self::E => 0.0,
				Self::N => 90.0,
				Self::W => 180.0,
				Self::S => 270.0
			}
		}
		pub fn to_string(&self) -> String {
			match &self {
				Self::E => "East".to_string(),
				Self::N => "North".to_string(),
				Self::W => "West".to_string(),
				Self::S => "South".to_string()
			}
		}
		pub fn is_horizontal(&self) -> bool {
			match &self {
				Self::E => true,
				Self::N => false,
				Self::W => true,
				Self::S => false
			}
		}
		pub fn opposite_direction(&self) -> Self {
			match &self {
				Self::E => Self::W,
				Self::N => Self::S,
				Self::W => Self::E,
				Self::S => Self::N
			}
		}
		pub fn turn_ccw(&self) -> Self {
			match &self {
				Self::E => Self::N,
				Self::N => Self::W,
				Self::W => Self::S,
				Self::S => Self::E
			}
		}
		pub fn turn_cw(&self) -> Self {
			match &self {
				Self::E => Self::S,
				Self::N => Self::E,
				Self::W => Self::N,
				Self::S => Self::W
			}
		}
		pub fn cos_sin(&self) -> (i32, i32) {
			match &self {
				Self::E => (1, 0),
				Self::N => (0, 1),
				Self::W => (-1, 0),
				Self::S => (0, -1)
			}
		}
		pub fn to_align2(&self) -> GenericAlign2 {
			match &self {
				Self::E => GenericAlign2::RIGHT_CENTER,
				Self::N => GenericAlign2::CENTER_TOP,
				Self::W => GenericAlign2::LEFT_CENTER,
				Self::S => GenericAlign2::CENTER_BOTTOM
			}
		}
		/// To put text either vertically or horizontally but not upside down
		pub fn rectify(&self) -> Self {
			match &self {
				Self::E => Self::E,
				Self::N => Self::N,
				Self::W => Self::E,
				Self::S => Self::N
			}
		}
		/// 2D rotation matrix multiplication
		pub fn rotate_v2(&self, in_: V2) -> V2 {
			let (cos_int, sin_int) = self.cos_sin();
			let (cos, sin) = (cos_int as f32, sin_int as f32);
			V2::new(in_.x * cos - in_.y * sin, in_.x * sin + in_.y * cos)
		}
		/// 2D rotation matrix multiplication
		pub fn rotate_v2_reverse(&self, in_: V2) -> V2 {
			let (cos_int, sin_int) = self.cos_sin();
			let (cos, sin) = (cos_int as f32, -(sin_int as f32));
			V2::new(in_.x * cos - in_.y * sin, in_.x * sin + in_.y * cos)
		}
		/// 2D rotation matrix multiplication with int
		pub fn rotate_intv2(&self, in_: IntV2) -> IntV2 {
			let (cos, sin) = self.cos_sin();
			IntV2(in_.0 * cos - in_.1 * sin, in_.0 * sin + in_.1 * cos)
		}
		/// 2D rotation matrix multiplication with int
		pub fn rotate_intv2_reverse(&self, in_: IntV2) -> IntV2 {
			let (cos, sin) = self.cos_sin();
			IntV2(in_.0 * cos + in_.1 * sin, in_.1 * cos - in_.0 * sin)
		}
	}
	impl Default for FourWayDir {
		fn default() -> Self {
			Self::E
		}
	}
	pub fn to_string_err<T, E: ToString>(result: Result<T, E>) -> Result<T, String> {
		match result {
			Ok(t) => Ok(t),
			Err(e) => Err(e.to_string())
		}
	}
	pub fn to_string_err_with_message<T, E: ToString>(result: Result<T, E>, message: &str) -> Result<T, String> {
		match result {
			Ok(t) => Ok(t),
			Err(e) => Err(format!("Message: {}, Error: {}", message, e.to_string()))
		}
	}

	/// Copied from Egui
	#[derive(Debug, Clone, Copy, PartialEq)]
	pub enum GenericAlign {
		Min,
		Center,
		Max,
	}
	impl GenericAlign {
		#[cfg(feature = "using_egui")]
		pub fn to_egui(&self) -> Align {
			match &self {
				Self::Min => Align::Min,
				Self::Center => Align::Center,
				Self::Max => Align::Max
			}
		}
	}
	#[derive(Debug, Clone, Copy, PartialEq)]
	pub struct GenericAlign2(pub [GenericAlign; 2]);
	impl GenericAlign2 {
		#[cfg(feature = "using_egui")]
		pub fn to_egui(&self) -> Align2 {
			Align2([self.0[0].to_egui(), self.0[1].to_egui()])
		}
		pub const LEFT_BOTTOM: Self = Self([GenericAlign::Min, GenericAlign::Max]);
		pub const LEFT_CENTER: Self = Self([GenericAlign::Min, GenericAlign::Center]);
		pub const LEFT_TOP: Self = Self([GenericAlign::Min, GenericAlign::Min]);
		pub const CENTER_BOTTOM: Self = Self([GenericAlign::Center, GenericAlign::Max]);
		pub const CENTER_CENTER: Self = Self([GenericAlign::Center, GenericAlign::Center]);
		pub const CENTER_TOP: Self = Self([GenericAlign::Center, GenericAlign::Min]);
		pub const RIGHT_BOTTOM: Self = Self([GenericAlign::Max, GenericAlign::Max]);
		pub const RIGHT_CENTER: Self = Self([GenericAlign::Max, GenericAlign::Center]);
		pub const RIGHT_TOP: Self = Self([GenericAlign::Max, GenericAlign::Min]);
	}

	#[derive(Default, Serialize, Deserialize, Clone, Copy, PartialEq, Debug, Eq, Hash)]
	pub struct IntV2(pub i32, pub i32);

	impl IntV2 {
		pub fn mult(&self, other: i32) -> Self {
			Self(self.0 * other, self.1 * other)
		}
		pub fn to_v2(&self) -> V2 {
			V2::new(self.0 as f32, self.1 as f32)
		}
		pub fn is_along_axis(&self) -> Option<FourWayDir> {
			if self.0 == 0 {
				if self.1 > 0 {
					return Some(FourWayDir::N);
				}
				else {
					return Some(FourWayDir::S);
				}
			}
			if self.1 == 0 {
				if self.0 > 0 {
					return Some(FourWayDir::E);
				}
				else {
					return Some(FourWayDir::W);
				}
			}
			None
		}
		pub fn taxicab(&self) -> u32 {
			(self.0.abs() + self.1.abs()) as u32
		}
		pub fn dot(&self, other: Self) -> i32 {
			self.0 * other.0 + self.1 * other.1
		}
		pub fn reverse_y(&self) -> Self {
			Self(self.0, -self.1)
		}
	}

	impl ops::Add<IntV2> for IntV2 {
		type Output = IntV2;

		fn add(self, other: Self) -> Self {
			Self(self.0 + other.0, self.1 + other.1)
		}
	}

	impl ops::Sub<IntV2> for IntV2 {
		type Output = IntV2;

		fn sub(self, other: IntV2) -> Self {
			Self(self.0 - other.0, self.1 - other.1)
		}
	}

	impl ops::Index<usize> for IntV2 {
		type Output = i32;
		fn index(&self, index: usize) -> &Self::Output {
			match index {
				0 => &self.0,
				1 => &self.1,
				n => panic!("IntV2 index must be 0 or 1, not {}", n)
			}
		}
	}

	use simulator::{CircuitWideLogicPinReference, ComponentLogicPinReference};
	pub fn create_simple_circuit() -> LogicCircuit {
		LogicCircuit::new(
			vec_to_u64_keyed_hashmap(vec![
				Box::new(builtin_components::GateAnd::new()).into_box()
			]),
			vec![
				(IntV2(-4, -1), FourWayDir::W, 1.0, "a".to_owned(), vec![0]),
				(IntV2(-4, 1), FourWayDir::W, 1.0, "b".to_owned(), vec![1]),
				(IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), vec![2]),
			],
			"test-circuit".to_string(),
			None,
			vec_to_u64_keyed_hashmap(vec![
				Wire::new(IntV2(-4, -1), 1, FourWayDir::E, vec![0], Rc::new(RefCell::new(HashSet::new())), Rc::new(RefCell::new(HashSet::new()))),
				Wire::new(IntV2(-4, 1), 1, FourWayDir::E, vec![1], Rc::new(RefCell::new(HashSet::new())), Rc::new(RefCell::new(HashSet::new()))),
				Wire::new(IntV2(4, 0), 1, FourWayDir::W, vec![2], Rc::new(RefCell::new(HashSet::new())), Rc::new(RefCell::new(HashSet::new())))
			]),
			"test".to_string(),
			true,
			true
		).unwrap()
	}
}

#[cfg(feature = "using_wasm")]
#[wasm_bindgen(js_name = "js_get_circuit_save_file")]
extern "C" {
    pub fn js_get_circuit_save_file(name: &str) -> Option<String>;
	pub fn alert(s: &str);
}

#[cfg(feature = "using_egui")]
use prelude::*;

#[cfg(feature = "using_egui")]
pub fn ui_main() {
	let native_options = eframe::NativeOptions::default();
	eframe::run_native(&APP_NAME, native_options, Box::new(|_| Ok(Box::new(App::new())))).unwrap();
}