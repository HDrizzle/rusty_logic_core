//! Graphical wire splitter, does not affect simulation performance

use std::{cell::RefCell, collections::HashSet, default::Default, fmt::Debug, rc::Rc};
use serde::{Deserialize, Serialize};
use crate::prelude::*;

/// Bus splitter with exact same functionality as in CircuitVerse
/// Not implemented as a component so that both ends can share the same net and make computation faster
/// Example Geometry/layout, note that graphical fanout pins can each have any number of logical pins as long as they all add up to the total bit width
/// Every logical pin on the splitter will be "attatched" to exactly 2 graphical pins: The base pin and one fanout pin (the splits do not overlap)
///     |- 4:7
///     |- 0:3
/// 0:7-|
///+ <- (0, 0)
#[derive(Debug, Clone)]
pub struct Splitter {
	pub ui_data: UIData,
	/// Total bit width
	pub bit_width: u16,
	/// Each entry represents a fanout graphic pin: Vec<(bit width, Option<(Grahic wire connection set, Vec of nets corresponding to bit width)>)>
	pub splits: Vec<(u16, Option<Rc<RefCell<HashSet<WireConnection>>>>)>,
	/// Graphical connections option for base pin, base pin is always graphical pin #0
	pub base_connections_opt: Option<Rc<RefCell<HashSet<WireConnection>>>>
}

impl Splitter {
	pub fn new() -> Self {
		Self {
			ui_data: UIData::new(IntV2(0, 0), FourWayDir::default(), Self::calculate_local_bb(8)),
			bit_width: 8,
			splits: vec![(1, None), (1, None), (1, None), (1, None), (1, None), (1, None), (1, None), (1, None)],
			base_connections_opt: None
		}
	}
	pub fn save(&self) -> SplitterSave {
		SplitterSave {
			pos: self.ui_data.position,
			dir: self.ui_data.direction,
			bit_width: self.bit_width,
			split_sizes: self.splits.iter().map(|t| t.0).collect()
		}
	}
	pub fn load(save: SplitterSave) -> Self {
		Self {
			ui_data: UIData::new(save.pos, save.dir, Self::calculate_local_bb(save.split_sizes.len())),
			bit_width: save.bit_width,
			splits: save.split_sizes.iter().map(|bw| (*bw, None)).collect(),
			base_connections_opt: None
		}
	}
	/// pin #0 is the full-bit-width pin and from 1... its the fanout pins
	pub fn graphic_pin_bit_width(&self, pin: u16) -> u16 {
		if pin == 0 {
			self.bit_width
		}
		else {
			self.splits[pin as usize - 1].0
		}
	}
	/// gets this splitters bit index (logical) from graphical split and logical bit index of a wire connected to that graphical split
	/// Returns: Splitter bit index
	pub fn get_bit_index_from_pin_i_and_wire_bit_index(&self, pin_i: u16, wire_bit_index: u16) -> u16 {
		assert!(pin_i as usize <= self.splits.len());
		assert!(wire_bit_index < self.bit_width);
		if pin_i == 0 {
			return wire_bit_index;
		}
		else {
			let mut prev_bits_count: u16 = 0;
			for (curr_split_index, split) in self.splits.iter().enumerate() {
				if curr_split_index as u16 == pin_i - 1 {
					return wire_bit_index + prev_bits_count;
				}
				prev_bits_count += split.0;
			}
		}
		panic!("Splitter::get_bit_index_from_split_and_wire_index() given too large split index")
		// That one time in summer 2025 when i biked to gardner, along the kennebec, to brunswick (got drinks at the hannies by the train station), along route 1, the co-op in damariscotta (I had the kleen kanteen water bottle with me, so it ended up back to where it was bought over a decade ago), to the DRA (super nostalgic), then my bike broke and
		// i waited in that random guy's garage and him and his friend were both ultra runners. I then got picked up by my dad. I got just over 100 miles that day.
		// It was misty and cold that whole day and rained when i was on the bath bridge
		// I miss that.
	}
	/// the bit index of a wire connected to a split connection
	pub fn get_wire_bit_index_from_pin_i_and_bit_index(&self, pin_i: u16, splitter_bit_index: u16) -> u16 {
		if pin_i == 0 {
			return splitter_bit_index;
		}
		else {
			let mut prev_bits_count: u16 = 0;
			for (curr_split_index, split) in self.splits.iter().enumerate() {
				if curr_split_index as u16 == pin_i - 1 {
					return splitter_bit_index - prev_bits_count;
				}
				prev_bits_count += split.0;
			}
		}
		panic!("Splitter::get_bit_index_from_split_and_wire_index() given too large split index")
	}
	pub fn pin_pos_local(pin_i: u16) -> IntV2 {
		if pin_i == 0 {
			IntV2(-2, -1)
		}
		else {
			IntV2(2, pin_i as i32)
		}
	}
	/// Point must be relative to this splitter
	pub fn is_connection_point(&self, point: IntV2) -> Option<(Vec<Option<u64>>, Option<Rc<RefCell<HashSet<WireConnection>>>>)> {
		if point == IntV2(-2, -1) {
			return Some((
				(0..self.bit_width).into_iter().map(|_| None).collect(),
				match &self.base_connections_opt {
					Some(conns_rc) => Some(Rc::clone(conns_rc)),
					None => None
				}
			));
		}
		else {
			for (split_i, split) in self.splits.iter().enumerate() {
				let split_pos = IntV2(2, split_i as i32 + 1);
				if split_pos == point {
					return Some((
						(0..self.bit_width).into_iter().map(|_| None).collect(),
						match &split.1 {
							Some(conns_rc) => Some(Rc::clone(conns_rc)),
							None => None
						}
					));
				}
			}
		}
		None
	}
	pub fn set_pin_wire_conns(&mut self, pin_i: u16, conns: &Option<Rc<RefCell<HashSet<WireConnection>>>>) {
		let new_conns = match conns {
			Some(borrowed_rc) => Some(Rc::clone(borrowed_rc)),
			None => None
		};
		if pin_i == 0 {
			self.base_connections_opt = new_conns;
		}
		else {
			self.splits[pin_i as usize - 1].1 = new_conns;
		}
	}
	/// From Gemini
	/// Given a logical bit index for the whole splitter, find which split pin it
	/// corresponds to and what the local bit index on a wire connected to that pin would be.
	/// Returns `(pin_i, wire_bit_index)`. `pin_i` is the 1-based graphical pin ID.
	pub fn get_pin_and_wire_bit_from_splitter_bit(&self, splitter_bit_index: u16) -> Option<(u16, u16)> {
		if splitter_bit_index >= self.bit_width {
			return None;
		}

		let mut prev_bits_count: u16 = 0;
		for (split_i, split) in self.splits.iter().enumerate() {
			let pin_i = split_i as u16 + 1;
			let split_bit_width = split.0;
			
			// Check if the target bit falls within the range of the current split
			if splitter_bit_index >= prev_bits_count && splitter_bit_index < prev_bits_count + split_bit_width {
				// This is the correct split pin.
				let wire_bit_index = splitter_bit_index - prev_bits_count;
				return Some((pin_i, wire_bit_index));
			}
			prev_bits_count += split_bit_width;
		}
		None// Should not be reached if splitter bit widths sum up correctly
	}
	fn calculate_local_bb(split_len: usize) -> (V2, V2) {
		(V2::new(-2.0, -1.0), V2::new(2.0, split_len as f32))
	}
}

impl GraphicSelectableItem for Splitter {
	fn draw<'a>(&self, draw_parent: &Box<dyn DrawInterface>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		draw.draw_polyline(
			vec![
				V2::new(-2.0, -1.0),
				V2::new(-1.0, -1.0),
				V2::new(0.0, 0.0),
				V2::new(0.0, self.splits.len() as f32 - 1.0)
			],
			draw.styles().color_foreground
		);
		let mut beginning_index: u16 = 0;
		for (i_usize, split) in self.splits.iter().enumerate() {
			let i_f32 = i_usize as f32;
			draw.draw_polyline(
				vec![
					V2::new(0.0, i_f32),
					V2::new(1.0, i_f32 + 1.0),
					V2::new(2.0, i_f32 + 1.0)
				],
				draw.styles().color_foreground
			);
			let text: String = match split.0 == 1 {
				true => beginning_index.to_string(),
				false => format!("{}:{}", beginning_index, beginning_index + split.0 - 1)
			};
			draw.text(&text, V2::new(2.0, i_f32 + 0.5), GenericAlign2::CENTER_CENTER, draw.styles().text_color, 0.8, !draw.get_draw_data().direction.is_horizontal());
			beginning_index += split.0;
		}
	}
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	/*fn is_connected_to_net(&self, net_id: u64) -> bool {
		for split in &self.splits {
			if let Some(split) = &split.1 {
				for net in &split.1 {
					if *net == net_id {
						return true;
					}
				}
			}
		}
		return false;
	}*/
	#[cfg(feature = "using_egui")]
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::BitWidth(self.bit_width),
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::Direction(self.ui_data.direction),
			SelectProperty::SplitterSplits(self.splits.iter().map(|t| t.0).collect())
		]
	}
	#[cfg(feature = "using_egui")]
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::BitWidth(bit_width) => {
				let diff = bit_width as i16 - (self.bit_width as i16);
				self.bit_width = bit_width;
				if diff > 0 {
					for _ in 0..diff {
						self.splits.push((0, None));
					}
				}
				if diff < 0 {
					let mut removed_count: i16 = 0;
					let mut curr_split_index = self.splits.len() - 1;
					// Haley & Ethan </3
					while removed_count < (-diff) {
						if self.splits[curr_split_index].0 == 1 {
							self.splits.pop();
							curr_split_index -= 1;
						}
						else {
							self.splits[curr_split_index].0 -= 1;
						}
						removed_count += 1;
					}
				}
			},
			SelectProperty::PositionX(x) => {
				self.ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.ui_data.position.1 = y;
			},
			SelectProperty::Direction(direction) => {
				self.ui_data.direction = direction;
			},
			SelectProperty::SplitterSplits(new_splits) => {
				let diff = new_splits.len() as isize - (self.splits.len() as isize);
				if diff > 0 {
					for _ in 0..diff {
						self.splits.push((1, None));
					}
				}
				if diff < 0 {
					for _ in 0..(-diff) {
						self.splits.pop();
					}
				}
				// Set sizes
				let mut bw: u16 = 0;
				for (i, s) in new_splits.iter().enumerate() {
					self.splits[i].0 = *s;
					bw += *s;
				}
				self.bit_width = bw;
				self.ui_data.local_bb = Self::calculate_local_bb(new_splits.len());
			}
			_ => {}
		}
	}
	#[cfg(feature = "using_egui")]
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Splitter(self.save())
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SplitterSave {
	pos: IntV2,
	dir: FourWayDir,
	bit_width: u16,
	split_sizes: Vec<u16>
}