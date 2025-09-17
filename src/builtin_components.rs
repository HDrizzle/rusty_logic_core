use crate::{prelude::*, simulator::{graphic_pin_config_from_single_pins, AncestryStack}};
use eframe::egui::Ui;
use common_macros::hash_map;
use std::{collections::HashMap, cell::RefCell, rc::Rc};
use serde::{Deserialize, Serialize};

/// For the component search popup
pub fn list_all_basic_components() -> Vec<EnumAllLogicDevices> {
	vec![
		GateAnd::new().save().unwrap(),
		GateNand::new().save().unwrap(),
		GateNot::new().save().unwrap(),
		GateOr::new().save().unwrap(),
		GateNor::new().save().unwrap(),
		GateXor::new().save().unwrap(),
		GateXnor::new().save().unwrap(),
		ClockSymbol::new().save().unwrap(),
		FixedSource::new().save().unwrap(),
		EncoderOrDecoder::new().save().unwrap(),
		Memory::new().save().unwrap(),
		TriStateBuffer::new().save().unwrap(),
		Adder::new().save().unwrap()
	]
}

/// For dealing with the geometry of a lot of pins
#[derive(Debug)]
struct BlockLayoutHelper {
	/// Vec<(
	/// 	Block side/pin direction,
	/// 	Margin (for other groups on same side, not edge),
	/// 	Number of pins,
	/// 	Name (before numbering if more than 1 pin),
	/// 	Whether to group them all into a multi-bit-width pin,
	/// 	Whether to list pins in order of whatever axis they are along, false means backwards,
	/// 	Starting position, ALONG SIDE GOING CCW, NOT ALONG AXIS
	/// )>
	pin_groups: Vec<(
		FourWayDir,
		u32,
		u16,
		String,
		bool,
		bool,
		i32
	)>,
	/// Logical pin ID for given group name and index
	pub group_logical_pins: HashMap<(String, u16), u64>,
	/// Box to be drawn, pins will be 1 unit away from edges
	bb: (IntV2, IntV2),
	/// Map group name to index of `self.pin_groups`
	names_to_pin_groups: HashMap<String, usize>
}

impl BlockLayoutHelper {
	/// Vec<(
	/// 	Block side/pin direction,
	/// 	Margin (for other groups on same side, not edge),
	/// 	Number of pins,
	/// 	Name (before numbering if more than 1 pin)
	/// )>
	pub fn load(
		layout_save: BusLayoutSave,
		pin_groups_static: Vec<(
			FourWayDir,
			u32,
			u16,
			String
		)>,
		min_size_v: IntV2
	) -> Self {
		// Build pin groups from static config and save state
		// When I wrote this only me and god knew how it worked. Now only god knows.
		// Vec<(Pin dir, Margin, Bit width, Name, Group, List in order)>
		let pin_groups: Vec<(FourWayDir, u32, u16, String, bool, bool)> = pin_groups_static.into_iter().enumerate().map(|(i, t)| {
			let (group, forward) = if i < layout_save.0.len() {
				layout_save.0[i]
			}
			else {
				(true, true)
			};
			(t.0, t.1, t.2, t.3, group, forward)
		}).collect();
		// Split pin groups by side/direction
		let mut current_logic_pin_id: u64 = 0;
		let mut group_logical_pins = HashMap::<(String, u16), u64>::new();
		// HashMap<Pin dir, (Margin, Bit width, Name, Group, List in order)>
		let mut groups_per_side = HashMap::<FourWayDir, Vec<(u32, u16, String, bool, bool)>>::new();
		for (i, group) in pin_groups.into_iter().enumerate() {
			let group_wo_dir = (group.1, group.2, group.3.clone(), group.4, group.5);
			// If this is is the first pin group in its direction, create new hashmap entry, otherwise add it to the existing hashmap entry
			if let Some(v) = groups_per_side.get_mut(&group.0) {
				v.push(group_wo_dir);
			}
			else {
				groups_per_side.insert(group.0, vec![group_wo_dir]);
			}
			// Loop bit width
			for group_i in 0..group.2 {
				group_logical_pins.insert((group.3.clone(), group_i), current_logic_pin_id);
				current_logic_pin_id += 1;
			}
		}
		// Get pin group starting positions
		let mut groups_with_start_positions = Vec::<(FourWayDir, u32, u16, String, bool, bool, i32)>::new();
		let mut side_sizes = HashMap::<FourWayDir, i32>::new();
		let mut names_to_pin_groups: HashMap<String, usize> = HashMap::new();
		for (side, side_groups) in groups_per_side {
			let mut size: i32 = 0;
			let mut current_margin: i32 = 1;
			for (i, group) in side_groups.iter().enumerate() {
				// Set margin if bigger then previous
				if group.0 as i32 > current_margin {
					current_margin = group.0 as i32;
				}
				/*if i == side_groups.len() - 1 {
					current_margin = 1;
				}*/
				// Add margin
				size += current_margin;
				// Group name
				let group_name = group.2.clone();
				if names_to_pin_groups.contains_key(&group_name) {
					panic!("Layout pin group name \"{}\" used at least twice", &group_name);
				}
				names_to_pin_groups.insert(group_name, groups_with_start_positions.len());
				// Record group's starting position
				groups_with_start_positions.push((side, group.0, group.1, group.2.clone(), group.3, group.4, size));
				// Add group width (n-1), only if expanded
				if !group.3 {
					size += group.1 as i32 - 1;
				}
				// Set margin
				current_margin = group.0 as i32;
			}
			size += 1;// Edge margin
			side_sizes.insert(side, size);
		}
		// Find width & height, adjust all start positions
		let mut start_corner = Vec::<i32>::new();// Size 2
		let mut rectified_size = Vec::<i32>::new();// Size 2
		for (i, (dir0, dir1)) in vec![(FourWayDir::S, FourWayDir::N), (FourWayDir::W, FourWayDir::E)].into_iter().enumerate() {
			let size0 = side_sizes.get(&dir0).unwrap_or(&1);
			let size1 = side_sizes.get(&dir1).unwrap_or(&1);
			let min_size = if i == 0 {
				min_size_v.0
			}
			else {
				min_size_v.1
			};
			let max_side_size: i32 = if size0 > size1 {
				*size0
			}
			else {
				*size1
			};
			let size: i32 = if max_side_size > min_size {
				max_side_size
			}
			else {
				min_size
			};
			let side_start: i32 = -size/2;
			for side_group in &mut groups_with_start_positions {
				// Only modify group if its facing the right way
				if side_group.0 == dir0 || side_group.0 == dir1 {
					side_group.6 += side_start;
					// Reverse if direction goes against axis
					if side_group.0 == FourWayDir::N || side_group.0 == FourWayDir::W {
						if size % 2 == 1 {
							side_group.6 -= 1
						}
					}
					// Center on side
					if side_group.0 == dir0 {
						let diff = (size - size0) / 2;
						side_group.6 += diff;
					}
					if side_group.0 == dir1 {
						let diff = (size - size1) / 2;
						side_group.6 += diff;
					}
					// Normal reverse
					if side_group.5 {
						side_group.6 += side_group.1 as i32 - 1;
					}
				}
			}
			start_corner.push(side_start);
			rectified_size.push(size);
		}
		Self {
			pin_groups: groups_with_start_positions,
			group_logical_pins,
			bb: (IntV2(start_corner[0], start_corner[1]), IntV2(start_corner[0]+rectified_size[0], start_corner[1]+rectified_size[1])),
			names_to_pin_groups
		}
	}
	pub fn pin_config(&self) -> HashMap<u64, (IntV2, FourWayDir, f32, String, bool, Vec<u64>)> {
		let mut out = HashMap::<u64, (IntV2, FourWayDir, f32, String, bool, Vec<u64>)>::new();
		let mut graphic_pin_id: u64 = 0;
		for group in &self.pin_groups {
			let first_pin_pos = group.0.rotate_intv2(IntV2(self.get_bb_half_width(group.0) + 1, group.6));// + 1 because pin starts 1 away from bounding box
			if group.4 {// Group to single graphic pin
				out.insert(graphic_pin_id, (
					first_pin_pos,
					group.0,
					1.0,
					group.3.clone(),
					true,
					(0..group.2).into_iter().map(|bit_i| -> u64 {*self.group_logical_pins.get(&(group.3.clone(), bit_i)).unwrap()}).into_iter().collect()
				));
				graphic_pin_id += group.2 as u64 + 1;
			}
			else {
				for group_i in 0..group.2 {
					out.insert(graphic_pin_id, (
						first_pin_pos + group.0.rotate_intv2(IntV2(0, group_i as i32)),
						group.0,
						1.0,
						format!("{}{}", &group.3,group_i),
						true,
						vec![*self.group_logical_pins.get(&(group.3.clone(), group_i)).unwrap()]
					));
					graphic_pin_id += 1;
				}
			}
		}
		out
	}
	fn get_bb_half_width(&self, dir: FourWayDir) -> i32 {
		match dir {
			FourWayDir::E => self.bb.1.0,
			FourWayDir::N => self.bb.1.1,
			FourWayDir::W => -self.bb.0.0,
			FourWayDir::S => -self.bb.0.1
		}
	}
	fn get_properties(&self) -> Vec<SelectProperty> {
		let mut out = Vec::new();
		for group in &self.pin_groups {
			if group.2 > 1 {
				out.push(SelectProperty::BusLayout(group.3.clone(), group.4, group.5));
			}
		}
		out
	}
	fn set_property(&mut self, property: SelectProperty) {
		if let SelectProperty::BusLayout(group_name, single_pin, forward) = property {
			if let Some(group_i) = self.names_to_pin_groups.get(&group_name) {
				self.pin_groups[*group_i].4 = single_pin;
				self.pin_groups[*group_i].5 = forward;
			}
		}
	}
	pub fn save(&self) -> BusLayoutSave {
		BusLayoutSave(self.pin_groups.iter().map(|t| (t.4, t.5)).collect())
	}
	pub fn get_bb_float(&self) -> (V2, V2) {
		(self.bb.0.to_v2(), self.bb.1.to_v2())
	}
}

/// Vec<(Whether to group (use a single graphic pin), Whether to list pins in order along whatever axis their side is on)>
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BusLayoutSave(Vec<(bool, bool)>);

#[derive(Debug, Clone)]
pub struct GateAnd(LogicDeviceGeneric);

impl GateAnd {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false, vec![0]),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false, vec![1]),
				2 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![2]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for GateAnd {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		self.set_pin_internal_state_panic(2, (self.get_pin_state_panic(0).to_bool() && self.get_pin_state_panic(1).to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateAnd(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(0.0, -2.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(0.0, 2.0)
		], draw.styles.color_foreground);
		draw.draw_arc(V2::zeros(), 2.0, -90.0, 90.0, draw.styles.color_foreground);
	}
}

#[derive(Debug, Clone)]
pub struct GateNand(LogicDeviceGeneric);

impl GateNand {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false, vec![0]),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false, vec![1]),
				2 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![2]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for GateNand {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		let and: bool = self.get_pin_state_panic(0).to_bool() && self.get_pin_state_panic(1).to_bool();
		self.set_pin_internal_state_panic(2, (!and).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateNand(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(0.0, -2.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(0.0, 2.0)
		], draw.styles.color_foreground);
		draw.draw_arc(V2::zeros(), 2.0, -90.0, 90.0, draw.styles.color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles.color_foreground);
	}
}

#[derive(Debug, Clone)]
pub struct GateNot(LogicDeviceGeneric);

impl GateNot {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, 0), FourWayDir::W, 1.0, "a".to_owned(), false, vec![0]),
				1 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![1]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for GateNot {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		self.set_pin_internal_state_panic(1, (!self.get_pin_state_panic(0).to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateNot(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(2.0, 0.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(2.0, 0.0)
		], draw.styles.color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles.color_foreground);
	}
}

#[derive(Debug, Clone)]
pub struct GateOr(LogicDeviceGeneric);

impl GateOr {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false, vec![0]),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false, vec![1]),
				2 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![2]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for GateOr {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		self.set_pin_internal_state_panic(2, (self.get_pin_state_panic(0).to_bool() || self.get_pin_state_panic(1).to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateOr(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles.color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles.color_foreground);
	}
}

#[derive(Debug, Clone)]
pub struct GateNor(LogicDeviceGeneric);

impl GateNor {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false, vec![0]),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false, vec![1]),
				2 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![2]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for GateNor {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		self.set_pin_internal_state_panic(2, (!(self.get_pin_state_panic(0).to_bool() || self.get_pin_state_panic(1).to_bool())).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateNor(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles.color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles.color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles.color_foreground);
	}
}

#[derive(Debug, Clone)]
pub struct GateXor(LogicDeviceGeneric);

impl GateXor {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 0.7, "a".to_owned(), false, vec![0]),
				1 => (IntV2(-3, 1), FourWayDir::W, 0.7, "b".to_owned(), false, vec![1]),
				2 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![2]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for GateXor {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		self.set_pin_internal_state_panic(2, (self.get_pin_state_panic(0).to_bool() != self.get_pin_state_panic(1).to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateXor(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles.color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-8.1, 0.0), 6.0, -19.5, 19.5, draw.styles.color_foreground);
	}
}

#[derive(Debug, Clone)]
pub struct GateXnor(LogicDeviceGeneric);

impl GateXnor {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 0.7, "a".to_owned(), false, vec![0]),
				1 => (IntV2(-3, 1), FourWayDir::W, 0.7, "b".to_owned(), false, vec![1]),
				2 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![2]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for GateXnor {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		self.set_pin_internal_state_panic(2, (self.get_pin_state_panic(0).to_bool() == self.get_pin_state_panic(1).to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateXnor(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles.color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles.color_foreground);
		draw.draw_arc(V2::new(-8.1, 0.0), 6.0, -19.5, 19.5, draw.styles.color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles.color_foreground);
	}
}

#[derive(Debug, Clone)]
pub struct ClockSymbol(LogicDeviceGeneric);

impl ClockSymbol {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(0, 0), FourWayDir::W, 1.0, "CLK".to_owned(), false, vec![0])
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for ClockSymbol {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, clock_state: bool, first_propagation: bool) {
		if first_propagation {
			self.set_pin_internal_state_panic(0, clock_state.into());
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::Clock(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(
			vec![
				V2::new(1.1, -0.9),
				V2::new(1.1, 0.9),
				V2::new(2.9, 0.9),
				V2::new(2.9, -0.9),
				V2::new(1.1, -0.9)
			],
			draw.styles.color_from_logic_state(self.get_pin_state_panic(0))
		);
		let clk_scale = 0.7;
		draw.draw_polyline(
			vec![
				V2::new(-clk_scale, 0.0),
				V2::new(-clk_scale, clk_scale),
				V2::new(0.0, clk_scale),
				V2::new(0.0, -clk_scale),
				V2::new(clk_scale, -clk_scale),
				V2::new(clk_scale, 0.0)
			].iter().map(|p| self.0.ui_data.direction.rotate_v2_reverse(*p) + V2::new(2.0, 0.0)).collect(),
			draw.styles.color_foreground
		);
	}
}

#[derive(Debug)]
pub struct FixedSource {
	generic: LogicDeviceGeneric,
	state: bool
}

/// "Power" or "GND" symbol
impl FixedSource {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), false)
	}
	pub fn from_save(save: LogicDeviceSave, state: bool) -> Self {
		let (direction, name, bb): (FourWayDir, String, (V2, V2)) = match state {
			true => (FourWayDir::S, "+V".to_string(), (V2::new(-1.0, 2.0), V2::new(1.0, 0.0))),
			false => (FourWayDir::N, "GND".to_string(), (V2::new(-1.0, 0.0), V2::new(1.0, -2.0)))
		};
		Self {
			generic: LogicDeviceGeneric::load(
				save,
				hash_map!(0 => (IntV2(0, 0), direction, 1.0, name, false, vec![0])),
				bb,
				false,
				false
			),
			state
		}
	}
}

impl LogicDevice for FixedSource {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		self.set_pin_internal_state_panic(0, self.state.into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::FixedSource(self.generic.save(), self.state))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		match self.state {
			true => {
				draw.draw_polyline(vec![V2::new(0.0, 2.0), V2::new(1.0, 1.0), V2::new(-1.0, 1.0), V2::new(0.0, 2.0)], draw.styles.color_foreground);
			},
			false => {
				draw.draw_polyline(vec![V2::new(1.0, -1.0), V2::new(-1.0, -1.0)], draw.styles.color_foreground);
				draw.draw_polyline(vec![V2::new(0.75, -1.5), V2::new(-0.75, -1.5)], draw.styles.color_foreground);
				draw.draw_polyline(vec![V2::new(0.5, -2.0), V2::new(-0.5, -2.0)], draw.styles.color_foreground);
			}
		}
	}
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		vec![SelectProperty::FixedSourceState(self.state)]
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::FixedSourceState(state) = property {
			self.state = state;
		}
	}
}

/// Returns: (Address X start, in/out Y start, BB)
fn encoder_decoder_geometry(addr_size: u8) -> (i32, i32, (V2, V2)) {
	let (addr_x_start, fanout_y_start) = (-((addr_size / 2 + 1) as i32), -2_i32.pow((addr_size - 1) as u32));
	let fanout_size = 2_i32.pow(addr_size as u32);
	(
		addr_x_start,
		fanout_y_start,
		(
			IntV2(addr_x_start - 1, fanout_y_start - 1).to_v2(),
			IntV2(addr_x_start + (addr_size as i32) + 1, fanout_y_start + fanout_size).to_v2()
		)
	)
}

/// Encoder/Decoder, these have very similar layouts and stuff so easier to combine them to avoid repeating code
/// Maximum address size is 8 so maximum input/output count of 256
/// Pins: 0 -> Output/Enable, 1..n -> Address, n+1..n+1+2^n -> Inputs/Outputs
/// Width: ctrl size + 2, height: 2^(ctrl size) + 2
#[derive(Debug, Clone)]
pub struct EncoderOrDecoder {
	pub generic: LogicDeviceGeneric,
	pub addr_size: u8,
	pub is_encoder: bool
}

impl EncoderOrDecoder {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), 3, true)
	}
	pub fn from_save(save: LogicDeviceSave, addr_size: u8, is_encoder: bool) -> Self {
		assert!(addr_size > 0);
		assert!(addr_size <= 8);
		let (addr_x_start, fanout_y_start, bb) = encoder_decoder_geometry(addr_size);
		let fanout_size = 2_i32.pow(addr_size as u32);
		let addr_size_i32 = addr_size as i32;
		// Generate pins
		let mut pin_config = HashMap::<u64, (IntV2, FourWayDir, f32, String, bool)>::new();
		// Input/Enable
		pin_config.insert(0, (IntV2(addr_x_start - 2, 0), FourWayDir::W, 1.0, "Enable".to_owned(), true));
		// Addresses
		for a_u8 in 0..addr_size {
			let a = a_u8 as i32;
			pin_config.insert((a_u8+1) as u64, (IntV2(addr_x_start + a, fanout_y_start - 2), FourWayDir::S, 1.0, format!("A{}", a_u8), true));
		}
		// Outputs
		for d in 0..fanout_size {
			pin_config.insert((1+addr_size_i32+d) as u64, (IntV2(addr_x_start + addr_size_i32 + 2, fanout_y_start + d), FourWayDir::E, 1.0, format!("D{}", d), true));
		}
		Self {
			generic: LogicDeviceGeneric::load(
				save,
				graphic_pin_config_from_single_pins(pin_config),
				bb,
				false,
				false
			),
			addr_size,
			is_encoder
		}
	}
	fn get_fanout_pin_id(&self, address: u8) -> u64 {
		assert!(2_u16.pow(self.addr_size as u32) > address as u16);
		1 + (self.addr_size as u64) + (address as u64)
	}
	fn get_address(&self) -> u8 {
		let mut out: u8 = 0;
		for a in 0..self.addr_size {
			if self.get_pin_state_panic(a as u64 + 1).to_bool() {
				out += 2_u8.pow(a as u32);
			}
		}
		out
	}
}

impl LogicDevice for EncoderOrDecoder {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		if self.is_encoder {
			let input = self.get_pin_state_panic(self.get_fanout_pin_id(self.get_address())).to_bool();
			self.set_pin_internal_state_panic(0, input.into());
		}
		else {
			let addr = self.get_address();
			for d_16 in 0..2_u16.pow(self.addr_size as u32) {
				let d = d_16 as u8;
				self.set_pin_internal_state_panic(
					self.get_fanout_pin_id(d),
					match d == addr {
						true => self.get_pin_state_panic(0).to_bool(),
						false => false
					}.into()
				);
			}
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::EncoderOrDecoder(self.generic.save(), self.addr_size, self.is_encoder))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(bb_to_polyline(self.generic.ui_data.local_bb), draw.styles.color_foreground);
	}
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::AddressWidth(self.addr_size, 8),
			SelectProperty::EncoderOrDecoder(self.is_encoder)
		]
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::AddressWidth(new_addr_size, _) = property {
			*self = Self::from_save(self.generic.save(), new_addr_size, self.is_encoder);
		}
		if let SelectProperty::EncoderOrDecoder(new_encoder_state) = property {
			*self = Self::from_save(self.generic.save(), self.addr_size, new_encoder_state);
		}
	}
}

/// Memory can be volatile (RAM) or nonvolatile (Flash)
/// Maximum address size is 16 for 65,536 bytes
/// CE - chip enable - active high, RE - enables outputs, WE - write enable - level triggered
/// Pin layout: data on top left, controls (CE, WE, RE) on bottom left, address on right
/// Pin ID assignments:
///   0 - CE
///   1 - WE
///   2 - RE
///   3 - D0
///   ...
///  10 - D7
///  11 - A0
/// n+10 - A[n-1]
#[derive(Debug)]
pub struct Memory {
	pub generic: LogicDeviceGeneric,
	pub addr_size: u8,
	pub data: Vec<u8>,
	pub nonvolatile: bool,
	ui_csv_paste_string: Rc<RefCell<String>>,
	ui_error_opt: Option<String>
}

impl Memory {
	pub const HALF_WIDTH: i32 = 5;
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), 8, None)
	}
	/// Returns: (BB, pin config)
	fn compute_geometry_and_pins(addr_size: u8) -> ((IntV2, IntV2), HashMap<u64, (IntV2, FourWayDir, f32, String, bool)>) {
		let height: i32 = match addr_size < 12 {
			true => 14,
			false => addr_size as i32 + 2
		};
		let data_y_start = -2;
		let addr_y_start = -6;
		let bb_int = (IntV2(-Self::HALF_WIDTH, -height/2), IntV2(Self::HALF_WIDTH, height/2));
		let mut pin_config = HashMap::<u64, (IntV2, FourWayDir, f32, String, bool)>::new();
		// Pin config
		// Controls
		pin_config.insert(0, (IntV2(-Self::HALF_WIDTH - 1, data_y_start - 2), FourWayDir::W, 1.0, "CE".to_owned(), true));
		pin_config.insert(1, (IntV2(-Self::HALF_WIDTH - 1, data_y_start - 3), FourWayDir::W, 1.0, "WE".to_owned(), true));
		pin_config.insert(2, (IntV2(-Self::HALF_WIDTH - 1, data_y_start - 4), FourWayDir::W, 1.0, "RE".to_owned(), true));
		// Data
		for d in 0..8_i32 {
			pin_config.insert(d as u64 + 3, (IntV2(-Self::HALF_WIDTH - 1, data_y_start + d), FourWayDir::W, 1.0, format!("D{}", d), true));
		}
		// Addresses
		for a in 0..(addr_size as i32) {
			pin_config.insert(a as u64 + 11, (IntV2(Self::HALF_WIDTH + 1, addr_y_start + a), FourWayDir::E, 1.0, format!("A{}", a), true));
		}
		(
			bb_int,
			pin_config
		)
	}
	fn format_data(data_opt: Option<Vec<u8>>, addr_size: u8) -> Vec<u8> {
		let correct_size: usize = 2_usize.pow(addr_size as u32);
		let mut len_unchecked: Vec<u8> = match data_opt {
			Some(data) => data,
			None => Vec::new()
		};
		let size_diff: i32 = len_unchecked.len() as i32 - correct_size as i32;
		if size_diff <= 0 {// Too short or correct
			for _ in 0..-size_diff {
				len_unchecked.push(0);
			}
		}
		else {
			for _ in 0..size_diff {
				len_unchecked.pop();
			}
		}
		len_unchecked
	}
	pub fn from_save(save: LogicDeviceSave, addr_size: u8, data_opt: Option<Vec<u8>>) -> Self {
		let (bb_int, pin_config) = Self::compute_geometry_and_pins(addr_size);
		let nonvolatile = data_opt.is_some();
		Self {
			generic: LogicDeviceGeneric::load(
				save,
				graphic_pin_config_from_single_pins(pin_config),
				(bb_int.0.to_v2(), bb_int.1.to_v2()),
				true,
				false
			),
			addr_size,
			data: Self::format_data(data_opt, addr_size),
			nonvolatile,
			ui_csv_paste_string: Rc::new(RefCell::new(String::new())),
			ui_error_opt: None
		}
	}
	fn get_address(&self) -> u16 {
		let mut out: u16 = 0;
		for a in 0..self.addr_size {
			if self.get_pin_state_panic(a as u64 + 11).to_bool() {
				out += 2_u16.pow(a as u32);
			}
		}
		out
	}
}

impl LogicDevice for Memory {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		let ce: bool = self.get_pin_state_panic(0).to_bool();
		let we: bool = self.get_pin_state_panic(1).to_bool();
		let re: bool = self.get_pin_state_panic(2).to_bool();
		let address = self.get_address() as usize;
		if !re || !ce {// Set all data lines floating
			for i in 3..11_u64 {
				self.set_pin_internal_state_panic(i, LogicState::Floating);
			}
		}
		if ce {
			if re {// Memory read
				let byte: u8 = self.data[address];
				for i in 3..11_u64 {
					self.set_pin_internal_state_panic(i, match (byte >> (i - 3)) & 1 {0 => false, 1 => true, _ => panic!("bruh")}.into());
				}
			}
			else if we {
				let mut new_byte: u8 = 0;
				for i in 3..11_u64 {
					if self.get_pin_state_panic(i).to_bool() {
						new_byte += 2_u8.pow((i - 3) as u32);
					}
				}
				self.data[address] = new_byte;
			}
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::Memory(
			self.generic.save(),
			self.addr_size,
			match self.nonvolatile {
				true => Some(self.data.clone()),
				false => None
			}
		))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(bb_to_polyline(self.generic.ui_data.local_bb), draw.styles.color_foreground);
	}
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::AddressWidth(self.addr_size, 16),
			SelectProperty::MemoryProperties(MemoryPropertiesUI::new(self.nonvolatile, self.ui_csv_paste_string.clone(), self.ui_error_opt.clone()))
		]
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::AddressWidth(new_addr_size, _) = property {
			*self = Self::from_save(self.generic.save(), new_addr_size, Some(self.data.clone()));
		}
		if let SelectProperty::MemoryProperties(props) = property {
			let mut new_err_opt = Option::<String>::None;
			self.nonvolatile = props.nonvolatile;
			if props.erase {
				for i in 0..self.data.len() {
					self.data[i] = 0;
				}
			}
			if props.paste {
				let csv_string = self.ui_csv_paste_string.borrow();
				for (i, number_string) in csv_string.split(",").into_iter().enumerate() {
					match number_string.parse::<u8>() {
						Ok(new_elem) => {
							self.data[i] = new_elem;
						}
						Err(e) => {
							new_err_opt = Some(format!("CSV paste error: {}", e));
							break;
						}
					}
				}
			}
			if let Some(new_err) = new_err_opt {
				self.ui_error_opt = Some(new_err);
			}
			else {
				if props.changed {
					self.ui_error_opt = None;
				}
			}
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemoryPropertiesUI {
	pub nonvolatile: bool,
	pub erase: bool,
	pub csv_paste_string: Rc<RefCell<String>>,
	pub paste: bool,
	pub error_opt: Option<String>,
	pub changed: bool
}

impl MemoryPropertiesUI {
	pub fn new(
		nonvolatile: bool,
		csv_paste_string: Rc<RefCell<String>>,
		error_opt: Option<String>
	) -> Self {
		Self {
			nonvolatile,
			erase: false,
			csv_paste_string,
			paste: false,
			error_opt,
			changed: false
		}
	}
	pub fn show_ui(&mut self, ui: &mut Ui) -> bool {
		ui.vertical(|ui| {
			self.changed |= ui.checkbox(&mut self.nonvolatile, "Nonvolatile").changed();
			ui.horizontal(|ui| {
				ui.label("Paste in CSV");
				ui.text_edit_singleline(&mut *self.csv_paste_string.borrow_mut());
			});
			self.paste = ui.button("Write").clicked();
			ui.separator();
			self.erase = ui.button("Erase").clicked();
			self.changed |= self.paste || self.erase;
			if let Some(error) = &self.error_opt {
				ui.separator();
				ui.colored_label(u8_3_to_color32([255, 0, 0]), error);
			} 
		});
		self.changed
	}
}

#[derive(Debug)]
pub struct TriStateBuffer(LogicDeviceGeneric);

impl TriStateBuffer {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, 0), FourWayDir::W, 1.0, "a".to_owned(), false, vec![0]),
				1 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![1]),
				2 => (IntV2(0, -2), FourWayDir::S, 1.0, "En".to_owned(), false, vec![2]),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			false,
			false
		))
	}
}

impl LogicDevice for TriStateBuffer {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.0
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.0
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _: u64, _: bool, _: bool) {
		if self.get_pin_state_panic(2).to_bool() {
			self.set_pin_internal_state_panic(1, self.get_pin_state_panic(0).to_bool().into());
		}
		else {
			self.set_pin_internal_state_panic(1, LogicState::Floating);
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::TriStateBuffer(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		draw.draw_polyline(vec![
			V2::new(2.0, 0.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(2.0, 0.0)
		], draw.styles.color_foreground);
	}
}

/// Parameterized Adder
#[derive(Debug)]
pub struct Adder {
	generic: LogicDeviceGeneric,
	/// 0 to 256
	bits: u16,
	layout: BlockLayoutHelper
}

impl Adder {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), BusLayoutSave::default(), 8)
	}
	pub fn from_save(save: LogicDeviceSave, layout_save: BusLayoutSave, bits: u16) -> Self {
		let layout = BlockLayoutHelper::load(
			layout_save,
			vec![
				(FourWayDir::W, 1, bits, "A".to_owned()),
				(FourWayDir::W, 1, bits, "B".to_owned()),
				(FourWayDir::E, 1, bits, "C".to_owned()),
				(FourWayDir::N, 1, 1, "Cin".to_owned()),
				(FourWayDir::S, 1, 1, "Cout".to_owned()),
			],
			IntV2(4, 4)
		);
		Self{
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				false,
				false
			),
			bits,
			layout
		}
	}
}

impl LogicDevice for Adder {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _self_component_id: u64, _clock_state: bool, _first_propagation_step: bool) {
		let mut carry = self.get_pin_state_panic(self.layout.group_logical_pins[&("Cin".to_owned(), 0)]).to_bool();
		for i in 0..self.bits {
			let a = self.get_pin_state_panic(self.layout.group_logical_pins[&("A".to_owned(), i)]).to_bool();
			let b = self.get_pin_state_panic(self.layout.group_logical_pins[&("B".to_owned(), i)]).to_bool();
			let sum: bool = (a ^ b) ^ carry;
			self.set_pin_internal_state_panic(self.layout.group_logical_pins[&("C".to_owned(), i)], sum.into());
			carry = (a & b) | ((a ^ b) & carry);
		}
		self.set_pin_internal_state_panic(self.layout.group_logical_pins[&("Cout".to_owned(), 0)], carry.into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::Adder(self.generic.save(), self.layout.save(), self.bits))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		// TODO
	}
	fn get_bit_width(&self) -> Option<u16> {
		Some(self.bits)
	}
	fn set_bit_width(&mut self, bit_width: u16) {
		*self = Self::from_save(self.generic.save(), self.layout.save(), bit_width);
	}
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		self.layout.get_properties()
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		self.layout.set_property(property);
	}
}