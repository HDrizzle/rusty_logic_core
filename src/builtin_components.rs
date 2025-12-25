use crate::{prelude::*, simulator::AncestryStack};
#[cfg(feature = "using_egui")]
use eframe::egui::Ui;
use common_macros::hash_map;
use std::{collections::HashMap, cell::RefCell, rc::Rc};
use serde::{Deserialize, Serialize};
use web_time::Instant;

/// For the component search popup
pub fn list_all_basic_components() -> Vec<EnumAllLogicDevices> {
	vec![
		GateAnd::new().save().unwrap(),
		GateNand::new().save().unwrap(),
		GateNotNew::new().save().unwrap(),
		GateOr::new().save().unwrap(),
		GateNor::new().save().unwrap(),
		GateXor::new().save().unwrap(),
		GateXnor::new().save().unwrap(),
		ClockSymbol::new().save().unwrap(),
		FixedSource::new().save().unwrap(),
		EncoderOrDecoder::new().save().unwrap(),
		Memory::new().save().unwrap(),
		TriStateBuffer::new().save().unwrap(),
		Adder::new().save().unwrap(),
		DLatch::new().save().unwrap(),
		Counter::new().save().unwrap(),
		SRLatch::new().save().unwrap(),
		VectorCRT::new().save().unwrap()
	]
}

/// For dealing with the geometry of a lot of pins
#[derive(Debug, Clone)]
pub struct BlockLayoutHelper {
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
		// Vec<(Pin dir, Margin, Bit width, Name, Group, List in order, Pin groups static order)>
		let pin_groups: Vec<(FourWayDir, u32, u16, String, bool, bool, usize)> = pin_groups_static.into_iter().enumerate().map(|(i, t)| {
			let (group, forward) = if i < layout_save.0.len() {
				layout_save.0[i]
			}
			else {
				(true, true)
			};
			(t.0, t.1, t.2, t.3, group, forward, i)
		}).collect();
		// Split pin groups by side/direction
		let mut current_logic_pin_id: u64 = 0;
		let mut group_logical_pins = HashMap::<(String, u16), u64>::new();
		// HashMap<Pin dir, (Margin, Bit width, Name, Group, Forward)>
		let mut groups_per_side = HashMap::<FourWayDir, Vec<(u32, u16, String, bool, bool, usize)>>::new();
		for group in &pin_groups {
			let group_wo_dir = (group.1, group.2, group.3.clone(), group.4, group.5, group.6);
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
		let mut groups_with_start_positions = Vec::<(FourWayDir, u32, u16, String, bool, bool, i32, usize)>::new();
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
				if i == 0 {
					current_margin = 1;
				}
				// Add margin
				size += current_margin;
				// Group name
				let group_name = group.2.clone();
				if names_to_pin_groups.contains_key(&group_name) {
					panic!("Layout pin group name \"{}\" used at least twice", &group_name);
				}
				names_to_pin_groups.insert(group_name, group.5);
				// Record group's starting position
				groups_with_start_positions.push((side, group.0, group.1, group.2.clone(), group.3, group.4, size, group.5));
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
					if side_group.5 && !side_group.4 {
						side_group.6 += side_group.2 as i32 - 1;
					}
				}
			}
			start_corner.push(side_start);
			rectified_size.push(size);
		}
		// Sort `groups_with_start_positions` by their original `pin_groups_static` index so they are the same order and can be saved correctly
		groups_with_start_positions.sort_by(|t0, t1| t0.7.cmp(&t1.7));
		Self {
			pin_groups: groups_with_start_positions.into_iter().map(|t| (t.0, t.1, t.2, t.3, t.4, t.5, t.6)).collect(),
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
			if group.4 || group.2 == 1 {// Group to single graphic pin
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
					let group_i_possible_reverse: i32 = match group.5 {
						true => -(group_i as i32),
						false => group_i as i32
					};
					out.insert(graphic_pin_id, (
						first_pin_pos + group.0.rotate_intv2(IntV2(0, group_i_possible_reverse)),
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
	#[cfg(feature = "using_egui")]
	fn get_properties(&self) -> Vec<SelectProperty> {
		let mut out = Vec::new();
		for group in &self.pin_groups {
			if group.2 > 1 {
				out.push(SelectProperty::BusLayout(group.3.clone(), group.4, group.5));
			}
		}
		out
	}
	#[cfg(feature = "using_egui")]
	/// Returns: Whether anything changed
	fn set_property(&mut self, property: SelectProperty) -> bool {
		if let SelectProperty::BusLayout(group_name, single_pin, forward) = property {
			if let Some(group_i) = self.names_to_pin_groups.get(&group_name) {
				self.pin_groups[*group_i].4 = single_pin;
				self.pin_groups[*group_i].5 = forward;
			}
			true
		}
		else {
			false
		}
	}
	pub fn save(&self) -> BusLayoutSave {
		BusLayoutSave(self.pin_groups.iter().map(|t| (t.4, t.5)).collect())
	}
	pub fn get_bb_float(&self) -> (V2, V2) {
		(self.bb.0.to_v2(), self.bb.1.to_v2())
	}
	pub fn get_logic_pin_id_panic(&self, group_name: &str, bit_i: u16) -> u64 {
		*self.group_logical_pins.get(&(group_name.to_owned(), bit_i)).unwrap()
	}
	/// Gets binary number from group of pins, panics if the group name doesn't exist
	pub fn get_bus_value_panic(&self, bus_name: &str, logic_pins: &Rc<RefCell<HashMap<u64, RefCell<LogicConnectionPin>>>>) -> (u128, u128) {
		let mut lower: u128 = 0;
		let mut upper: u128 = 0;
		let mut bus_size: u16 = 0;
		let mut group_found = false;
		for group in &self.pin_groups {
			if group.3 == bus_name {
				group_found = true;
				bus_size = group.2;
			}
		}
		if !group_found {
			panic!("Pin group/bus \"{}\" does not exist", bus_name);
		}
		for i in 0..bus_size {
			if logic_pins.borrow().get(&self.get_logic_pin_id_panic(bus_name, i)).unwrap().borrow().state().to_bool() {
				if i < 128 {
					lower += 1 << i;
				}
				else {
					upper += 1 << (i - 128);
				}
			}
		}
		(lower, upper)
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		let styles = draw.styles();
		draw.draw_polyline(vec![
			V2::new(0.0, -2.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(0.0, 2.0)
		], styles.color_foreground);
		draw.draw_arc(V2::zeros(), 2.0, -90.0, 90.0, draw.styles().color_foreground);
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(0.0, -2.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(0.0, 2.0)
		], draw.styles().color_foreground);
		draw.draw_arc(V2::zeros(), 2.0, -90.0, 90.0, draw.styles().color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles().color_foreground);
	}
}

/// Original one, kinda big
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(2.0, 0.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(2.0, 0.0)
		], draw.styles().color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles().color_foreground);
	}
}

/// Newer NOT gate, smaller
#[derive(Debug, Clone)]
pub struct GateNotNew(LogicDeviceGeneric);

impl GateNotNew {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, 0), FourWayDir::W, 1.0, "a".to_owned(), false, vec![0]),
				1 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false, vec![1]),
			),
			(V2::new(-2.0, -1.5), V2::new(1.0, 1.5)),
			false,
			false
		))
	}
}

impl LogicDevice for GateNotNew {
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
		Ok(EnumAllLogicDevices::GateNotNew(self.0.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(1.0, 0.0),
			V2::new(-2.0, -1.5),
			V2::new(-2.0, 1.5),
			V2::new(1.0, 0.0)
		], draw.styles().color_foreground);
		draw.draw_circle(V2::new(1.5, 0.0), 0.5, draw.styles().color_foreground);
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles().color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles().color_foreground);
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles().color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles().color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles().color_foreground);
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles().color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-8.1, 0.0), 6.0, -19.5, 19.5, draw.styles().color_foreground);
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(-1.5, -2.0),
			V2::new(-2.1, -2.0)
		], draw.styles().color_foreground);
		draw.draw_polyline(vec![
			V2::new(-1.5, 2.0),
			V2::new(-2.1, 2.0)
		], draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, 2.0), 4.0, -90.0, -30.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-1.5, -2.0), 4.0, 30.0, 90.0, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-7.8, 0.0), 6.0, -19.5, 19.5, draw.styles().color_foreground);
		draw.draw_arc(V2::new(-8.1, 0.0), 6.0, -19.5, 19.5, draw.styles().color_foreground);
		draw.draw_circle(V2::new(2.5, 0.0), 0.5, draw.styles().color_foreground);
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
			(V2::new(1.0, -1.0), V2::new(3.0, 1.0)),
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(
			vec![
				V2::new(1.1, -0.9),
				V2::new(1.1, 0.9),
				V2::new(2.9, 0.9),
				V2::new(2.9, -0.9),
				V2::new(1.1, -0.9)
			],
			draw.styles().color_from_logic_state(self.get_pin_state_panic(0))
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
			draw.styles().color_foreground
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		match self.state {
			true => {
				draw.draw_polyline(vec![V2::new(0.0, 2.0), V2::new(1.0, 1.0), V2::new(-1.0, 1.0), V2::new(0.0, 2.0)], draw.styles().color_foreground);
			},
			false => {
				draw.draw_polyline(vec![V2::new(1.0, -1.0), V2::new(-1.0, -1.0)], draw.styles().color_foreground);
				draw.draw_polyline(vec![V2::new(0.625, -1.5), V2::new(-0.625, -1.5)], draw.styles().color_foreground);
				draw.draw_polyline(vec![V2::new(0.25, -2.0), V2::new(-0.25, -2.0)], draw.styles().color_foreground);
			}
		}
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		vec![SelectProperty::FixedSourceState(self.state)]
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::FixedSourceState(state) = property {
			self.state = state;
			*self = Self::from_save(self.generic.save(), state);
		}
	}
}

/// Encoder/Decoder, these have very similar layouts and stuff so easier to combine them to avoid repeating code
/// Maximum address size is 8 so maximum input/output count of 256
/// Pins: 0 -> Output/Enable, 1..n -> Address, n+1..n+1+2^n -> Inputs/Outputs
/// Width: ctrl size + 2, height: 2^(ctrl size) + 2
#[derive(Debug, Clone)]
pub struct EncoderOrDecoder {
	pub generic: LogicDeviceGeneric,
	pub addr_size: u8,
	pub is_encoder: bool,
	pub layout: BlockLayoutHelper
}

impl EncoderOrDecoder {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), 3, true, BusLayoutSave::default())
	}
	pub fn from_save(save: LogicDeviceSave, addr_size: u8, is_encoder: bool, layout_save: BusLayoutSave) -> Self {
		assert!(addr_size > 0);
		assert!(addr_size <= 8);
		let fanout_size = 2_i32.pow(addr_size as u32);
		let layout = BlockLayoutHelper::load(
			layout_save,
			vec![
				(FourWayDir::S, 3, addr_size as u16, "A".to_owned()),
				(FourWayDir::E, 2, fanout_size as u16, "D".to_owned()),
				(FourWayDir::W, 1, 1, "Enable".to_owned())
			],
			IntV2(6, 4)
		);
		let mut out = Self {
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				false,
				false
			),
			addr_size,
			is_encoder,
			layout
		};
		// Clear address inputs
		for a in 0..out.addr_size {
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("A", a as u16), LogicState::Floating);
		}
		if is_encoder {// Clear fanout pins
			for d in 0..2_u16.pow(out.addr_size as u32) {
				out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("D", d as u16), LogicState::Floating);
			}
		}
		else {// Clear enable/input
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Enable", 0), LogicState::Floating);
		}
		out
	}
	fn get_address(&self) -> u8 {
		let mut out: u8 = 0;
		for a in 0..self.addr_size {
			if self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("A", a as u16)).to_bool() {
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
			let input = self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("D", self.get_address() as u16)).to_bool();
			self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Enable", 0), input.into());
		}
		else {
			let addr = self.get_address();
			for d_16 in 0..2_u16.pow(self.addr_size as u32) {
				self.set_pin_internal_state_panic(
					self.layout.get_logic_pin_id_panic("D", d_16),
					match d_16 as u8 == addr {
						true => self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("Enable", 0)).to_bool(),
						false => false
					}.into()
				);
			}
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::EncoderOrDecoder(self.generic.save(), self.addr_size, self.is_encoder, self.layout.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(bb_to_polyline(self.generic.ui_data.local_bb), draw.styles().color_foreground);
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		let mut out = vec![
			SelectProperty::AddressWidth(self.addr_size, 8),
			SelectProperty::EncoderOrDecoder(self.is_encoder)
		];
		out.append(&mut self.layout.get_properties());
		out
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::AddressWidth(new_addr_size, _) = property {
			*self = Self::from_save(self.generic.save(), new_addr_size, self.is_encoder, self.layout.save());
		}
		if let SelectProperty::EncoderOrDecoder(new_encoder_state) = property {
			*self = Self::from_save(self.generic.save(), self.addr_size, new_encoder_state, self.layout.save());
		}
		if self.layout.set_property(property) {
			*self = Self::from_save(self.generic.save(), self.addr_size, self.is_encoder, self.layout.save());
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
	ui_error_opt: Option<String>,
	layout: BlockLayoutHelper
}

impl Memory {
	pub const HALF_WIDTH: i32 = 5;
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), 8, None, BusLayoutSave::default())
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
	pub fn from_save(save: LogicDeviceSave, addr_size: u8, data_opt: Option<Vec<u8>>, layout_save: BusLayoutSave) -> Self {
		let nonvolatile = data_opt.is_some();
		let layout = BlockLayoutHelper::load(
			layout_save,
			vec![
				(FourWayDir::E, 2, addr_size as u16, "A".to_owned()),
				(FourWayDir::W, 2, 8, "D".to_owned()),
				(FourWayDir::W, 1, 1, "CE".to_owned()),
				(FourWayDir::W, 1, 1, "WE".to_owned()),
				(FourWayDir::W, 1, 1, "RE".to_owned())
			],
			IntV2(10, 4)
		);
		let mut out = Self {
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				true,
				false
			),
			addr_size,
			data: Self::format_data(data_opt, addr_size),
			nonvolatile,
			ui_csv_paste_string: Rc::new(RefCell::new(String::new())),
			ui_error_opt: None,
			layout
		};
		for a in 0..out.addr_size {
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("A", a as u16), LogicState::Floating);
		}
		for i in 0..8 {
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("D", i as u16), LogicState::Floating);
		}
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("CE", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("WE", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("RE", 0), LogicState::Floating);
		out
	}
	fn get_address(&self) -> u16 {
		let mut out: u16 = 0;
		for a in 0..self.addr_size {
			if self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("A", a as u16)).to_bool() {
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
		let ce: bool = self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("CE", 0)).to_bool();
		let we: bool = self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("WE", 0)).to_bool();
		let re: bool = self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("RE", 0)).to_bool();
		let address = self.get_address() as usize;
		if !re || !ce {// Set all data lines floating
			for bit_i in 0..8_u16 {
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("D", bit_i), LogicState::Floating);
			}
		}
		if ce {
			if re {// Memory read
				let byte: u8 = self.data[address];
				for bit_i in 0..8_u16 {
					self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("D", bit_i), match (byte >> bit_i) & 1 {0 => false, 1 => true, _ => panic!("bruh")}.into());
				}
			}
			else if we {
				let mut new_byte: u8 = 0;
				for bit_i in 0..8_u16 {
					if self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("D", bit_i)).to_bool() {
						new_byte += 2_u8.pow(bit_i as u32);
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
			},
			self.layout.save()
		))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(bb_to_polyline(self.generic.ui_data.local_bb), draw.styles().color_foreground);
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		let mut out = vec![
			SelectProperty::AddressWidth(self.addr_size, 16),
			SelectProperty::MemoryProperties(MemoryPropertiesUI::new(self.nonvolatile, self.ui_csv_paste_string.clone(), self.ui_error_opt.clone()))
		];
		out.append(&mut self.layout.get_properties());
		out
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::AddressWidth(new_addr_size, _) = property {
			*self = Self::from_save(self.generic.save(), new_addr_size, Some(self.data.clone()), self.layout.save());
		}
		if let SelectProperty::MemoryProperties(props) = &property {
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
		if self.layout.set_property(property) {
			*self = Self::from_save(self.generic.save(), self.addr_size, Some(self.data.clone()), self.layout.save());
		}
	}
}

#[cfg(feature = "using_egui")]
#[derive(Clone, Debug, PartialEq)]
pub struct MemoryPropertiesUI {
	pub nonvolatile: bool,
	pub erase: bool,
	pub csv_paste_string: Rc<RefCell<String>>,
	pub paste: bool,
	pub error_opt: Option<String>,
	pub changed: bool
}

#[cfg(feature = "using_egui")]
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
pub struct TriStateBufferOld(LogicDeviceGeneric);

impl TriStateBufferOld {
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

impl LogicDevice for TriStateBufferOld {
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(2.0, 0.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(2.0, 0.0)
		], draw.styles().color_foreground);
	}
}

#[derive(Debug)]
pub struct TriStateBuffer(LogicDeviceGeneric, BlockLayoutHelper, u16);

impl TriStateBuffer {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), BusLayoutSave::default(), 1)
	}
	pub fn from_save(save: LogicDeviceSave, layout_save: BusLayoutSave, bw: u16) -> Self {
		let layout = BlockLayoutHelper::load(
			layout_save,
			vec![
				(FourWayDir::E, 1, bw, "Q".to_owned()),
				(FourWayDir::W, 1, bw, "D".to_owned()),
				(FourWayDir::S, 1, 1, "En".to_owned())
			],
			IntV2(4, 2)
		);
		let mut out = Self(
			LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				false,
				false
			),
			layout,
			bw
		);
		for bit_i in 0..out.2 {
			out.set_pin_internal_state_panic(out.1.get_logic_pin_id_panic("D", bit_i), LogicState::Floating);
		}
		out
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
		let enable: bool = self.get_pin_state_panic(self.1.get_logic_pin_id_panic("En", 0)).to_bool();
		for bit_i in 0..self.2 {
			let state: LogicState = if enable {
				self.get_pin_state_panic(self.1.get_logic_pin_id_panic("D", bit_i))
					.to_bool()// Even if input is floating the output has to have a defined state
					.into()
			}
			else {
				LogicState::Floating
			};
			self.set_pin_internal_state_panic(self.1.get_logic_pin_id_panic("Q", bit_i), state);
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::TriStateBufferNew(self.0.save(), self.1.save(), self.2))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_polyline(vec![
			V2::new(2.0, 0.0),
			V2::new(-2.0, -2.0),
			V2::new(-2.0, 2.0),
			V2::new(2.0, 0.0)
		], draw.styles().color_foreground);
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		let mut out = self.1.get_properties();
		out.push(SelectProperty::BitWidth(self.2));
		out
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		self.1.set_property(property);
		*self = Self::from_save(self.0.save(), self.1.save(), self.2);
	}
	fn set_bit_width(&mut self, bit_width: u16) {
		*self = Self::from_save(self.0.save(), self.1.save(), bit_width);
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
				(FourWayDir::W, 2, bits, "A".to_owned()),
				(FourWayDir::W, 2, bits, "B".to_owned()),
				(FourWayDir::E, 1, bits, "C".to_owned()),
				(FourWayDir::N, 1, 1, "Cin".to_owned()),
				(FourWayDir::S, 1, 1, "Cout".to_owned()),
			],
			IntV2(4, 4)
		);
		let mut out = Self{
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				false,
				false
			),
			bits,
			layout
		};
		for i in 0..out.bits {
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("A", i), LogicState::Floating);
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("B", i), LogicState::Floating);
		}
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Cin", 0), LogicState::Floating);
		out
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
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_rect(self.generic.ui_data.local_bb.0, self.generic.ui_data.local_bb.1, [0,0,0,0], draw.styles().color_foreground);
	}
	fn get_bit_width(&self) -> Option<u16> {
		Some(self.bits)
	}
	fn set_bit_width(&mut self, bit_width: u16) {
		*self = Self::from_save(self.generic.save(), self.layout.save(), bit_width);
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		self.layout.get_properties()
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		self.layout.set_property(property);
		*self = Self::from_save(self.generic.save(), self.layout.save(), self.bits)
	}
}

/// Parameterized Data Latch
#[derive(Debug)]
pub struct DLatch {
	generic: LogicDeviceGeneric,
	/// 0 to 256
	bits: u16,
	layout: BlockLayoutHelper,
	prev_clock: Option<bool>,
	data_low: u128,
	data_high: u128,
	// Whether there is an output enable pin
	oe: bool,
	// Whether there are Set and Reset pins
	sr: bool
}

impl DLatch {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), BusLayoutSave::default(), 1, 0, 0, false, false)
	}
	pub fn from_save(save: LogicDeviceSave, layout_save: BusLayoutSave, bits: u16, data_low: u128, data_high: u128, oe: bool, sr: bool) -> Self {
		let d_margin: u32 = match oe {
			true => 1,
			false => 2
		};
		let mut pin_groups = vec![
			(FourWayDir::W, d_margin, bits, "D".to_owned()),
			(FourWayDir::E, 2, bits, "Q#".to_owned()),
			(FourWayDir::E, 2, bits, "Q".to_owned())
		];
		if oe {
			pin_groups.push((FourWayDir::W, 1, 1, "OE".to_owned()));
		}
		if sr {
			pin_groups.push((FourWayDir::S, 2, 1, "S".to_owned()));
			pin_groups.push((FourWayDir::S, 2, 1, "R".to_owned()));
		}
		pin_groups.push((FourWayDir::W, 1, 1, "CLK".to_owned()));
		let layout = BlockLayoutHelper::load(
			layout_save,
			pin_groups,
			IntV2(4, 4)
		);
		let mut out = Self {
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				false,
				false
			),
			bits,
			layout,
			prev_clock: None,
			data_low,
			data_high,
			oe,
			sr
		};
		for i in 0..out.bits {
			out.set_pin_internal_state_panic(*out.layout.group_logical_pins.get(&("D".to_owned(), i)).unwrap(), LogicState::Floating);
		}
		out.set_pin_internal_state_panic(*out.layout.group_logical_pins.get(&("CLK".to_owned(), 0)).unwrap(), LogicState::Floating);
		if out.oe {
			out.set_pin_internal_state_panic(*out.layout.group_logical_pins.get(&("OE".to_owned(), 0)).unwrap(), LogicState::Floating);
		}
		if out.sr {
			out.set_pin_internal_state_panic(*out.layout.group_logical_pins.get(&("S".to_owned(), 0)).unwrap(), LogicState::Floating);
			out.set_pin_internal_state_panic(*out.layout.group_logical_pins.get(&("R".to_owned(), 0)).unwrap(), LogicState::Floating);
		}
		out
	}
}

impl LogicDevice for DLatch {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _self_component_id: u64, _clock_state: bool, _first_propagation_step: bool) {
		// Check positive clock edge
		let current_clock: bool = self.get_pin_state_panic(*self.layout.group_logical_pins.get(&("CLK".to_owned(), 0)).unwrap()).to_bool();
		let clock_rising_edge = match self.prev_clock {
			Some(bool) => {
				current_clock && !bool
			},
			None => false
		};
		self.prev_clock = Some(current_clock);
		// Load data if clock edge
		if clock_rising_edge {
			for i in 0..self.bits {
				let bit: bool = self.get_pin_state_panic(*self.layout.group_logical_pins.get(&("D".to_owned(), i)).unwrap()).to_bool();
				if i < 128 {
					if bit {
						self.data_low |= 1 << i;
					}
					else {
						self.data_low &= !(1 << i);
					}
				}
				else {
					if bit {
						self.data_high |= 1 << (i-128);
					}
					else {
						self.data_high &= !(1 << (i-128));
					}
				}
			}
		}
		// Check if set or reset
		if self.sr {
			if self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("S", 0)).to_bool() {
				self.data_low = u128::MAX;
				self.data_high = u128::MAX;
			}
			if self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("R", 0)).to_bool() {
				self.data_low = 0;
				self.data_high = 0;
			}
		}
		// Output enable/disable
		let oe: bool = if self.oe {
			self.get_pin_state_panic(*self.layout.group_logical_pins.get(&("OE".to_owned(), 0)).unwrap()).to_bool()
		}
		else {
			true
		};
		for i in 0..self.bits {
			if oe {
				let bit: bool = if i < 128 {
					((self.data_low >> i) & 1) % 2 == 1
				}
				else {
					((self.data_high >> (i-128)) & 1) % 2 == 1
				};
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q", i), bit.into());
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q#", i), (!bit).into());
			}
			else {
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q", i), LogicState::Floating);
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q#", i), LogicState::Floating);
			}
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::DLatch(self.generic.save(), self.layout.save(), self.bits, self.data_low, self.data_high, self.oe, self.sr))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_rect(self.generic.ui_data.local_bb.0, self.generic.ui_data.local_bb.1, [0,0,0,0], draw.styles().color_foreground);
		//draw.text("DLatch".to_owned(), V2::zeros(), Align2::CENTER_CENTER, draw.styles().text_color, draw.styles().text_size_grid, !draw.direction.is_horizontal());
	}
	fn get_bit_width(&self) -> Option<u16> {
		Some(self.bits)
	}
	fn set_bit_width(&mut self, bit_width: u16) {
		*self = Self::from_save(self.generic.save(), self.layout.save(), bit_width, self.data_low, self.data_high, self.oe, self.sr);
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		let mut out = self.layout.get_properties();
		out.push(SelectProperty::HasPin("OE".to_owned(), self.oe));
		out.push(SelectProperty::HasPin("SR".to_owned(), self.oe));
		out
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::HasPin(pin_name, pin_en) = &property {
			match pin_name.as_str() {
				"OE" => {
					self.oe = *pin_en;
				},
				"SR" => {
					self.sr = *pin_en;
				},
				_ => {}
			}
		}
		else {
			self.layout.set_property(property);
		}
		*self = Self::from_save(self.generic.save(), self.layout.save(), self.bits, self.data_low, self.data_high, self.oe, self.sr)
	}
}

/// Parameterized Data Latch
#[derive(Debug)]
pub struct SRLatch {
	generic: LogicDeviceGeneric,
	layout: BlockLayoutHelper,
	state: bool
}

impl SRLatch {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), BusLayoutSave::default(), false)
	}
	pub fn from_save(save: LogicDeviceSave, layout_save: BusLayoutSave, state: bool) -> Self {
		let layout = BlockLayoutHelper::load(
			layout_save,
			vec![
				(FourWayDir::W, 2, 1, "S".to_owned()),
				(FourWayDir::W, 2, 1, "R".to_owned()),
				(FourWayDir::E, 2, 1, "Q#".to_owned()),
				(FourWayDir::E, 2, 1, "Q".to_owned())
			],
			IntV2(4, 4)
		);
		let mut out = Self {
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				false,
				false
			),
			layout,
			state
		};
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("S", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("R", 0), LogicState::Floating);
		out
	}
}

impl LogicDevice for SRLatch {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _self_component_id: u64, _clock_state: bool, _first_propagation_step: bool) {
		let s = self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("S", 0)).to_bool();
		let r = self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("R", 0)).to_bool();
		if s {
			self.state = true;
		}
		if r {
			self.state = false;
		}
		self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q", 0), self.state.into());
		self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q#", 0), (!(self.state || (s && r))).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::SRLatch(self.generic.save(), self.layout.save(), self.state))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_rect(self.generic.ui_data.local_bb.0, self.generic.ui_data.local_bb.1, [0,0,0,0], draw.styles().color_foreground);
		//draw.text("DLatch".to_owned(), V2::zeros(), Align2::CENTER_CENTER, draw.styles().text_color, draw.styles().text_size_grid, !draw.direction.is_horizontal());
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		self.layout.get_properties()
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		self.layout.set_property(property);
		*self = Self::from_save(self.generic.save(), self.layout.save(), self.state)
	}
}

/// Parameterized Counter
#[derive(Debug)]
pub struct Counter {
	generic: LogicDeviceGeneric,
	/// 0 to 256
	bits: u16,
	layout: BlockLayoutHelper,
	prev_clock: Option<bool>,
	data_low: u128,
	data_high: u128,
	// Whether there is an output enable pin
	oe: bool
}

impl Counter {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), BusLayoutSave::default(), 8, 0, 0, false)
	}
	pub fn from_save(save: LogicDeviceSave, layout_save: BusLayoutSave, bits: u16, data_low: u128, data_high: u128, oe: bool) -> Self {
		let mut pin_groups = vec![
			(FourWayDir::E, 1, bits, "Q".to_owned()),
			(FourWayDir::S, 1, 1, "RCO#".to_owned()),
			(FourWayDir::N, 1, 1, "CLK".to_owned()),
			(FourWayDir::W, 1, 1, "CLR".to_owned()),
			(FourWayDir::W, 1, 1, "CLK EN".to_owned()),
		];
		if oe {
			pin_groups.push((FourWayDir::W, 1, 1, "OE".to_owned()));
		}
		let layout = BlockLayoutHelper::load(
			layout_save,
			pin_groups,
			IntV2(6, 6)
		);
		let mut out = Self {
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				layout.get_bb_float(),
				false,
				false
			),
			bits,
			layout,
			prev_clock: None,
			data_low,
			data_high,
			oe
		};
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("CLK", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("CLR", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("CLK EN", 0), LogicState::Floating);
		if out.oe {
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("OE", 0), LogicState::Floating);
		}
		out
	}
}

impl LogicDevice for Counter {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _self_component_id: u64, _clock_state: bool, _first_propagation_step: bool) {
		// Check positive clock edge
		let current_clock: bool = self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("CLK", 0)).to_bool();
		let clock_rising_edge = match self.prev_clock {
			Some(bool) => {
				current_clock && !bool
			},
			None => false
		} && self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("CLK EN", 0)).to_bool();
		self.prev_clock = Some(current_clock);
		// Increment data if clock edge
		if clock_rising_edge {
			let old_data_low = self.data_low;
			self.data_low = self.data_low.wrapping_add(1);
			if old_data_low > self.data_low {// Will this ever be be used?
				self.data_high = self.data_high.wrapping_add(1);
			}
		}
		// Clear
		if self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("CLR", 0)).to_bool() {
			self.data_low = 0;
			self.data_high = 0;
		}
		// Output enable/disable
		let oe: bool = if self.oe {
			self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("OE", 0)).to_bool()
		}
		else {
			true
		};
		for i in 0..self.bits {
			if oe {
				let bit: bool = if i < 128 {
					((self.data_low >> i) & 1) % 2 == 1
				}
				else {
					((self.data_high >> (i-128)) & 1) % 2 == 1
				};
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q", i), bit.into());
			}
			else {
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Q", i), LogicState::Floating);
			}
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::Counter(self.generic.save(), self.layout.save(), self.bits, self.data_low, self.data_high, self.oe))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		draw.draw_rect(self.generic.ui_data.local_bb.0, self.generic.ui_data.local_bb.1, [0,0,0,0], draw.styles().color_foreground);
		//draw.text("DLatch".to_owned(), V2::zeros(), Align2::CENTER_CENTER, draw.styles().text_color, draw.styles().text_size_grid, !draw.direction.is_horizontal());
	}
	fn get_bit_width(&self) -> Option<u16> {
		Some(self.bits)
	}
	fn set_bit_width(&mut self, bit_width: u16) {
		*self = Self::from_save(self.generic.save(), self.layout.save(), bit_width, self.data_low, self.data_high, self.oe);
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		let mut out = self.layout.get_properties();
		out.push(SelectProperty::HasPin("OE".to_owned(), self.oe));
		out
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::HasPin(_, oe_pin) = &property {
			self.oe = *oe_pin;
		}
		self.layout.set_property(property);
		*self = Self::from_save(self.generic.save(), self.layout.save(), self.bits, self.data_low, self.data_high, self.oe)
	}
}

/// Vector graphics CRT simulator made specifically to test a part of my DIY computer that I am expanding
/// Line/Point time is constant, LERPing controlled by triangle wave
#[derive(Debug)]
pub struct VectorCRT {
	generic: LogicDeviceGeneric,
	layout: BlockLayoutHelper,
	/// 0 to 1, for lerping
	start_time: Instant,
	/// Is the LERP triangle wave going up or down?
	lerp_rising: bool,
	/// Vec<(Start, End which will be updated until the LERP is over)>
	lines: Vec<(IntV2, IntV2)>,
	points: Vec<IntV2>,
	/// Time taken per line, or time spent per point
	period: f32,
	v0: V2,
	v1: V2,
	curr_lerp_value: f32,
	start_lerp_pos: f32
}

impl VectorCRT {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default(), BusLayoutSave::default())
	}
	pub fn from_save(save: LogicDeviceSave, layout_save: BusLayoutSave) -> Self {
		let pin_groups = vec![
			(FourWayDir::W, 2, 10, "X0".to_owned()),
			(FourWayDir::W, 2, 10, "Y0".to_owned()),
			(FourWayDir::W, 2, 10, "X1".to_owned()),
			(FourWayDir::W, 2, 10, "Y1".to_owned()),
			(FourWayDir::W, 1, 1, "Clear".to_owned()),
			(FourWayDir::W, 1, 1, "Line Done".to_owned()),
			(FourWayDir::W, 1, 1, "Beam Enable".to_owned()),
			(FourWayDir::W, 1, 1, "Point (0) / Line (1) select".to_owned()),
			(FourWayDir::W, 1, 1, "Update V0".to_owned()),
			(FourWayDir::W, 1, 1, "Update V1".to_owned())
		];
		let layout = BlockLayoutHelper::load(
			layout_save,
			pin_groups,
			IntV2(280, 260)// Not actually correct, just to get it to put the pins in the right place
		);
		let mut out = Self {
			generic: LogicDeviceGeneric::load(
				save,
				layout.pin_config(),
				(V2::new(-140.0, -130.0), V2::new(129.0, 129.0)),
				false,
				false
			),
			layout,
			start_time: Instant::now(),
			lerp_rising: true,
			lines: Vec::new(),
			points: Vec::new(),
			period: 2.0,
			v0: V2::zeros(),
			v1: V2::zeros(),
			curr_lerp_value: 0.0,
			start_lerp_pos: 0.0
		};
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Clear", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Beam Enable", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Point (0) / Line (1) select", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Update V0", 0), LogicState::Floating);
		out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Update V1", 0), LogicState::Floating);
		for bit_i in 0..10 {
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("X0", bit_i), LogicState::Floating);
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("X1", bit_i), LogicState::Floating);
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Y0", bit_i), LogicState::Floating);
			out.set_pin_internal_state_panic(out.layout.get_logic_pin_id_panic("Y1", bit_i), LogicState::Floating);
		}
		out
	}
	fn save_current_line(&mut self) {
		self.lines.push((round_v2_to_intv2(self.get_lerp_start_pos()), round_v2_to_intv2(self.get_lerp_pos())));
	}
	fn get_line_start(&self) -> V2 {
		match self.lerp_rising {
			true => self.v0,
			false => self.v1
		}
	}
	fn get_line_end(&self) -> V2 {
		match self.lerp_rising {
			true => self.v1,
			false => self.v0
		}
	}
	fn get_lerp_pos(&self) -> V2 {
		self.v0 + (self.v1 - self.v0)*self.curr_lerp_value
	}
	fn get_lerp_start_pos(&self) -> V2 {
		self.v0 + (self.v1 - self.v0)*self.start_lerp_pos
	}
	fn update_input_vectors(&self) -> ((V2, V2), bool) {
		let logic_pins_rc = Rc::clone(&self.generic.logic_pins);
		let new_v0 = IntV2(self.layout.get_bus_value_panic("X0", &logic_pins_rc).0 as i32, self.layout.get_bus_value_panic("Y0", &logic_pins_rc).0 as i32);
		let new_v1 = IntV2(self.layout.get_bus_value_panic("X1", &logic_pins_rc).0 as i32, self.layout.get_bus_value_panic("Y1", &logic_pins_rc).0 as i32);
		(
			(new_v0.to_v2(), new_v1.to_v2()),
			new_v0 != round_v2_to_intv2(self.v0) || new_v1 != round_v2_to_intv2(self.v1)
		)
	}
}

impl LogicDevice for VectorCRT {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack, _self_component_id: u64, _clock_state: bool, _first_propagation_step: bool) {
		// Set quick signals back to 0
		self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Line Done", 0), LogicState::Driven(false));
		self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Update V0", 0), LogicState::Driven(false));
		self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Update V1", 0), LogicState::Driven(false));
		// Check clear
		if self.get_pin_state_panic(self.layout.get_logic_pin_id_panic("Clear", 0)).to_bool() {
			self.lines = Vec::new();
			self.points = Vec::new();
		}
		// Timing
		let now = Instant::now();
		let dt = (now - self.start_time).as_secs_f32();
		self.curr_lerp_value = if self.lerp_rising {
			dt / self.period
		}
		else {
			1.0 - (dt / self.period)
		};
		// Check if line is done
		if dt >= self.period {// Line end
			self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Line Done", 0), LogicState::Driven(true));
			self.save_current_line();
			self.start_lerp_pos = self.curr_lerp_value;
			let ((new_v0, new_v1), _) = self.update_input_vectors();
			self.v0 = new_v0;
			self.v1 = new_v1;
			self.start_time = now;
			self.lerp_rising = !self.lerp_rising;
			if self.lerp_rising {
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Update V1", 0), LogicState::Driven(true));
			}
			else {
				self.set_pin_internal_state_panic(self.layout.get_logic_pin_id_panic("Update V0", 0), LogicState::Driven(true));
			}
		}
		// If vectors change even when line isn't done, save and start a new line
		let ((new_v0, new_v1), are_vecs_new) = self.update_input_vectors();
		if are_vecs_new {
			self.save_current_line();
			self.start_lerp_pos = self.curr_lerp_value;
			self.v0 = new_v0;
			self.v1 = new_v1;
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::VectorCRT(self.generic.save(), self.layout.save()))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
		/*
		BB size: (269, 259), BB from (-140, -130) to (129, 129)
		Connections on left (west)
		CRT View from (-128, -128) to (128, 128)
		*/
		draw.draw_rect(self.generic.ui_data.local_bb.0, self.generic.ui_data.local_bb.1, [0,0,0,0], draw.styles().color_foreground);
		draw.draw_rect(V2::new(-128.0, -128.0), V2::new(128.0, 128.0), [0,0,0,0], draw.styles().color_foreground);
		let to_display_offset = IntV2(256+128, 256+128);
		for line in self.lines.iter().chain(vec![(round_v2_to_intv2(self.get_lerp_start_pos()), round_v2_to_intv2(self.get_lerp_pos()))].iter()).map(|(v0, v1)| (*v0 - to_display_offset, *v1 - to_display_offset)) {
			if let Some((start_cliped, end_cliped)) = clip_line_to_rect((line.0.to_v2(), line.1.to_v2()), (V2::new(-128.0, -128.0), V2::new(128.0, 128.0))) {
				draw.draw_polyline(vec![start_cliped, end_cliped], [0, 255, 0]);
			}
		}
		// Just current line test
		/*for line in vec![(self.get_line_start(), self.get_line_end())].iter().map(|(v0, v1)| (*v0 - to_display_offset, *v1 - to_display_offset)) {
			if let Some((start_cliped, end_cliped)) = clip_line_to_rect((line.0.to_v2(), line.1.to_v2()), (V2::new(-128.0, -128.0), V2::new(128.0, 128.0))) {
				draw.draw_polyline(vec![start_cliped, end_cliped], [0, 255, 0]);
			}
		}*/
		// Without current line test
		/*for line in self.lines.iter().map(|(v0, v1)| (*v0 - to_display_offset, *v1 - to_display_offset)) {
			if let Some((start_cliped, end_cliped)) = clip_line_to_rect((line.0.to_v2(), line.1.to_v2()), (V2::new(-128.0, -128.0), V2::new(128.0, 128.0))) {
				draw.draw_polyline(vec![start_cliped, end_cliped], [0, 255, 0]);
			}
		}*/
		draw.text(&format!("Pos from: {:?}, current: {:?}, to: {:?}, Current line time: {}", self.get_line_start(), round_v2_to_intv2(self.get_lerp_pos()), self.get_line_end(), (Instant::now() - self.start_time).as_secs_f32()), V2::new(-128.0, -129.0), GenericAlign2::LEFT_CENTER, draw.styles().text_color, 1.5, false);
	}
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		self.layout.get_properties()
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		self.layout.set_property(property);
		*self = Self::from_save(self.generic.save(), self.layout.save())
	}
}