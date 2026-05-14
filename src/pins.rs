//! For Logical and Graphical pins

use std::{cell::{Ref, RefCell, RefMut}, collections::{HashMap, HashSet}, default::Default, fmt::Debug, rc::Rc};
use serde::{Deserialize, Serialize};
use crate::{prelude::*, simulator::merge_logic_states};

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicConnectionPin {
	pub internal_source: Option<LogicConnectionPinInternalSource>,
	pub internal_state: LogicState,
	pub external_source: Option<LogicConnectionPinExternalSource>,
	pub external_state: LogicState,
	
}

impl LogicConnectionPin {
	pub fn new(
		internal_source: Option<LogicConnectionPinInternalSource>,
		external_source: Option<LogicConnectionPinExternalSource>
	) -> Self {
		Self {
			internal_source,
			internal_state: LogicState::Floating,
			external_source,
			external_state: LogicState::Floating
		}
	}
	pub fn set_drive_internal(&mut self, state: LogicState) {
		self.internal_state = state;
	}
	pub fn set_drive_external(&mut self, state: LogicState) {
		self.external_state = state;
	}
	pub fn state(&self) -> LogicState {
		merge_logic_states(self.internal_state, self.external_state)
	}
	fn is_connected_to_net(&self, net_id: u64) -> bool {
		match &self.internal_source {
			Some(source) => match source {
				LogicConnectionPinInternalSource::ComponentInternal => false,
				LogicConnectionPinInternalSource::Net(test_net_id) => *test_net_id == net_id
			},
			None => false
		}
	}
}

#[derive(Clone, Debug)]
pub struct GraphicPin {
	/// Each graphic pin has a reference to all external conection logic pins so that they can be added/removed when the graphic bit width is changed
	pub component_all_logic_pins: Rc<RefCell<HashMap<u64, RefCell<LogicConnectionPin>>>>,
	/// Vec of keys to `component_all_logic_pins`, order is important
	pub owned_pins: Vec<u64>,
	/// Usually 1, may be something else if theres a curve on an OR input or something
	pub length: f32,
	pub ui_data: UIData,
	/// Only for user, defaults to ""
	pub name: String,
	pub show_name: bool,
	pub wire_connections: Option<Rc<RefCell<HashSet<WireConnection>>>>
}

impl GraphicPin {
	pub fn new(
		component_all_logic_pins: Rc<RefCell<HashMap<u64, RefCell<LogicConnectionPin>>>>,
		owned_pins: Vec<u64>,
		relative_end_grid: IntV2,
		direction: FourWayDir,
		length: f32,
		name: String,
		show_name: bool
	) -> Self {
		let bw: usize = owned_pins.len();
		Self {
			component_all_logic_pins,
			owned_pins,
			length,
			ui_data: UIData::new(relative_end_grid, direction, Self::get_local_bb(bw as u16)),
			name,
			show_name,
			wire_connections: Some(Rc::new(RefCell::new(HashSet::new())))
		}
	}
	pub fn iter_owned_pins<T, U: FnMut(Ref<'_, LogicConnectionPin>) -> T>(&self, mut f: U) -> Vec<T> {
		let mut out = Vec::<T>::new();
		let logic_pins = self.component_all_logic_pins.borrow();
		for logic_pin_id in &self.owned_pins {
			let pin_cell = logic_pins.get(logic_pin_id).unwrap();
			out.push(f(pin_cell.borrow()));
		}
		out
	}
	pub fn iter_owned_pins_mut<T, U: FnMut(RefMut<'_, LogicConnectionPin>) -> T>(&self, mut f: U) -> Vec<T> {
		let mut out = Vec::<T>::new();
		let logic_pins = self.component_all_logic_pins.borrow();
		for logic_pin_id in &self.owned_pins {
			let pin_cell = logic_pins.get(logic_pin_id).unwrap();
			out.push(f(pin_cell.borrow_mut()));
		}
		out
	}
	pub fn states(&self) -> Vec<LogicState> {
		self.iter_owned_pins(|pin| pin.state())
	}
	pub fn internal_sources(&self) -> Vec<Option<LogicConnectionPinInternalSource>> {
		self.iter_owned_pins(|pin| {pin.internal_source.clone()})
	}
	pub fn external_sources(&self) -> Vec<Option<LogicConnectionPinExternalSource>> {
		self.iter_owned_pins(|pin| {pin.external_source.clone()})
	}
	pub fn get_color(&self, styles: &Styles) -> [u8; 3] {
		if self.owned_pins.len() == 1 {
			styles.color_from_logic_state(self.component_all_logic_pins.borrow().get(&self.owned_pins[0]).unwrap().borrow().state())
		}
		else {
			styles.color_from_logic_states(&self.states())
		}
	}
	fn get_local_bb(bw: u16) -> (V2, V2) {
		(V2::new(1.0, -1.0), V2::new(1.0 + (bw as f32 * 2.0), 1.0))
	}
}

/// ONLY meant to be used on external pins on the toplevel circuit, all other pins are just rendered as part of the component
impl GraphicSelectableItem for GraphicPin {
	fn draw<'a>(&self, draw_parent: &Box<dyn DrawInterface>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		let external_and_combined_states: Vec<(LogicState, LogicState)> = self.iter_owned_pins(|pin| (pin.external_state.clone(), pin.state()));
		let color = draw.styles().color_from_logic_states(&external_and_combined_states.iter().map(|t|t.1.clone()).collect());
		for i in 0..self.owned_pins.len() {
			draw.draw_polyline(
				vec![
					V2::new(1.1, -0.9),
					V2::new(1.1, 0.9),
					V2::new(2.9, 0.9),
					V2::new(2.9, -0.9),
					V2::new(1.1, -0.9)
				].iter().map(|v| v + V2::new((i*2) as f32, 0.0)).collect(),
				draw.styles().color_from_logic_state(external_and_combined_states[i].1)
			);
			if let Some(value) = external_and_combined_states[i].0.to_bool_opt() {
				draw.text(match value {true => "1", false => "0"}, V2::new((i*2) as f32 + 2.0, 0.0), GenericAlign2::CENTER_CENTER, draw.styles().text_color, 1.5, false);
			}
		}
		draw.draw_polyline(
			vec![
				V2::zeros(),
				V2::new(1.0, 0.0)
			],
			color
		);
	}
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	/*fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let half_diagonal = V2::new(1.0, 1.0);
		let box_center = (self.ui_data.direction.to_unit() * 2.0) + self.ui_data.position.to_v2();
		let global_offset = grid_offset + box_center;
		(global_offset - half_diagonal, global_offset + half_diagonal)
	}*/
	fn is_connected_to_net(&self, net_id: u64) -> bool {
		let mut out = false;
		self.iter_owned_pins(|pin| {
			out |= pin.is_connected_to_net(net_id);
		});
		out
	}
	#[cfg(feature = "using_egui")]
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::BitWidth(self.owned_pins.len() as u16),
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::GlobalConnectionState(self.iter_owned_pins(|pin| pin.external_state.to_bool_opt())),
			SelectProperty::Direction(self.ui_data.direction),
			SelectProperty::Name(self.name.clone())
		]
	}
	#[cfg(feature = "using_egui")]
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::BitWidth(bit_width) => {
				assert!(bit_width > 0, "Bit width cannot be 0");
				self.ui_data.local_bb = Self::get_local_bb(bit_width);
				let diff: isize = bit_width as isize - (self.owned_pins.len() as isize);
				if diff > 0 {
					// Add logic pins
					let mut all_pins = self.component_all_logic_pins.borrow_mut();
					for _ in 0..diff {
						let new_logic_pin_id: u64 = lowest_unused_key(&*all_pins);
						all_pins.insert(new_logic_pin_id, RefCell::new(LogicConnectionPin::new(None, Some(LogicConnectionPinExternalSource::Global))));
						self.owned_pins.push(new_logic_pin_id);
					}
				}
				if diff < 0 {
					// Remove logic pins
					let mut all_pins = self.component_all_logic_pins.borrow_mut();
					for _ in 0..(-diff) {
						let logic_pin_id: u64 = self.owned_pins.pop().expect("There should always be at least 1 owned pin");
						all_pins.remove(&logic_pin_id);
					}
				}
			},
			SelectProperty::PositionX(x) => {
				self.ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.ui_data.position.1 = y;
			},
			SelectProperty::GlobalConnectionState(driven_opts) => {
				let all_pins = self.component_all_logic_pins.borrow();
				for (i, pin_id) in self.owned_pins.iter().enumerate() {
					let pin_cell = all_pins.get(pin_id).unwrap();
					if i < driven_opts.len() {// In case multiple pins with different bit widths are selected and this one is longer than the one setting the property
						pin_cell.borrow_mut().external_state = driven_opts[i].into();
					}
				}
			},
			SelectProperty::Direction(direction) => {
				self.ui_data.direction = direction;
			},
			SelectProperty::Name(name) => {
				self.name = name;
			}
			_ => {}
		}
	}
	#[cfg(feature = "using_egui")]
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::ExternalConnection(self.ui_data.position, self.ui_data.direction, self.name.clone(), self.show_name, self.owned_pins.len() as u16)
	}
	#[cfg(feature = "using_egui")]
	fn accept_click(&mut self, local_pos: V2) -> bool {
		let bit_index = ((local_pos.x - 1.0) / 2.0) as isize;
		if bit_index >= 0 && bit_index < self.owned_pins.len() as isize {
			let comp_pins = self.component_all_logic_pins.borrow();
			let mut pin_mut = comp_pins.get(&self.owned_pins[bit_index as usize]).unwrap().borrow_mut();
			match pin_mut.external_source.clone().expect("Pin being used as a graphic item must have an external source") {
				LogicConnectionPinExternalSource::Global => {
					if pin_mut.external_state.is_valid() {
						pin_mut.external_state = (!pin_mut.external_state.to_bool()).into();
						true
					}
					else {
						false
					}
				},
				LogicConnectionPinExternalSource::Net(_) => panic!("Pin being used as a graphic item cannot have external net source")
			}
		}
		else {
			false
		}
	}
}

/// Custom implementation to get rid of logic pins owned by this graphic pin
impl Drop for GraphicPin {
	fn drop(&mut self) {
		let mut all_pins = self.component_all_logic_pins.borrow_mut();
		for pin_id in &self.owned_pins {
			all_pins.remove(&pin_id);
		}
	}
}