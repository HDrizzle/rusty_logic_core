//! Heavily based off of the logic simulation I wrote in TS for use w/ MotionCanvas, found at https://github.com/HDrizzle/stack_machine/blob/main/presentation/src/logic_sim.tsx

use std::{cell::{Ref, RefCell, RefMut}, collections::{HashMap, HashSet}, default::Default, fmt::Debug, fs, ops::{Deref, DerefMut, RangeInclusive}, rc::Rc, time::{Duration, Instant}};
use serde::{Deserialize, Serialize};
use crate::{prelude::*, resource_interface};
use resource_interface::LogicCircuitSave;
use eframe::egui::{self, response::Response, Align2, DragValue, Key, KeyboardShortcut, Label, Modifiers, PointerButton, Pos2, Rect, ScrollArea, Vec2, Widget};
use arboard;
use common_macros::hash_map;
use eframe::egui::{Ui, Sense, Stroke, Frame};

fn logic_device_to_graphic_item(x: &dyn LogicDevice) -> &dyn GraphicSelectableItem {
	x
}

fn logic_device_to_graphic_item_mut(x: &mut dyn LogicDevice) -> &mut dyn GraphicSelectableItem {
	x
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum LogicState {
	Driven(bool),
	Floating,
	Contested
}

impl LogicState {
	pub fn value(&self) -> Option<bool> {
		match self {
			Self::Driven(state) => Some(*state),
			Self::Floating => None,
			Self::Contested => None
		}
	}
	pub fn is_valid(&self) -> bool {
		if let Self::Driven(_test) = self {
			true
		}
		else {
			false
		}
	}
	pub fn is_contested(&self) -> bool {
		if let Self::Contested = self {
			true
		}
		else {
			false
		}
	}
	pub fn is_floating(&self) -> bool {
		if let Self::Floating = self {
			true
		}
		else {
			false
		}
	}
	/// WARNING! Not neccessarily the same as real-world, this method is only here because logic gates will have to work w/ something, even if their inputs are floating or contested
	pub fn to_bool(&self) -> bool {
		match &self {
			Self::Driven(b) => *b,
			Self::Floating => false,
			Self::Contested => false
		}
	}
	pub fn to_bool_opt(&self) -> Option<bool> {
		match &self {
			Self::Driven(b) => Some(*b),
			Self::Floating => None,
			Self::Contested => None
		}
	}
}

impl Default for LogicState {
	fn default() -> Self {
		Self::Floating
	}
}

impl From<bool> for LogicState {
	fn from(value: bool) -> Self {
		Self::Driven(value)
	}
}

impl From<Option<bool>> for LogicState {
	fn from(value_opt: Option<bool>) -> Self {
		match value_opt {
			Some(value) => Self::Driven(value),
			None => Self::Floating
		}
	}
}

/// If two wires are connected, what will their combined state be?
pub fn merge_logic_states(a: LogicState, b: LogicState) -> LogicState {
	if a.is_valid() || b.is_valid() {// Both driven normally
		if a.is_valid() && b.is_valid() {
			if a.value().expect("This shouldn't happen") == b.value().expect("This shouldn't happen") {
				LogicState::Driven(a.value().expect("This shouldn't happen"))
			}
			else {
				LogicState::Contested
			}
		}
		else {// One of them is driven normally, the other is either contested or floating
			let (valid, invalid): (LogicState, LogicState) = if a.is_valid() {
				(a, b)
			}
			else {
				(b, a)
			};
			if invalid.is_contested() {
				LogicState::Contested
			}
			else {// Other one is floating
				valid
			}
		}
	}
	else {
		if a.is_contested() || b.is_contested() {
			LogicState::Contested
		}
		else {// Both floating
			LogicState::Floating
		}
	}
}

/// For devices that don't use busses
pub fn graphic_pin_config_from_single_pins(in_: HashMap<u64, (IntV2, FourWayDir, f32, String, bool)>) -> HashMap<u64, (IntV2, FourWayDir, f32, String, bool, Vec<u64>)> {
	HashMap::from_iter(in_.into_iter().map(|t| (t.0, (t.1.0, t.1.1, t.1.2, t.1.3, t.1.4, vec![t.0]))))
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubCircuitPath(Vec<u64>);

impl SubCircuitPath {
	pub fn to_string(&self) -> String {
		self.0.iter().map(|id| id.to_string()).collect::<Vec<String>>().join("/") + "/"
	}
}

// Not just something that is connected, but something that is setting the voltage either high or low
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LogicDriveSource {
	/// Set by the UI or the Clock or whatever
	Global,
	/// Connection to something outside this circuit
	ExternalConnection(GenericQuery<LogicConnectionPin>),
	/// A set of things connected together
	/// This also depends on context, for example the nets one either side of a sub-circuit pin would be referenced within different "namespaces"
	Net(GenericQuery<LogicNet>),
	/// Output of a basic logic gate, the actual transistors are not gonna be simulated
	ComponentInternal(ComponentLogicPinReference)
}

impl LogicDriveSource {
	/// Basic component or global interface, cannot resolve any deeper
	pub fn is_final_source(&self) -> bool {
		match &self {
			Self::Global => true,
			Self::ExternalConnection(_) => false,
			Self::Net(_) => false,
			Self::ComponentInternal(_) => true
		}
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GlobalSourceReference {
	/// Pin of top level circuit, which always takes its inputs from Global
	Global(u64),
	/// Output from basic logic component
	/// 0. Vec of strings, each one a sub-circuit of the last
	/// 1. Component reference within the previously given circuit
	ComponentInternal(SubCircuitPath, ComponentLogicPinReference)
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicNet {
	connections: Vec<CircuitWideLogicPinReference>,
	pub sources: Vec<GlobalSourceReference>,
	pub state: LogicState
}

impl LogicNet {
	pub fn new(
		connections: Vec<CircuitWideLogicPinReference>
	) -> Self {
		Self {
			connections,
			sources: Vec::new(),
			state: LogicState::Floating
		}
	}
	/// Goes "down the rabbit hole" and finds everything that this net is connected to, and only takes logic states from global inputs and basic component outputs
	/// Pretty sure it can't get stuck in a recursive loop
	fn resolve_sources(&self, self_ancestors: &AncestryStack, self_id: u64, caller_history: &Vec<(AncestryStack, u64)>) -> Vec<(GlobalSourceReference, LogicState)> {
		// Keep history of recursion to ignore nets that have already been reached
		for (caller_ancestry, caller_id) in caller_history {
			if caller_ancestry == self_ancestors && *caller_id == self_id {
				return Vec::new();
			}
		}
		let mut new_caller_history = caller_history.clone();
		new_caller_history.push((self_ancestors.clone(), self_id));
		let mut out = Vec::<(GlobalSourceReference, LogicState)>::new();
		for connection in &self.connections {
			match self_ancestors.parent() {
				Some((circuit, circuit_id)) => match connection {
					CircuitWideLogicPinReference::ComponentPin(component_pin_ref) => match circuit.components.borrow().get(&component_pin_ref.component_id) {
						Some(component_cell) => match component_cell.borrow().get_logic_pins_cell().borrow().get(&component_pin_ref.pin_id) {
							Some(pin) => {
								// Check what the internal source is
								if let Some(source) = &pin.borrow().internal_source {
									match source {
										// Check if pin is internally driven (by a sub-circuit)
										LogicConnectionPinInternalSource::Net(child_circuit_net_id) => {
											let component = component_cell.borrow();
											let circuit = component.get_circuit();
											match circuit.nets.borrow().get(&child_circuit_net_id) {
												Some(child_net) => out.append(&mut child_net.borrow().resolve_sources(&self_ancestors.push((circuit, circuit_id)), *child_circuit_net_id, &new_caller_history)),
												None => panic!("Internal connection in circuit \"{}\" references net {:?} inside sub-circuit \"{}\", the net does not exist", circuit.get_generic().name, &child_circuit_net_id, component_cell.borrow().get_generic().name)
											};
										},
										// Check if pin is driven by a regular component
										LogicConnectionPinInternalSource::ComponentInternal => {
											out.push((GlobalSourceReference::ComponentInternal(self_ancestors.to_sub_circuit_path(), component_pin_ref.clone()), pin.borrow().internal_state));
										}
									}
								}
							},
							None => panic!("Net references internal pin {} on component \"{}\" circuit \"{}\", which doesn't exist on that component", component_pin_ref.pin_id, component_cell.borrow().get_generic().name, circuit.get_generic().name)
						},
						None => panic!("Net references internal pin on component {} circuit \"{}\", which doesn't exist in the circuit", component_pin_ref.component_id, circuit.get_generic().name)
					},
					CircuitWideLogicPinReference::ExternalConnection(ext_conn_id) => match circuit.get_logic_pins_cell().borrow().get(ext_conn_id) {
						Some(pin) => {
							// Check what the external source is
							if let Some(source) = &pin.borrow().external_source {
								match source {
									// Check if external pin is connected to net on other side
									LogicConnectionPinExternalSource::Net(parent_circuit_net_id) => {
										// External pin is connected to a net in a circuit that contains the circuit that this net is a part of
										// Check that this net's "grandparent" is a circuit and not toplevel
										match self_ancestors.grandparent() {
											Some((parent_circuit, _)) => match parent_circuit.nets.borrow().get(parent_circuit_net_id) {
												Some(parent_circuit_net) => out.append(&mut parent_circuit_net.borrow().resolve_sources(&self_ancestors.trim(), *parent_circuit_net_id, &new_caller_history)),
												None => panic!("External connection is referencing a net ({}) which does not exist in this circuit's parent", parent_circuit_net_id)
											},
											None => panic!("External connection is connected to a net, but the parent of this circuit is toplevel, this shouldn't happen")
										}
									}
									// Check if it is connected to a global pin
									LogicConnectionPinExternalSource::Global => {
										out.push((GlobalSourceReference::Global(ext_conn_id.to_owned()), pin.borrow().external_state));
									}
								}
							}
							// TODO: Only assert this if toplevel
							/*else {
								panic!("External connection doesn't have source");
							}*/
						},
						None => panic!("Net references external connection {:?} which is invalid", connection)
					}
				},
				None => panic!("Net cannot have a toplevel wrapper as its parent")
			}
		}
		// Done
		out
	}
	pub fn update_state(&self, ancestors: &AncestryStack, self_id: u64) -> (LogicState, Vec<GlobalSourceReference>) {
		let sources_raw = self.resolve_sources(ancestors, self_id, &Vec::new());
		let mut sources = Vec::<GlobalSourceReference>::new();
		let mut new_state = LogicState::Floating;
		// Go through and remove any logic states that are floating
		for (source, state) in sources_raw {
			if !state.is_floating() {
				sources.push(source);
				new_state = merge_logic_states(new_state, state);
			}
		}
		// Done
		(new_state, sources)
	}
	/// Makes sure component connection is or isn't included in this net
	pub fn edit_component_connection(&mut self, include: bool, comp_id: u64, pin_id: u64) {
		let mut index_to_remove_opt = Option::<usize>::None;
		for (conn_i, conn) in self.connections.iter().enumerate() {
			match conn {
				CircuitWideLogicPinReference::ExternalConnection(_) => {},
				CircuitWideLogicPinReference::ComponentPin(test_comp_pin_ref) => {
					if test_comp_pin_ref.component_id == comp_id && test_comp_pin_ref.pin_id == pin_id {
						if include {
							return;// If the connection already exists, return
						}
						else {
							index_to_remove_opt = Some(conn_i);
						}
					}
				}
			}
		}
		if include {
			// Hasn't returned yet, add connection
			self.connections.push(CircuitWideLogicPinReference::ComponentPin(ComponentLogicPinReference::new(comp_id, pin_id.to_owned())));
		}
		else {
			// If don't include and found it, remove it
			if let Some(index_to_remove) = index_to_remove_opt {
				self.connections.remove(index_to_remove);
			}
		}
	}
	/// Makes sure external connection is or isn't included in this net
	pub fn edit_external_connection(&mut self, include: bool, pin_id: u64) {
		let mut index_to_remove_opt = Option::<usize>::None;
		for (conn_i, conn) in self.connections.iter().enumerate() {
			match conn {
				CircuitWideLogicPinReference::ComponentPin(_) => {},
				CircuitWideLogicPinReference::ExternalConnection(test_pin_ref) => {
					if *test_pin_ref == pin_id {
						if include {
							return;// If the connection already exists, return
						}
						else {
							index_to_remove_opt = Some(conn_i);
						}
					}
				}
			}
		}
		if include {
			// Hasn't returned yet, add connection
			self.connections.push(CircuitWideLogicPinReference::ExternalConnection(pin_id.to_owned()));
		}
		else {
			// If don't include and found it, remove it
			if let Some(index_to_remove) = index_to_remove_opt {
				self.connections.remove(index_to_remove);
			}
		}
	}
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinExternalSource {
	/// Even if not connected, it will have its own net
	Net(u64),
	#[default]
	Global
}

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
pub enum LogicConnectionPinInternalSource {
	/// Even if not connected, it will have its own net
	Net(u64),
	#[default]
	ComponentInternal
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LogicConnectionPin {
	pub internal_source: Option<LogicConnectionPinInternalSource>,
	internal_state: LogicState,
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
	fn draw<'a>(&self, draw_parent: &ComponentDrawInfo<'a>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		let external_and_combined_states: Vec<(LogicState, LogicState)> = self.iter_owned_pins(|pin| (pin.external_state.clone(), pin.state()));
		let color = draw.styles.color_from_logic_states(&external_and_combined_states.iter().map(|t|t.1.clone()).collect());
		for i in 0..self.owned_pins.len() {
			draw.draw_polyline(
				vec![
					V2::new(1.1, -0.9),
					V2::new(1.1, 0.9),
					V2::new(2.9, 0.9),
					V2::new(2.9, -0.9),
					V2::new(1.1, -0.9)
				].iter().map(|v| v + V2::new((i*2) as f32, 0.0)).collect(),
				draw.styles.color_from_logic_state(external_and_combined_states[i].1)
			);
			if let Some(value) = external_and_combined_states[i].0.to_bool_opt() {
				draw.text(match value {true => "1", false => "0"}.to_owned(), V2::new((i*2) as f32 + 2.0, 0.0), Align2::CENTER_CENTER, draw.styles.text_color, 1.5, false);
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
					pin_cell.borrow_mut().external_state = driven_opts[i].into();
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
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::ExternalConnection(self.ui_data.position, self.ui_data.direction, self.name.clone(), self.show_name, self.owned_pins.len() as u16)
	}
	fn accept_click(&mut self, local_pos: V2) -> bool {
		let bit_index = ((local_pos.x - 1.0) / 2.0) as isize;
		if bit_index > 0 && bit_index < self.owned_pins.len() as isize {
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

/// Any Logic pin within a circuit
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CircuitWideLogicPinReference {
	ComponentPin(ComponentLogicPinReference),
	ExternalConnection(u64)
}

impl CircuitWideLogicPinReference {
	pub fn is_external(&self) -> bool {
		if let Self::ExternalConnection(_) = self {
			true
		}
		else {
			false
		}
	}
}

/// Any Graphic pin within a circuit
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CircuitWideGraphicPinReference {
	ComponentPin(ComponentGraphicPinReference),
	ExternalConnection(u64)
}

impl CircuitWideGraphicPinReference {
	pub fn is_external(&self) -> bool {
		if let Self::ExternalConnection(_) = self {
			true
		}
		else {
			false
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ComponentLogicPinReference {
	/// Has to be a query for something else (()), not <Box<dyn LogicDevice>> so it will work with serde
	component_id: u64,
	pin_id: u64
}

impl ComponentLogicPinReference {
	pub fn new(component_id: u64, pin_id: u64) -> Self {
		Self {
			component_id,
			pin_id
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ComponentGraphicPinReference {
	/// Has to be a query for something else (()), not <Box<dyn LogicDevice>> so it will work with serde
	pub component_id: u64,
	pub pin_id: u64
}

impl ComponentGraphicPinReference {
	pub fn new(component_id: u64, pin_id: u64) -> Self {
		Self {
			component_id,
			pin_id
		}
	}
}

#[derive(Debug, Clone)]
pub struct GraphicLabel {
	ui_data: UIData,
	text: String,
	/// Relative to parent circuit
	vertical: bool
}

impl GraphicLabel {
	pub fn new() -> Self {
		Self {
			ui_data: UIData::new(IntV2(0, 0), FourWayDir::default(), (V2::zeros(), V2::zeros())),
			text: "New Label".to_owned(),
			vertical: false
		}
	}
	pub fn save(&self) -> GraphicLabelSave {
		GraphicLabelSave {
			pos: self.ui_data.position,
			dir: self.ui_data.direction,
			text: self.text.clone(),
			vertical: self.vertical
		}
	}
	pub fn load(save: GraphicLabelSave) -> Self {
		Self {
			ui_data: UIData::new(save.pos, save.dir, (V2::zeros(), V2::zeros())),
			text: save.text,
			vertical: save.vertical
		}
	}
}

impl GraphicSelectableItem for GraphicLabel {
	fn draw<'a>(&self, draw_parent: &ComponentDrawInfo<'a>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		draw.text(self.text.clone(), V2::zeros(), Align2::CENTER_CENTER, draw.styles.text_color, draw.styles.text_size_grid, false);
	}
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::Direction(self.ui_data.direction),
			SelectProperty::Name(self.text.clone())
		]
	}
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::PositionX(x) => {
				self.ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.ui_data.position.1 = y;
			},
			SelectProperty::Direction(direction) => {
				self.ui_data.direction = direction;
			},
			SelectProperty::Name(new_text) => {
				self.text = new_text;
			}
			_ => {}
		}
	}
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::GraphicLabel(self.save())
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphicLabelSave {
	pos: IntV2,
	dir: FourWayDir,
	text: String,
	/// Relative to parent circuit
	vertical: bool
}

/// Bus splitter with exact same functionality as in CircuitVerse
/// Not implemented as a component so that both ends can share the same net and make computation faster
/// Example Geometry
///     |- 4:7
///     |- 0:3
/// 0:7-|
///+ <- (0, 0)
#[derive(Debug, Clone)]
pub struct Splitter {
	ui_data: UIData,
	pub bit_width: u16,
	/// Each entry represents a graphic pin: Vec<(bit width, Option<(Grahic wire connection set, Vec of nets corresponding to bit width)>)>
	pub splits: Vec<(u16, Option<Rc<RefCell<HashSet<WireConnection>>>>)>,
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
	pub fn get_bit_index_from_pin_i_and_wire_bit_index(&self, pin_i: u16, wire_bit_index: u16) -> u16 {
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
	fn draw<'a>(&self, draw_parent: &ComponentDrawInfo<'a>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		draw.draw_polyline(
			vec![
				V2::new(-2.0, -1.0),
				V2::new(-1.0, -1.0),
				V2::new(0.0, 0.0),
				V2::new(0.0, self.splits.len() as f32 - 1.0)
			],
			draw.styles.color_foreground
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
				draw.styles.color_foreground
			);
			let text: String = match split.0 == 1 {
				true => beginning_index.to_string(),
				false => format!("{}:{}", beginning_index, beginning_index + split.0 - 1)
			};
			draw.text(text, V2::new(2.0, i_f32 + 0.5), Align2::CENTER_CENTER, draw.styles.text_color, 0.8, !draw.direction.is_horizontal());
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
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::BitWidth(self.bit_width),
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::Direction(self.ui_data.direction),
			SelectProperty::SplitterSplits(self.splits.iter().map(|t| t.0).collect())
		]
	}
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

/// Just a straight segment, either horizontal or vertical
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Wire {
	ui_data: UIData,
	length: u32,
	pub nets: Vec<u64>,
	color: [u8; 3],
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
	fn draw<'a>(&self, draw: &ComponentDrawInfo<'a>) {
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
			draw.draw_circle_filled(start_pos, draw.styles.connection_dot_grid_size, self.color);
		}
		if self.end_connections.borrow().len() >= 3 {
			draw.draw_circle_filled(end_pos, draw.styles.connection_dot_grid_size, self.color);
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
	fn get_properties(&self) -> Vec<SelectProperty> {
		Vec::new()
	}
	fn set_property(&mut self, _property: SelectProperty) {}
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

/// Read-only probe, used for the timing diagram
#[derive(Debug, Default, Clone)]
pub struct Probe {
	ui_data: UIData,
	name: String,
	nets_opt: Vec<Option<u64>>,
	/// Wrt grid
	text_len: f32
}

impl Probe {
	pub fn load(save: ProbeSave) -> Self {
		Self {
			ui_data: UIData::new(save.0, save.1, Self::get_bb(save.3)),
			name: save.2,
			nets_opt: vec![None],
			text_len: save.3
		}
	}
	pub fn save(&self) -> ProbeSave {
		(self.ui_data.position, self.ui_data.direction, self.name.clone(), self.text_len)
	}
	fn get_bb(text_len: f32) -> (V2, V2) {
		(V2::new(1.0, -1.0), V2::new(2.0 + text_len, 1.0))
	}
}

impl GraphicSelectableItem for Probe {
	fn get_ui_data(&self) -> &UIData {
		&self.ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.ui_data
	}
	fn draw<'a>(&self, draw_parent: &ComponentDrawInfo<'a>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.ui_data.position, self.ui_data.direction);
		let text_length: f32 = draw.text_size(self.name.clone(), 1.0).x;
		let half_height: f32 = 0.7;
		draw.draw_polyline(
			vec![
				V2::new(0.0, 0.0),
				V2::new(1.0, 0.0),
				V2::new(1.0 + half_height, -half_height),
				V2::new(1.0 + text_length + half_height, -half_height),
				V2::new(1.0 + text_length + half_height*2.0, 0.0),
				V2::new(1.0 + text_length + half_height, half_height),
				V2::new(1.0 + half_height, half_height),
				V2::new(1.0, 0.0),
			],
			draw.styles.color_foreground
		);
		let probe_text_start: f32 = match draw.direction {
			FourWayDir::E => text_length/2.0,
			FourWayDir::N => text_length,
			FourWayDir::W => text_length/2.0,
			FourWayDir::S =>0.0
		};
		draw.text(self.name.clone(), V2::new(1.0 + half_height + probe_text_start, 0.0), Align2::CENTER_CENTER, draw.styles.text_color, 1.0, !self.ui_data.direction.is_horizontal());
	}
	fn get_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::PositionX(self.ui_data.position.0),
			SelectProperty::PositionY(self.ui_data.position.1),
			SelectProperty::Direction(self.ui_data.direction),
			SelectProperty::Name(self.name.clone())
		]
	}
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::PositionX(x) => {
				self.ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.ui_data.position.1 = y;
			},
			SelectProperty::Direction(direction) => {
				self.ui_data.direction = direction;
			},
			SelectProperty::Name(new_name) => {
				self.name = new_name;
			}
			_ => {}
		}
	}
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Probe(self.save())
	}
}

/// (Position, Direction, Name, Name length wrt grid)
pub type ProbeSave = (IntV2, FourWayDir, String, f32);

/// Only essential things like position/orientation, logic state
/// Not used for saving circuits
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct LogicDeviceSave {
	pin_states: HashMap<u64, LogicState>,
	pos: IntV2,
	dir: FourWayDir,
	bit_width: Option<u16>,
	name: String
}

/// It is recommended for anything implementing the trait `LogicDevice` to have a field for this
#[derive(Debug, Clone)]
pub struct LogicDeviceGeneric {
	pub logic_pins: Rc<RefCell<HashMap<u64, RefCell<LogicConnectionPin>>>>,
	pub graphic_pins: RefCell<HashMap<u64, GraphicPin>>,
	pub ui_data: UIData,
	pub name: String,
	pub bit_width: Option<u16>,
	pub show_name: bool
}

impl LogicDeviceGeneric {
	pub fn load(
		save: LogicDeviceSave,
		graphic_pin_config: HashMap<u64, (IntV2, FourWayDir, f32, String, bool, Vec<u64>)>,
		bounding_box: (V2, V2),
		show_name: bool,
		are_states_external: bool
	) -> Self {
		let logic_pins = Rc::new(RefCell::new(HashMap::<u64, RefCell<LogicConnectionPin>>::new()));
		let mut logic_pins_mut = logic_pins.borrow_mut();
		let mut graphic_pins = HashMap::<u64, GraphicPin>::new();
		// Create graphic pins and logic pins
		for (pin_id, config) in graphic_pin_config {
			// Create necessary logic pin(s)
			assert!(config.5.len() > 0, "Graphic pin must have at least one logical pin attached to it");
			for logic_pin_id in &config.5 {
				let mut logic_pin = LogicConnectionPin::new(None, None);
				if are_states_external {
					logic_pin.external_state = match save.pin_states.get(&logic_pin_id) {
						Some(state) => *state,
						None => LogicState::Floating
					};
				}
				else {
					// External source will be set by circuit based on geometry
					#[cfg(feature = "restore_pin_states")]
					{
						logic_pin.internal_state = match save.pin_states.get(&logic_pin_id) {
							Some(state) => *state,
							None => LogicState::Floating
						};
					}
					logic_pin.internal_source = Some(LogicConnectionPinInternalSource::ComponentInternal);
				}
				logic_pins_mut.insert(*logic_pin_id, RefCell::new(logic_pin));
			}
			let graphic_pin = GraphicPin::new(Rc::clone(&logic_pins), config.5, config.0, config.1, config.2, config.3, config.4);
			graphic_pins.insert(pin_id.clone(), graphic_pin);
		}
		Self {
			logic_pins: Rc::clone(&logic_pins),
			graphic_pins: RefCell::new(graphic_pins),
			ui_data: UIData::new(save.pos, save.dir, bounding_box),
			name: save.name,
			bit_width: save.bit_width,
			show_name
		}
	}
	pub fn save(&self) -> LogicDeviceSave {
		let mut pin_states = HashMap::<u64, LogicState>::new();
		let pins = self.logic_pins.borrow();
		for (pin_id, pin_cell) in pins.iter() {
			pin_states.insert(*pin_id, pin_cell.borrow().internal_state);
			
		}
		LogicDeviceSave {
			pin_states,
			pos: self.ui_data.position,
			dir: self.ui_data.direction,
			bit_width: self.bit_width,
			name: self.name.clone()
		}
	}
}

/// Could be a simple gate, or something more complicated like an adder, or maybe even the whole computer
pub trait LogicDevice: Debug + GraphicSelectableItem where Self: 'static {
	fn get_generic(&self) -> &LogicDeviceGeneric;
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric;
	fn compute_step(&mut self, ancestors: &AncestryStack, self_component_id: u64, clock_state: bool, first_propagation_step: bool);
	fn save(&self) -> Result<EnumAllLogicDevices, String>;
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>);
	/// In CircuitVerse there can be, for example, one AND gate that acts like 8 gates, with 8-bit busses going in and out of it
	fn get_bit_width(&self) -> Option<u16> {None}
	#[allow(unused)]
	fn set_bit_width(&mut self, bit_width: u16) {}
	fn is_toplevel_circuit(&self) -> bool {false}
	fn is_circuit(&self) -> bool {false}
	/// Everything besides what `impl<T: LogicDevice> GraphicSelectableItem for T::get_properties()` generates
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {Vec::new()}
	/// Everything besides what `impl<T: LogicDevice> GraphicSelectableItem for T::set_property()` accepts
	fn device_set_special_select_property(&mut self, property: SelectProperty) {drop(property);}// So that unused variable warning doesn't happen
	/// A circuit might override this to instead use the block display pin locations
	fn get_pin_position_override(&self, pin_id: u64) -> Option<(IntV2, FourWayDir, f32)> {
		self.get_pin_position(pin_id)
	}
	/// DO NOT DIRECTLY CALL THIS
	fn get_pin_position(&self, graphic_pin_id: u64) -> Option<(IntV2, FourWayDir, f32)> {
		match self.get_generic().graphic_pins.borrow().get(&graphic_pin_id) {
			Some(pin) => {
				Some((pin.ui_data.position, pin.ui_data.direction, pin.length))
			},
			None => None
		}
	}
	fn compute(&mut self, ancestors: &AncestryStack, self_component_id: u64, clock_state: bool, first_propagation_step: bool) {
		//for _ in 0..self.get_generic().sub_compute_cycles {
			self.compute_step(ancestors, self_component_id, clock_state, first_propagation_step);
		//}
	}
	fn get_circuit(&self) -> &LogicCircuit {
		panic!("LogicDevice::get_circuit only works on the LogicCircuit class which overrides it");
	}
	fn get_circuit_mut(&mut self) -> &mut LogicCircuit {
		panic!("LogicDevice::get_circuit_mut only works on the LogicCircuit class which overrides it");
	}
	fn set_logic_pin_external_state(
		&mut self,
		pin_id: u64,
		state: LogicState
	) -> Result<(), String> {
		let generic = self.get_generic_mut();
		let pins = &generic.logic_pins;
		let mut pins_borrow_mut = pins.borrow_mut();
		let pin: &mut LogicConnectionPin = pins_borrow_mut.get_mut(&pin_id).expect(&format!("Pin ID {} does not work on logic device \"{}\"", pin_id, generic.name)).get_mut();
		pin.set_drive_external(state);
		Ok(())
	}
	fn get_logic_pins_cell(&self) -> &RefCell<HashMap<u64, RefCell<LogicConnectionPin>>> {
		&self.get_generic().logic_pins
	}
	/*fn query_pin_mut(&mut self, pin_id: &str) -> Option<&mut LogicConnectionPin> {
		let generic = self.get_generic_mut();
		match generic.pins.get_mut(pin_id) {
			Some(pin_cell) => ,
			None => None
		}
	}*/
	fn set_all_logic_pin_states(&mut self, states: Vec<(u64, LogicState, LogicDriveSource)>) -> Result<(), String> {
		for (pin_query, state, _source) in states {
			self.set_logic_pin_external_state(pin_query, state)?;
		}
		Ok(())
	}
	fn get_pin_state_panic(&self, pin_query: u64) -> LogicState {
		self.get_logic_pins_cell().borrow().get(&pin_query).expect(&format!("Pin query {:?} for logic device \"{}\" not valid", &pin_query, &self.get_generic().name)).borrow().state()
	}
	fn set_pin_internal_state_panic(&mut self, pin_query: u64, state: LogicState) {
		self.get_logic_pins_cell().borrow_mut().get_mut(&pin_query).expect(&format!("Pin query {:?} not valid", &pin_query)).borrow_mut().internal_state = state;
	}
	fn into_box(self: Box<Self>) -> Box<dyn LogicDevice> where Self: Sized {
		self as Box<dyn LogicDevice>
	}
}

/// Everything that implements `Component` also automatically works with the graphics
impl<T: LogicDevice> GraphicSelectableItem for T {
	fn draw<'a>(&self, draw_parent: &ComponentDrawInfo<'a>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.get_generic().ui_data.position, self.get_generic().ui_data.direction);
		self.draw_except_pins(&draw);
		for (pin_id, pin) in self.get_generic().graphic_pins.borrow().iter() {
			let position: (IntV2, FourWayDir, f32) = self.get_pin_position_override(*pin_id).unwrap();
			let global_dir = position.1.rotate_intv2(draw.direction.to_unit_int()).is_along_axis().unwrap();
			let vertical = global_dir == FourWayDir::N || global_dir == FourWayDir::S;
			if !self.is_toplevel_circuit() {
				draw.draw_polyline(
					vec![
						position.0.to_v2(),
						position.0.to_v2() - (position.1.to_unit() * position.2)
					],
					pin.get_color(&draw.styles)
				);
				if pin.show_name {
					draw.text(
						pin.name.clone(),//"test".to_owned(),
						position.0.to_v2() - (position.1.to_unit()*1.2),
						global_dir.to_egui_align2(),
						draw.styles.text_color,
						draw.styles.text_size_grid,
						vertical
					);
				}
			}
			else {
				if pin.show_name {
					draw.text(
						pin.name.clone(),//"test".to_owned(),
						position.0.to_v2() + (position.1.to_unit()*3.2),
						global_dir.opposite_direction().to_egui_align2(),
						draw.styles.text_color,
						draw.styles.text_size_grid,
						vertical
					);
				}
			}
		}
	}
	fn get_ui_data(&self) -> &UIData {
		&self.get_generic().ui_data
	}
	fn get_ui_data_mut(&mut self) -> &mut UIData {
		&mut self.get_generic_mut().ui_data
	}
	// TODO: fix
	fn bounding_box(&self, grid_offset: V2) -> (V2, V2) {
		let local_bb: (V2, V2) = if self.is_circuit() {
			if self.get_circuit().displayed_as_block {
				self.get_generic().ui_data.local_bb
			}
			else {
				self.get_circuit().circuit_internals_bb
			}
		}
		else {
			self.get_generic().ui_data.local_bb
		};
		let ui_data: &UIData = &self.get_generic().ui_data;
		merge_points_to_bb(vec![ui_data.pos_to_parent_coords_float(local_bb.0) + grid_offset, ui_data.pos_to_parent_coords_float(local_bb.1) + grid_offset])
	}
	fn is_connected_to_net(&self, _net_id: u64) -> bool {
		false// Don't highlight a whole component, only wires and pins
	}
	fn get_properties(&self) -> Vec<SelectProperty> {
		let mut out = vec![
			SelectProperty::PositionX(self.get_generic().ui_data.position.0),
			SelectProperty::PositionY(self.get_generic().ui_data.position.1),
			SelectProperty::Direction(self.get_generic().ui_data.direction)
		];
		if self.is_circuit() {
			out.push(SelectProperty::DisplayCircuitAsBlock(self.get_circuit().displayed_as_block));
		}
		if let Some(bit_width) = self.get_bit_width() {
			out.push(SelectProperty::BitWidth(bit_width));
		}
		out.append(&mut self.device_get_special_select_properties());
		out
	}
	fn set_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::BitWidth(bit_width) => {
				self.set_bit_width(bit_width);
			},
			SelectProperty::PositionX(x) => {
				self.get_generic_mut().ui_data.position.0 = x;
			},
			SelectProperty::PositionY(y) => {
				self.get_generic_mut().ui_data.position.1 = y;
			},
			SelectProperty::Direction(dir) => {
				self.get_generic_mut().ui_data.direction = dir;
			},
			SelectProperty::GlobalConnectionState(_) => {},
			SelectProperty::DisplayCircuitAsBlock(block) => {
				if self.is_circuit() {
					self.get_circuit_mut().displayed_as_block = block;
				}
			},
			other => self.device_set_special_select_property(other),
		}
	}
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Component(self.save().unwrap())
	}
}

/// Used to keep track of sub-circuit levels, tracks ancester circuits and their component IDs
/// The toplevel circuit will just put 0 as its ID
#[derive(Clone)]
pub struct AncestryStack<'a>(Vec<(&'a LogicCircuit, u64)>);

impl<'a> AncestryStack<'a> {
	pub fn new() -> Self {
		Self(Vec::new())
	}
	pub fn parent(&self) -> Option<(&'a LogicCircuit, u64)> {
		if self.0.len() == 0 {
			None
		}
		else {
			Some(*self.0.last().expect("Ancestor stack should not be empty"))
		}
	}
	pub fn grandparent(&self) -> Option<(&'a LogicCircuit, u64)> {
		if self.0.len() < 2 {
			None
		}
		else {
			Some(self.0[self.0.len() - 2])
		}
	}
	pub fn trim(&self) -> Self {
		if self.0.len() == 0 {
			panic!("Attempt to trim ancestry stack with no items");
		}
		let mut out = self.clone();
		out.0.pop();
		out
	}
	pub fn push(&self, new_node: (&'a LogicCircuit, u64)) -> Self {
		let mut out = self.clone();
		out.0.push(new_node);
		out
	}
	/// IMPORTANT: The first entry here will be ignored when creating the path because it would otherwise be redundant
	pub fn to_sub_circuit_path(&self) -> SubCircuitPath {
		let mut out = Vec::<u64>::new();
		for (i, (_, circuit_id)) in self.0.iter().enumerate() {
			if i ==  0 {
				continue;
			}
			out.push(*circuit_id);
		}
		SubCircuitPath(out)
	}
}

impl<'a> PartialEq for AncestryStack<'a> {
	fn eq(&self, other: &Self) -> bool {
		if self.0.len() != other.0.len() {
			return false;
		}
		for i in 0..self.0.len() {
			if self.0[i].1 != other.0[i].1 {// Compare IDs
				return false;
			}
		}
		return true;
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GraphicSelectableItemRef {
	Component(u64),
	Wire(u64),
	/// Graphic pin, NOT logic pin
	Pin(u64),
	Splitter(u64),
	GraphicLabel(u64),
	Probe(u64)
}

#[derive(Debug, Clone)]
pub struct Clock {
	pub enabled: bool,
	/// If set to 0 then clock will change state as fast a spossible
	pub freq: f32,
	pub last_change: Instant,
	pub state: bool
}

impl Clock {
	pub fn load(enabled: bool, freq: f32, state: bool) -> Self {
		Self {
			enabled,
			freq,
			last_change: Instant::now(),
			state
		}
	}
	pub fn update(&mut self) {
		if self.enabled {
			if self.freq == 0.0 {
				self.state = !self.state;
				self.last_change = Instant::now();
			}
			else if self.last_change.elapsed() > Duration::from_secs_f32(0.5 / self.freq) {// The frequency is based on a whole period, it must change twice per period, so 0.5/f not 1/f
				self.state = !self.state;
				self.last_change = Instant::now();
			}
		}
	}
}

impl Default for Clock {
	fn default() -> Self {
		Self {
			enabled: true,
			freq: 1.0,
			last_change: Instant::now(),
			state: false
		}
	}
}

#[derive(Debug, Clone)]
pub struct TimingDiagram {
	/// List of signal groups and corresponding probe IDs (CLK probe ID is ignored and set to 0), each signal group contains list of signals (one signal per bit width of probe), each signal contains list of samples
	pub signal_groups: Vec<(u64, Vec<Vec<LogicState>>)>,
	pub n_samples: usize,
	pub running: TimingTiagramRunningState
}

impl TimingDiagram {
	pub fn from_probe_id_list(probes: Vec<u64>) -> Self {
		Self {
			signal_groups: vec![0_u64].into_iter().chain(probes.into_iter()).map(|probe_id| (probe_id, vec![vec![]])).collect(),
			n_samples: 0,
			running: TimingTiagramRunningState::Clk
		}
	}
}

/// Manual control or whenever the clock is running
#[derive(Debug, Clone, Default)]
pub enum TimingTiagramRunningState {
	Play,
	Pause,
	#[default]
	Clk
}

#[derive(Debug, Default, Clone)]
pub enum SelectionState {
	/// Just there
	#[default]
	Fixed,
	/// Being dragged by mouse, keeps track of where it started (wrt grid). If there aren't any selected items, then use this to drag a rectangle to select stuff
	/// (
	/// 	Start,
	/// 	Delta,
	/// 	Vector of wires being dragged: (
	/// 		Wire ID,
	/// 		Whether start is selected,
	/// 		Initial position of either start or end based on field 1
	/// 	)
	/// )
	Dragging(V2, V2),
	/// After Paste operation, the pasted stuff will remain selected and following the mouse until a left click
	/// (Current or most recent mouse pos)
	FollowingMouse(V2)
}

#[derive(Debug, Clone)]
pub enum Tool {
	Select {
		selected_graphics: HashSet<GraphicSelectableItemRef>,
		selected_graphics_state: SelectionState
	},
	HighlightNet(Option<u64>),
	/// Wire initially horizontal/vertical rules:
	/// When the firt perpindicular pair is placed it defauts to Horizontal (horiz), meaning the first segment is horizontal and then a vertical one from the end of that to the mouse
	/// Whenever the most recent pair is completely horiz or vert, its initial direction is set to that direction
	PlaceWire {
		/// Each of these represents two perpindicular segments (if the difference between this one's position and the next one's position is perfectly horizontal or vertical then one of the straight segments will just be length zero)
		/// [(Starting pos, Initial direction to get to next position)]
		/// When the actual wires are created, consecutive segments in the same direction will be combined
		perp_pairs: Vec<(IntV2, FourWayDir)>
	}
}

impl Tool {
	/// The tool buttons side bar cannot always be enabled, for example when the component placement ui is active
	pub fn tool_select_allowed(&self) -> bool {
		match &self {
			Self::Select{selected_graphics: _, selected_graphics_state} => match selected_graphics_state {
				SelectionState::Fixed => true,
				SelectionState::Dragging(_, _) => false,
				SelectionState::FollowingMouse(_) => false
			},
			Self::HighlightNet(_) => true,
			Self::PlaceWire{perp_pairs: _} => false,
		}
	}
	pub fn tool_select_ui(&self, _draw: &ComponentDrawInfo) {
		// TODO
	}
}

impl Default for Tool {
	fn default() -> Self {
		Self::Select{selected_graphics: HashSet::new(), selected_graphics_state: SelectionState::default()}
	}
}

#[derive(Debug)]
pub struct LogicCircuit {
	pub generic_device: LogicDeviceGeneric,
	pub components: RefCell<HashMap<u64, RefCell<Box<dyn LogicDevice>>>>,
	pub nets: RefCell<HashMap<u64, RefCell<LogicNet>>>,
	pub wires: RefCell<HashMap<u64, RefCell<Wire>>>,
	pub splitters: RefCell<HashMap<u64, Splitter>>,
	pub labels: RefCell<HashMap<u64, GraphicLabel>>,
	pub save_path: String,
	/// Inspired by CircuitVerse, block-diagram version of circuit
	/// {pin ID: (relative position (ending), direction, whether to show name)}
	pub block_pin_positions: HashMap<u64, (IntV2, FourWayDir, bool)>,
	displayed_as_block: bool,
	/// For UI
	pub tool: RefCell<Tool>,
	is_toplevel: bool,
	/// Bounding box for the circuit, not the block diagram, relative to this circuit
	/// The block diagram BB can be found at `self.generic_device.bounding_box`
	pub circuit_internals_bb: (V2, V2),
	/// For example, "D Latch", not "Register #7"
	pub type_name: String,
	/// For something like a flip flop that might oscillate without ever being stable, use this to fix the sub compute cycles of that circuit so it will ALWAYS be run that many times per cycle of the parent circuit and it's changed state will be ignored
	pub fixed_sub_cycles_opt: Option<usize>,
	self_reload_err_opt: Option<String>,
	pub clock: RefCell<Clock>,
	/// Timing diagram probes
	pub probes: RefCell<HashMap<u64, Probe>>,
	pub timing: RefCell<TimingDiagram>
}

impl LogicCircuit {
	pub fn new(
		components_not_celled: HashMap<u64, Box<dyn LogicDevice>>,
		external_graphic_pin_config: Vec<(IntV2, FourWayDir, f32, String, Vec<u64>)>,
		type_name: String,
		fixed_sub_cycles_opt: Option<usize>,
		wires: HashMap<u64, Wire>,
		save_path: String,
		displayed_as_block: bool,
		is_toplevel: bool
	) -> Result<Self, String> {
		let mut components = HashMap::<u64, RefCell<Box<dyn LogicDevice>>>::new();
		for (ref_, comp) in components_not_celled.into_iter() {
			components.insert(ref_, RefCell::new(comp));
		}
		let mut graphic_pin_config = HashMap::<u64, (IntV2, FourWayDir, f32, String, bool, Vec<u64>)>::new();
		for (i, config) in external_graphic_pin_config.into_iter().enumerate() {
			graphic_pin_config.insert(i as u64, (config.0, config.1, config.2, config.3, true, vec![i as u64]));
		}
		let mut new = Self {
			/*generic_device: LogicDeviceGeneric::new(
				vec_to_u64_keyed_hashmap(external_connections.into_iter().map(|t| (t.0, t.1, t.2, t.3, true)).collect()),
				(V2::zeros(), V2::zeros()),
				sub_compute_cycles,
				displayed_as_block
			),*/
			generic_device: LogicDeviceGeneric::load(LogicDeviceSave::default(), graphic_pin_config, (V2::zeros(), V2::zeros()), displayed_as_block, true),
			components: RefCell::new(components),
			nets: RefCell::new(HashMap::new()),
			wires: RefCell::new(hashmap_into_refcells(wires)),
			splitters: RefCell::new(HashMap::new()),
			labels: RefCell::new(HashMap::new()),
			save_path,
			block_pin_positions: HashMap::new(),
			displayed_as_block,
			tool: RefCell::new(Tool::default()),
			is_toplevel,
			circuit_internals_bb: (V2::zeros(), V2::zeros()),
			type_name,
			fixed_sub_cycles_opt,
			self_reload_err_opt: None,
			clock: RefCell::new(Clock::default()),
			probes: RefCell::new(HashMap::new()),
			timing: RefCell::new(TimingDiagram::from_probe_id_list(Vec::new()))
		};
		new.setup_external_connection_sources();
		new.recompute_default_layout();
		new.check_wire_geometry_and_connections();
		Ok(new)
	}
	pub fn new_mostly_default(
		type_name: String,
		save_path: String,
		toplevel: bool
	) -> Self {
		Self::new(
			HashMap::new(),
			Vec::new(),
			type_name,
			None,
			HashMap::new(),
			save_path,
			false,
			toplevel
		).unwrap()
	}
	pub fn from_save(save: LogicCircuitSave, save_path: String, displayed_as_block: bool, toplevel: bool, pos: IntV2, dir: FourWayDir, name: String) -> Result<Self, String> {
		// Init compnents
		let mut components = HashMap::<u64, RefCell<Box<dyn LogicDevice>>>::new();
		for (ref_, save_comp) in save.components.into_iter() {
			components.insert(ref_, RefCell::new(EnumAllLogicDevices::to_dynamic(save_comp)?));
		}
		// Reconstruct wires
		let mut reconstructed_wires = HashMap::<u64, RefCell<Wire>>::new();
		for (wire_id, wire_geom) in save.wires {
			reconstructed_wires.insert(wire_id, RefCell::new(Wire::new(wire_geom.0, wire_geom.2, wire_geom.1, vec![0], Rc::new(RefCell::new(HashSet::new())), Rc::new(RefCell::new(HashSet::new())))));
		}
		let mut pin_states = HashMap::<u64, LogicState>::new();
		for (pin_id, logic_pin) in save.logic_pins {
			pin_states.insert(pin_id, logic_pin.external_state);
		}
		let generic_device = LogicDeviceGeneric::load(
			LogicDeviceSave {
				pin_states,
				pos,
				dir,
				bit_width: None,
				name
			},
			HashMap::from_iter(save.graphic_pins.into_iter().map(|t| (t.0, (t.1.0, t.1.1, 1.0, t.1.2, t.1.3, t.1.4)))),
			(V2::zeros(), V2::zeros()),
			displayed_as_block,
			true
		);
		let probes = HashMap::from_iter(save.probes.into_iter().map(|(id, save)| (id, Probe::load(save))));
		let timing: TimingDiagram = TimingDiagram::from_probe_id_list(save.timing_probe_order);
		let mut out = Self {
			generic_device: generic_device,
			components: RefCell::new(components),
			nets: RefCell::new(hash_map!(0 => RefCell::new(LogicNet::new(Vec::new())))),
			wires: RefCell::new(reconstructed_wires),
			splitters: RefCell::new(HashMap::from_iter(save.splitters.into_iter().map(|t| -> (u64, Splitter) {(t.0, Splitter::load(t.1))}))),
			labels: RefCell::new(HashMap::from_iter(save.labels.into_iter().map(|t| -> (u64, GraphicLabel) {(t.0, GraphicLabel::load(t.1))}))),
			save_path,
			block_pin_positions: save.block_pin_positions,
			displayed_as_block,
			tool: RefCell::new(Tool::default()),
			is_toplevel: toplevel,
			circuit_internals_bb: (V2::zeros(), V2::zeros()),
			type_name: save.type_name,
			fixed_sub_cycles_opt: save.fixed_sub_cycles_opt,
			self_reload_err_opt: None,
			clock: RefCell::new(Clock::load(save.clock_enabled, save.clock_freq, save.clock_state)),
			probes: RefCell::new(probes),
			timing: RefCell::new(timing)
		};
		out.setup_external_connection_sources();
		out.check_wire_geometry_and_connections();
		out.update_pin_block_positions();
		Ok(out)
	}
	fn setup_external_connection_sources(&mut self) {
		// Get rid of Global pin sources if not toplevel and nets if toplevel
		for (_, pin_cell) in self.generic_device.logic_pins.borrow().iter() {
			let mut pin = pin_cell.borrow_mut();
			if self.is_toplevel {
				pin.external_source = Some(LogicConnectionPinExternalSource::Global);
			}
			else {
				pin.external_source = None;
			}
			pin.internal_source = None;// Will be automatically assigned from wire geometry, remove because of invalid net references
		}
	}
	fn recompute_default_layout(&mut self) {
		// Bounding box & layout, like in CircuitVerse
		let mut count_pins_not_clock: i32 = 0;
		let mut block_pin_positions: HashMap<u64, (IntV2, FourWayDir, bool)> = HashMap::new();
		for (pin_id, _) in self.generic_device.graphic_pins.borrow().iter() {
			if count_pins_not_clock % 2 == 0 {
				block_pin_positions.insert(*pin_id, (IntV2(-(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as i32) - 1, count_pins_not_clock / 2), FourWayDir::W, true));
			}
			else {
				block_pin_positions.insert(*pin_id, (IntV2(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as i32 + 1, count_pins_not_clock / 2), FourWayDir::E, true));
			}
			count_pins_not_clock += 1;
		}
		self.block_pin_positions = block_pin_positions;
		self.generic_device.ui_data.local_bb = (V2::new(-(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as f32), -1.0), V2::new(CIRCUIT_LAYOUT_DEFAULT_HALF_WIDTH as f32, ((count_pins_not_clock / 2) + 1) as f32));
		self.update_pin_block_positions();
	}
	/// Makes sure all pins have a block position and that there are no extra block positions
	pub fn update_pin_block_positions(&mut self) {
		let pins = self.generic_device.graphic_pins.borrow_mut();
		for pin_id in pins.keys() {
			if !self.block_pin_positions.contains_key(pin_id) {
				self.block_pin_positions.insert(pin_id.clone(), (IntV2(0, 0), FourWayDir::default(), true));
			}
		}
		let mut block_positions_to_delete = Vec::<u64>::new();
		for pin_id in self.block_pin_positions.keys() {
			if !pins.contains_key(pin_id) {
				block_positions_to_delete.push(*pin_id);
			}
		}
		for pin_id_to_del in block_positions_to_delete {
			self.block_pin_positions.remove(&pin_id_to_del);
		}
	}
	/// Returns: Whether to recompute the circuit connections
	pub fn toplevel_ui_interact<'a, F: Fn(Pos2) -> V2>(&mut self, response: Response, context: &egui::Context, /*draw: &ComponentDrawInfo<'a>,*/ mut input_state: egui::InputState, grid_size: f32, mouse_pos2_to_grid: F) -> bool {
		let mut return_recompute_connections = false;
		let mut new_tool_opt = Option::<Tool>::None;
		let mut recompute_pin_block_positions = false;
		let mouse_pos_grid_opt: Option<V2> = match response.hover_pos() {
			Some(pos_px) => Some(mouse_pos2_to_grid(pos_px)),
			None => None
		};
		match self.tool.borrow_mut().deref_mut() {
			Tool::Select{selected_graphics, selected_graphics_state} => {
				match selected_graphics_state {
					SelectionState::Fixed => {
						if response.drag_started_by(PointerButton::Primary) {
							let begining_mouse_pos_grid: V2 = mouse_pos_grid_opt.expect("Hover pos should work when dragging");
							*selected_graphics_state =  SelectionState::Dragging(begining_mouse_pos_grid, emath_vec2_to_v2(response.drag_delta()) / grid_size);
							for item_ref in selected_graphics.iter() {
								self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
									graphic_item.start_dragging(begining_mouse_pos_grid);
								});
							}
						}
						if response.clicked() {
							// Find if command/ctrl is being held down
							let multi_select_key: bool = input_state.key_down(Key::A);// TODO: Command / Control
							// Find what was clicked (if anything)
							match self.was_anything_clicked(mouse_pos2_to_grid(response.interact_pointer_pos().expect("Interact pointer pos should work when clicked")), multi_select_key) {
								Some(new_selected_item) => match multi_select_key {
									true => match selected_graphics.contains(&new_selected_item) {
										true => {
											selected_graphics.remove(&new_selected_item);
										},
										false => {
											selected_graphics.insert(new_selected_item);
										}
									},
									false => {
										selected_graphics.clear();
										selected_graphics.insert(new_selected_item);
									}
								},
								None => {
									if !multi_select_key {
										*selected_graphics = HashSet::new();
									}
								}
							}
							//println!("Currently selected items: {:?}", &selected_graphics);
						}
						// Cmd-A for select-all
						if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::COMMAND, Key::A)) {
							*selected_graphics = HashSet::from_iter(self.get_all_graphics_references());
						}
						// W for Wire
						if input_state.consume_key(Modifiers::NONE, Key::W) {
							new_tool_opt = Some(Tool::PlaceWire{perp_pairs: vec![]});
						}
					},
					SelectionState::Dragging(start_grid, delta_grid) => {
						let delta_grid_backwards_y = emath_vec2_to_v2(response.drag_delta()) / grid_size;
						*delta_grid += if cfg!(feature = "reverse_y") {
							v2_reverse_y(delta_grid_backwards_y)
						} else {
							delta_grid_backwards_y
						};
						match selected_graphics.len() {
							0 => {// Drag a rectangle
								let select_bb: (V2, V2) = merge_points_to_bb(vec![*start_grid, *start_grid + *delta_grid]);
								if response.drag_stopped_by(PointerButton::Primary) {
									// Find all items that have BBs intersected by the rectangle and select them
									selected_graphics.clear();
									for item_ref in self.get_all_graphics_references() {
										if self.run_function_on_graphic_item(item_ref.clone(), |graphic_item| -> bool {
											bbs_overlap(graphic_item.bounding_box(V2::zeros()), select_bb)
										}).unwrap_or_else(|| false) {
											selected_graphics.insert(item_ref.clone());
										}
									}
									// Back to fixed selection
									*selected_graphics_state = SelectionState::Fixed
								}
							},
							_ => {// Move stuff
								for item_ref in selected_graphics.iter() {
									self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
										graphic_item.dragging_to(*start_grid + *delta_grid);
									});
								}
								if response.drag_stopped_by(PointerButton::Primary) {
									for item_ref in selected_graphics.iter() {
										self.run_function_on_graphic_item_mut(item_ref.clone(), |graphic_item| {
											graphic_item.stop_dragging(*start_grid + *delta_grid);
										});
									}
									*selected_graphics_state = SelectionState::Fixed;
									return_recompute_connections = true;
								}
							}
						}
					},
					SelectionState::FollowingMouse(mouse_pos) => {
						if let Some(new_mouse_pos) = mouse_pos_grid_opt {// In case the mouse goes off the edge or something idk
							*mouse_pos = new_mouse_pos;
						}
						for item_ref in selected_graphics.iter() {
							self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {item_box.get_ui_data_mut().position = round_v2_to_intv2(*mouse_pos) + item_box.get_ui_data().position_before_dragging;});
						}
						if response.clicked_by(PointerButton::Primary) {
							return_recompute_connections = true;
							new_tool_opt = Some(Tool::default());
						}
					}
				}
				// Copy and Paste will use a plain-text JSON array of instances of the `CopiedGraphicItem` enum
				// Copy
				if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::NONE, Key::C)) {// TODO
					let mut copied_items = Vec::<CopiedGraphicItem>::new();
					// Get combined BB center
					let bb_opt = self.selected_bb(selected_graphics);
					// If there is a BB then at least one selected item to copy, otherwise do nothing
					if let Some(bb) = bb_opt {
						let bb_center: V2 = (bb.1 + bb.0) / 2.0;
						//let bb_int = (IntV2(bb_float.0.x as i32, bb_float.0.y as i32), IntV2(bb_float.1.x as i32, bb_float.1.y as i32));
						for item_ref in selected_graphics.iter() {
							if let Some(copied_item) = self.copy_graphic_item(item_ref.clone()) {
								copied_items.push(copied_item);
							}
						}
						let item_set = CopiedItemSet::new(copied_items, round_v2_to_intv2(bb_center));
						let raw_string = serde_json::to_string(&item_set).unwrap();
						context.copy_text(raw_string);
					}
				}
				// Vaste
				if input_state.consume_shortcut(&KeyboardShortcut::new(Modifiers::NONE, Key::V)) {// TODO
					// Attempt to decode from clipboard
					let mut clipboard = arboard::Clipboard::new().unwrap();
					let string_raw = clipboard.get_text().unwrap();
					match serde_json::from_str::<CopiedItemSet>(&string_raw) {
						Ok(item_set) => {
							let pasted_selected_graphics = self.paste(item_set);
							new_tool_opt = Some(Tool::Select{selected_graphics: HashSet::from_iter(pasted_selected_graphics.into_iter()), selected_graphics_state: SelectionState::FollowingMouse(mouse_pos_grid_opt.unwrap_or_default())});
						},
						Err(_) => {}
					}
					recompute_pin_block_positions = true;
				}
				// Delete
				if input_state.consume_key(Modifiers::NONE, Key::Backspace) {
					for item_ref in selected_graphics.iter() {
						self.remove_graphic_item(item_ref);
					}
					recompute_pin_block_positions = true;
					return_recompute_connections = true;
					*selected_graphics = HashSet::new();
				}
				// Rotate w/ arrow keys
				if input_state.consume_key(Modifiers::NONE, Key::ArrowLeft) {
					self.rotate_selection(selected_graphics, false);
					return_recompute_connections = true;
				}
				if input_state.consume_key(Modifiers::NONE, Key::ArrowRight) {
					self.rotate_selection(selected_graphics, true);
					return_recompute_connections = true;
				}
				// Flip w/ arrow keys
				if input_state.consume_key(Modifiers::COMMAND, Key::ArrowLeft) || input_state.consume_key(Modifiers::COMMAND, Key::ArrowRight) {
					self.flip_selection(selected_graphics, true);
					return_recompute_connections = true;
				}
				if input_state.consume_key(Modifiers::COMMAND, Key::ArrowDown) || input_state.consume_key(Modifiers::COMMAND, Key::ArrowUp) {
					self.flip_selection(selected_graphics, false);
					return_recompute_connections = true;
				}
			},
			Tool::HighlightNet(_net_id) => {
				// TODO
			},
			Tool::PlaceWire{perp_pairs} => {
				if let Some(mouse_pos_grid) = mouse_pos_grid_opt {
					let mouse_pos_grid_rounded: IntV2 = round_v2_to_intv2(mouse_pos_grid);
					let n_pairs = perp_pairs.len();
					// Wire has been started
					if n_pairs >= 1 {
						let latest_pair: &mut (IntV2, FourWayDir) = &mut perp_pairs[n_pairs - 1];
						// Check if perp pair is perfectly horiz or vert
						let v: IntV2 = mouse_pos_grid_rounded - latest_pair.0;
						if let Some(new_dir) = v.is_along_axis() {
							latest_pair.1 = new_dir;
						}
						if response.clicked_by(PointerButton::Primary) {
							// First, check if this is a wire termination point
							let (is_term_point, _, _) = self.is_connection_point(mouse_pos_grid_rounded);
							if is_term_point {
								// End wire
								self.add_wire_geometry(perp_pairs.clone(), mouse_pos_grid_rounded);
								return_recompute_connections = true;
								new_tool_opt = Some(Tool::default());
							}
							else {
								perp_pairs.push((
									mouse_pos_grid_rounded,
									FourWayDir::E
								));
							}
						}
					}
					// Wire not started
					else {
						if response.clicked_by(PointerButton::Primary) {
							perp_pairs.push((
								mouse_pos_grid_rounded,
								FourWayDir::E
							));
						}
					}
				}
				if input_state.consume_key(Modifiers::NONE, Key::Escape) {
					new_tool_opt = Some(Tool::default());
				}
			}
		}
		if let Some(new_tool) = new_tool_opt {
			*self.tool.borrow_mut() = new_tool;
		}
		if recompute_pin_block_positions {
			self.update_pin_block_positions();
		}
		return_recompute_connections
	}
	pub fn show_timing_diagram_ui(&self, ui: &mut Ui, styles: &mut Styles) {
		ui.horizontal(|ui| {
			let mut timing = self.timing.borrow_mut();
			if ui.button("Clear").clicked() {
				timing.n_samples = 0;
				for (_, ref mut signal_group) in &mut timing.signal_groups {
					*signal_group = (0..signal_group.len()).map(|_| vec![]).collect();
				}
			}
			ui.label("Resolution:");
			ui.add(DragValue::new(&mut styles.timing_diagram_resolution_px).clamp_existing_to_range(true).range(RangeInclusive::new(5, 100)));
		});
		let timing = self.timing.borrow();
		ScrollArea::vertical().show(ui, |ui| {
			ui.horizontal(|ui| {
				let mut amplitude: f32 = 10.0;
				let mut vert_spacing: f32 = 25.0;
				// Labels
				let vert_widget_extra_spacing = ui.style().spacing.item_spacing.y;
				ui.vertical(|ui| {
					ui.add_space(8.0);
					let height = ui.label("CLK").rect.height() + vert_widget_extra_spacing;
					if height > vert_spacing {
						let r = height / vert_spacing;
						vert_spacing /= r;
						amplitude /= r;
					}
					ui.add_space(vert_spacing - height);
					let probes = self.probes.borrow();
					for (i, (probe_id, _)) in timing.signal_groups.iter().enumerate() {
						if i == 0 {
							continue;
						}
						let height = ui.label(&probes.get(probe_id).unwrap().name).rect.height() + vert_widget_extra_spacing;
						ui.add_space(vert_spacing - height);
					}
				});
				// Signals
				let wavelength = styles.timing_diagram_resolution_px as f32;
				if timing.n_samples > 0 {
					ScrollArea::horizontal().stick_to_right(true).show(ui, |ui| {
						Frame::canvas(ui.style()).show::<()>(ui, |ui| {
							let canvas_size = Vec2::new(timing.n_samples as f32 * wavelength + 4.0, timing.signal_groups.len() as f32 * vert_spacing);
							let (response, painter) = ui.allocate_painter(canvas_size, Sense::empty());
							let logic_state_to_graph_y_and_color = |state: LogicState| -> (f32, [u8; 3]) {
								match state {
									LogicState::Floating => (0.0, styles.color_wire_floating),
									LogicState::Contested => (0.0, styles.color_wire_contested),
									LogicState::Driven(bit) => match bit {
										true => (amplitude, styles.color_foreground),
										false => (-amplitude, styles.color_foreground)
									}
								}
							};
							let graph_pos_to_canvas_pos = |graph_x: f32, graph_y: f32, group_i: usize| -> Pos2 {
								Pos2::new(graph_x + response.rect.left() + 2.0, (-graph_y) + (group_i as f32 + 0.5)*vert_spacing + response.rect.top())
							};
							for (group_i, (_, signal_group)) in timing.signal_groups.iter().enumerate() {
								//let (response, painter) = ui.allocate_painter(Vec2::new(0.0, 20.0), Sense::empty());
								// Iterate signal samples
								let mut prev_sample: Vec<LogicState> = signal_group.iter().map(|signal| signal[0]).collect();
								for sample_i in 0..timing.n_samples {
									let current_sample: Vec<LogicState> = signal_group.iter().map(|signal| signal[sample_i]).collect();
									let sample_i_f32: f32 = sample_i as f32;
									assert!(current_sample.len() > 0, "Signal group must have at least one bit");
									if current_sample.len() == 1 {
										let (current_y, color) = logic_state_to_graph_y_and_color(current_sample[0]);
										let stroke = Stroke::new(1.0, u8_3_to_color32(color));
										if prev_sample[0] != current_sample[0] {// Vertical connection line if states are different
											let (prev_y, _) = logic_state_to_graph_y_and_color(prev_sample[0]);
											painter.line_segment([graph_pos_to_canvas_pos(sample_i_f32*wavelength, prev_y, group_i), graph_pos_to_canvas_pos(sample_i_f32*wavelength, current_y, group_i)], stroke);
										}
										painter.line_segment([graph_pos_to_canvas_pos(sample_i_f32*wavelength, current_y, group_i), graph_pos_to_canvas_pos((sample_i_f32+1.0)*wavelength, current_y, group_i)], stroke);
									}
									else {
										// TODO
									}
									prev_sample = current_sample;
								}
							}
						});
					});
				}
			});
		});
	}
	pub fn paste(&self, item_set: CopiedItemSet) -> Vec<GraphicSelectableItemRef> {
		let mut out = Vec::<GraphicSelectableItemRef>::new();
		for pasted_item in item_set.items {
			out.push(match pasted_item {
				CopiedGraphicItem::Component(comp_save) => self.insert_component(&comp_save),
				CopiedGraphicItem::ExternalConnection(pos, dir, name, show_name, bit_width) => {
					GraphicSelectableItemRef::Pin(self.insert_graphic_pin(pos, dir, name, show_name, bit_width))
				},
				CopiedGraphicItem::Wire((pos, dir, len)) => {
					let wire_ids = self.add_wire_geometry(vec![(pos, dir)], pos + dir.to_unit_int().mult(len as i32));
					GraphicSelectableItemRef::Wire(wire_ids[0])// There shoud be exactly one
				},
				CopiedGraphicItem::Splitter(save) => self.insert_splitter(save),
				CopiedGraphicItem::GraphicLabel(save) => self.insert_label(save),
				CopiedGraphicItem::Probe(save) => self.insert_probe(save)
			});
		}
		// Set each item's pre-drag position to the difference from the BB center it it's position
		for item_ref in &out {
			self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {item_box.get_ui_data_mut().position_before_dragging = item_box.get_ui_data().position - item_set.bb_center;});
		}
		out
	}
	pub fn insert_graphic_pin(&self, pos: IntV2, dir: FourWayDir, name: String, show_name: bool, bit_width: u16) -> u64 {
		let mut graphic_pins = self.generic_device.graphic_pins.borrow_mut();
		let new_pin_id = lowest_unused_key(&*graphic_pins);
		// Create new logical pins for graphic pin
		let mut owned_pins = Vec::<u64>::new();
		let mut logic_pins = self.get_logic_pins_cell().borrow_mut();
		for _ in 0..bit_width {
			let new_logic_pin_id = lowest_unused_key(&*logic_pins);
			owned_pins.push(new_logic_pin_id);
			logic_pins.insert(new_logic_pin_id, RefCell::new(LogicConnectionPin::new(None, Some(LogicConnectionPinExternalSource::Global))));
		}
		graphic_pins.insert(new_pin_id, GraphicPin::new(Rc::clone(&self.generic_device.logic_pins), owned_pins, pos, dir, 1.0, name, show_name));
		new_pin_id
	}
	pub fn insert_component(&self, comp_save: &EnumAllLogicDevices) -> GraphicSelectableItemRef {
		let mut components = self.components.borrow_mut();
		let new_comp_id = lowest_unused_key(&components);
		components.insert(new_comp_id, RefCell::new(EnumAllLogicDevices::to_dynamic(comp_save.clone()).unwrap()));
		GraphicSelectableItemRef::Component(new_comp_id)
	}
	pub fn insert_splitter(&self, splitter: SplitterSave) -> GraphicSelectableItemRef {
		let mut splitters = self.splitters.borrow_mut();
		let new_id = lowest_unused_key(&splitters);
		splitters.insert(new_id, Splitter::load(splitter));
		GraphicSelectableItemRef::Splitter(new_id)
	}
	pub fn insert_label(&self, label: GraphicLabelSave) -> GraphicSelectableItemRef {
		let mut labels = self.labels.borrow_mut();
		let new_id = lowest_unused_key(&labels);
		labels.insert(new_id, GraphicLabel::load(label));
		GraphicSelectableItemRef::GraphicLabel(new_id)
	}
	pub fn insert_probe(&self, probe: ProbeSave) -> GraphicSelectableItemRef {
		let mut probes = self.probes.borrow_mut();
		let new_id = lowest_unused_key(&probes);
		probes.insert(new_id, Probe::load(probe));
		GraphicSelectableItemRef::Probe(new_id)
	}
	pub fn set_graphic_item_following_mouse(&self, item_ref: GraphicSelectableItemRef) {
		*self.tool.borrow_mut() = Tool::Select{
			selected_graphics: HashSet::from_iter(vec![item_ref].into_iter()),
			selected_graphics_state: SelectionState::FollowingMouse(V2::zeros())
		};
	}
	fn selected_bb(&self, selected_graphics: &HashSet<GraphicSelectableItemRef>) -> Option<(V2, V2)> {
		let mut bb_opt = Option::<(V2, V2)>::None;
		for item_ref in selected_graphics.iter() {
			if let Some(bb_float) = self.run_function_on_graphic_item::<(V2, V2)>(item_ref.clone(), |item_box| item_box.bounding_box(V2::zeros())) {
				bb_opt = Some(match bb_opt.clone() {
					Some(bb) => {
						merge_points_to_bb(vec![bb_float.0, bb_float.1, bb.0, bb.1])
					}
					None => bb_float
				});
			}
		}
		bb_opt
	}
	pub fn rotate_selection(&self, selected_graphics: &HashSet<GraphicSelectableItemRef>, cw: bool) {
		// Only do anything of there's at least one thing selected
		if let Some(bb) = self.selected_bb(selected_graphics) {
			let bb_center: IntV2 = round_v2_to_intv2((bb.1 + bb.0) / 2.0);
			for item_ref in selected_graphics.iter() {
				self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {
					let ui_data: &mut UIData = item_box.get_ui_data_mut();
					let rotate_dir: FourWayDir = if cw {
						ui_data.direction = ui_data.direction.turn_cw();
						FourWayDir::S
					}
					else {
						ui_data.direction = ui_data.direction.turn_ccw();
						FourWayDir::N
					};
					ui_data.position = rotate_dir.rotate_intv2(ui_data.position - bb_center) + bb_center;
					ui_data.position_before_dragging = rotate_dir.rotate_intv2(ui_data.position_before_dragging);
				});
			}
		}
	}
	/// Flips anything with direction W or E if horiz, otherwise anything with N or S
	pub fn flip_selection(&self, selected_graphics: &HashSet<GraphicSelectableItemRef>, horiz: bool) {
		// Only do anything of there's at least one thing selected
		if let Some(bb) = self.selected_bb(selected_graphics) {
			let bb_center: IntV2 = round_v2_to_intv2((bb.1 + bb.0) / 2.0);
			for item_ref in selected_graphics.iter() {
				self.run_function_on_graphic_item_mut(item_ref.clone(), |item_box| {
					let ui_data: &mut UIData = item_box.get_ui_data_mut();
					// Fixed by Gemini
					if horiz {
						// Check if the item has a horizontal direction to flip
						if ui_data.direction == FourWayDir::E || ui_data.direction == FourWayDir::W {
							// Flip the item's direction (e.g., East becomes West)
							ui_data.direction = ui_data.direction.opposite_direction();
						}
						// Reflect the item's absolute position across the center's X-coordinate
						ui_data.position.0 = 2 * bb_center.0 - ui_data.position.0;
						// Reflect the relative dragging offset by negating its X-component
						ui_data.position_before_dragging.0 = -ui_data.position_before_dragging.0;
					} else { // Vertical flip
						// Check if the item has a vertical direction to flip
						if ui_data.direction == FourWayDir::S || ui_data.direction == FourWayDir::N {
							// Flip the item's direction (e.g., North becomes South)
							ui_data.direction = ui_data.direction.opposite_direction();
						}
						// Reflect the item's absolute position across the center's Y-coordinate
						ui_data.position.1 = 2 * bb_center.1 - ui_data.position.1;
						// Reflect the relative dragging offset by negating its Y-component
						ui_data.position_before_dragging.1 = -ui_data.position_before_dragging.1;
					}
				});
			}
		}
	}
	/// Checks: All wires, external pins, component pins
	/// Returns: (Is termination point, Vec<Optional nets corresponding to bit width>, Optional shared wire connection set)
	fn is_connection_point(&self, point: IntV2) -> (bool, Vec<Option<u64>>, Option<Rc<RefCell<HashSet<WireConnection>>>>) {
		if let Some((_, net_ids, _, connections_opt)) = self.is_point_on_wire(point, None) {
			return (true, net_ids.iter().map(|id| Some(*id)).collect(), connections_opt);
		}
		for (_, graphic_pin) in self.generic_device.graphic_pins.borrow().iter() {
			if graphic_pin.ui_data.position == point {
				return (
					true,
					graphic_pin.internal_sources().iter().map(|source_opt| {
						match source_opt {
							Some(source) => match source {
								LogicConnectionPinInternalSource::Net(net_id) => Some(*net_id),
								LogicConnectionPinInternalSource::ComponentInternal => panic!("Circuit external connection should not have ComponentInternal source")
							},
							None => None
						}
					}).collect(),
					None
				);
			}
		}
		for (_, comp_cell) in self.components.borrow().iter() {
			let comp = comp_cell.borrow();
			for (pin_id, graphic_pin) in comp.get_generic().graphic_pins.borrow().iter() {
				let pin_pos_wrt_comp = comp.get_pin_position_override(*pin_id).unwrap().0;
				if comp.get_ui_data().pos_to_parent_coords(pin_pos_wrt_comp) == point {
					return (
						true,
						graphic_pin.external_sources().iter().map(|source_opt| {
							match source_opt {
								Some(source) => match source {
									LogicConnectionPinExternalSource::Net(net_id) => Some(*net_id),
									LogicConnectionPinExternalSource::Global => panic!("Component connection should be to a Net and not global")
								},
								None => None
							}
						}).collect(),
						None
					);
				}
			}
		}
		for splitter in self.splitters.borrow().values() {
			let point_splitter_local = splitter.ui_data.parent_pos_to_local_coords(point);
			if let Some(return_t) = splitter.is_connection_point(point_splitter_local) {
				return (true, return_t.0, return_t.1);
			}
		}
		(false, vec![None], None)
	}
	/// Returns: Option<(Wire ID, Vec<Net IDs>, Wire intrcept triple, Optional end connection set)>
	fn is_point_on_wire(&self, point: IntV2, wire_to_exclude_opt: Option<u64>) -> Option<(u64, Vec<u64>, (bool, bool, bool), Option<Rc<RefCell<HashSet<WireConnection>>>>)> {
		for (wire_id, wire) in self.wires.borrow().iter() {
			if let Some(wire_to_exclude) = wire_to_exclude_opt {
				if *wire_id == wire_to_exclude {
					continue;
				}
			}
			let (bool_triple, wire_connections_opt) = wire.borrow().contains_point(point);
			if bool_triple.0 || bool_triple.1 || bool_triple.2 {
				return Some((*wire_id, wire.borrow().nets.clone(), bool_triple, wire_connections_opt));
			}
		}
		None
	}
	/// Adds new wires to circuit, `perp_segment_pairs` works the same as described in `Tool::PlaceWire`
	/// Does not take care of wire connections and nets
	/// Returns: Vec of new wire IDs
	fn add_wire_geometry(
		&self,
		perp_segment_pairs: Vec<(IntV2, FourWayDir)>,
		ending_pos: IntV2
	) -> Vec<u64> {
		// Create vec of bare wire geometries
		let mut wires = self.wires.borrow_mut();
		let mut new_wire_segment_geometries = Vec::<(IntV2, FourWayDir, u32)>::new();
		for (i, pair) in perp_segment_pairs.iter().enumerate() {
			let end_pos = if i == perp_segment_pairs.len() - 1 {
				ending_pos
			}
			else {
				perp_segment_pairs[i+1].0
			};
			let segments = Wire::perpindicular_pair_to_segments(pair, end_pos);
			for segment in segments {
				new_wire_segment_geometries.push(segment);
			}
		}
		// Create new wire IDs now that the number of wire segments is known
		let new_wire_ids: Vec<u64> = batch_unused_keys(&*wires, new_wire_segment_geometries.len());
		// Place down as actual wires
		for (i, segment) in new_wire_segment_geometries.iter().enumerate() {
			let new_wire_id: u64 = new_wire_ids[i];
			let new_wire = Wire::new(
				segment.0, segment.2,
				segment.1,
				vec![0],
				Rc::new(RefCell::new(HashSet::new())),
				Rc::new(RefCell::new(HashSet::new()))
			);
			wires.insert(new_wire_id, RefCell::new(new_wire));
		}
		new_wire_ids
	}
	/// Fixes everything, should be run when a new circuit is created or when anything is moved, deleted, or placed
	pub fn check_wire_geometry_and_connections(&mut self) {
		// Find overlapping wires and correct them, connections can be ignored
		self.merge_overlapping_wires();
		// Remove all wire connections except to themselves
		self.check_wires_connected_to_just_themselves();
		// Combine overlapping but seperate end connection and T-connection HashSets
		self.combine_overlapping_wire_connection_sets();
		// Remove all connections from nets, the legit ones will be added back later
		//self.remove_net_connections();
		// Everything so far just deals with wires, now update pin connections to the wires, possibly changing pin nets
		self.update_graphical_pin_to_wire_connections();
		// Graphical connections b/w wires and splitters
		self.update_splitter_wire_connections();
		// Combine consecutive segments in the same direction
		self.merge_consecutive_wires();
		// Compute nets, has to be done after all geometry and connection fixes
		self.recompute_nets();// TODO Use result
		// Logic probes
		self.update_probe_net_connections_and_timing();
		// Last net computation setep
		self.update_logical_pin_to_wire_connections();
		// Recompute BB for circuit internals
		self.recompute_internals_bb();
	}
	/// Ignores connections, just checks every wire against every other one, so O(n^2)
	fn merge_overlapping_wires(&self) {
		// If there are 3 or more overlapping wires, use this to keep track of the "original"
		// If a wire is a key in this then go to its value, check if that's a key then keep going... until it isn't then that is the original wire and all others will be deleted
		// {Wire to be deleted: Wire that will be kept (for now)}
		let mut wires_big_daddy = HashMap::<u64, u64>::new();
		let mut wires = self.wires.borrow_mut();
		for (wire_id_1, wire_cell_1) in wires.iter() {
			if wires_big_daddy.contains_key(wire_id_1) {
				continue;
			}
			for (wire_id_2, wire_cell_2) in wires.iter() {
				if wire_id_1 <= wire_id_2 {
					continue;
				}
				let mut wire_1 = wire_cell_1.borrow_mut();
				let wire_2 = wire_cell_2.borrow();
				// Determine if overlapping
				let same_forward = wire_1.ui_data.direction == wire_2.ui_data.direction;
				let same_backward = wire_1.ui_data.direction == wire_2.ui_data.direction.opposite_direction();
				if same_forward || same_backward {
					// Now compare difference in start positions and see if that is along a parallel axis
					if let Some(axis_1_to_2) = (wire_2.ui_data.position - wire_1.ui_data.position).is_along_axis() {
						if axis_1_to_2 == wire_1.ui_data.direction || axis_1_to_2 == wire_1.ui_data.direction.opposite_direction() {
							// Now we know wires are colinear, find if they're diddling eachother
							// Project them onto wire 1 axis, Thinking of Mr Byron (Beeran Ziddy) Ramirez!
							let axis_unit = wire_1.ui_data.direction.to_unit_int();// Use this instead of `axis_1_to_2` so that overlapping check is simpler
							let start_1_global = wire_1.ui_data.position;
							// All relative to wire 1 start
							let start_1 = 0;
							let end_1 = (wire_1.ui_data.direction.to_unit_int().mult(wire_1.length as i32)).dot(axis_unit);
							let start_2 = (wire_2.ui_data.position - start_1_global).dot(axis_unit);
							let end_2 = (wire_2.end_pos() - start_1_global).dot(axis_unit);
							if (start_1 < start_2 && start_2 < end_1) || (start_1 < end_2 && end_2 < end_1) {
								// There is overlap (just the edges connecting is fine, and consecutive wires w/o T connections are handled by a different function)
								// Check if wire 2 is already marked to be deleted
								// Wrt wire 1 start
								// If wire 2 already has a daddy then wire 1 becomes its granpa
								let mut daddy_id: u64 = *wire_id_2;
								loop {
									if let Some(new_target) = wires_big_daddy.get(&daddy_id) {
										daddy_id = *new_target;
									}
									else {
										break;
									}
								}
								if daddy_id == *wire_id_1 {
									continue;
								}
								drop(wire_2);// Avoid borrow issues
								wires_big_daddy.insert(daddy_id, *wire_id_1);
								let daddy = wires.get(&daddy_id).unwrap().borrow();
								let start_son = (daddy.ui_data.position - start_1_global).dot(axis_unit);
								let end_son = (daddy.end_pos() - start_1_global).dot(axis_unit);
								// Now modify wire 1
								let proj_locations = vec![start_1, end_1, start_son, end_son];
								let projection_min = n_min(&proj_locations).unwrap();
								let projection_max = n_max(&proj_locations).unwrap();
								wire_1.ui_data.position = start_1_global + axis_unit.mult(projection_min);
								wire_1.set_length((projection_max - projection_min) as u32);
							}
						}
					}
				}
			}
		}
		for wire_to_del in wires_big_daddy.keys() {
			wires.remove(wire_to_del);
		}
	}
	fn check_wires_connected_to_just_themselves(&self) {
		let wires = self.wires.borrow();
		for (wire_id, wire_cell) in wires.iter() {
			let this_connection = WireConnection::Wire(*wire_id);
			let mut wire = wire_cell.borrow_mut();
			wire.start_connections = Rc::new(RefCell::new(HashSet::from_iter(vec![this_connection.clone()].into_iter())));
			wire.end_connections = Rc::new(RefCell::new(HashSet::from_iter(vec![this_connection].into_iter())));
		}
	}
	/// Should be called AFTER the overlapping parallel wire checks, so that T-connections can only be intercepting along one wire
	fn combine_overlapping_wire_connection_sets(&self) {
		// First: T-conn check
		// vec<(Wire with end at pos, Wire to be sliced, Position)>
		// The code is in a seperate block so that the self.wires borrow will be dropped and won't cause borrow issues when T connections are fixed
		//let wires = self.wires.borrow();
		let mut unvisited_wires: Vec<u64> = self.wires.borrow().keys().map(|k| *k).collect();
		while unvisited_wires.len() != 0 {
			// Vec<(Wire with end being connected, Sliced wire, Position)>
			let t_conns_to_fix: Vec<(u64, u64, IntV2)> = {
				let wire_id = unvisited_wires.pop().unwrap();
				let wires = self.wires.borrow();
				let wire = wires.get(&wire_id).unwrap().borrow();
				let start_pos = wire.ui_data.position;
				let end_pos = wire.end_pos();
				let mut out = Vec::<(u64, u64, IntV2)>::new();
				if let Some((sliced_wire_id, _, intercept, _)) = self.is_point_on_wire(start_pos, Some(wire_id)) {// Some(*wire_id) is provided to avoid detecting an interception with the same wire
					if intercept == (false, true, false) {// Intercepting other wire in the middle
						out.push((wire_id, sliced_wire_id, start_pos));
					}
				}
				if let Some((sliced_wire_id, _, intercept, _)) = self.is_point_on_wire(end_pos, Some(wire_id)) {// Some(*wire_id) is provided to avoid detecting an interception with the same wire
					if intercept == (false, true, false) {// Intercepting other wire in the middle
						out.push((wire_id, sliced_wire_id, end_pos));
					}
				}
				out
			};
			for t_conn in t_conns_to_fix {
				let wire_conn = WireConnection::Wire(t_conn.0);
				let (_, new_wire_opt) = self.add_connection_to_wire(wire_conn, t_conn.1, t_conn.2, (false, true, false));
				// Do not need to add the new connection set because the end-to-end check will do that
				unvisited_wires.push(new_wire_opt.expect("Running `self.add_connection_to_wire()` to fix a T connection should always create a new wire"));
			}
		}
		// Now for end-to-end connections
		let wires = self.wires.borrow();
		for (wire_id, wire_cell) in wires.iter() {
			for (wire_id_2, wire_cell_2) in wires.iter() {
				if wire_id <= wire_id_2 {// This is a convenient way to avoid double-checking and self-checking
					continue;
				}
				let wire = wire_cell.borrow();
				let start_pos = wire.ui_data.position;
				let end_pos = wire.end_pos();
				let mut wire_2 = wire_cell_2.borrow_mut();
				let start_pos_2 = wire_2.ui_data.position;
				let end_pos_2 = wire_2.end_pos();
				if start_pos == start_pos_2 {
					merge_wire_end_connection_sets(&wire.start_connections, &wire_2.start_connections);
					wire_2.start_connections = Rc::clone(&wire.start_connections);
				}
				if start_pos == end_pos_2 {
					merge_wire_end_connection_sets(&wire.start_connections, &wire_2.end_connections);
					wire_2.end_connections = Rc::clone(&wire.start_connections);
				}
				if end_pos == start_pos_2 {
					merge_wire_end_connection_sets(&wire.end_connections, &wire_2.start_connections);
					wire_2.start_connections = Rc::clone(&wire.end_connections);
				}
				if end_pos == end_pos_2 {
					merge_wire_end_connection_sets(&wire.end_connections, &wire_2.end_connections);
					wire_2.end_connections = Rc::clone(&wire.end_connections);
				}
			}
		}
	}
	/// Almost last step in recomputing circuit connections after the circuit is edited
	/// If any pins (component or external) touch any wire, update the pin's net to the wire's net
	/// Also make sure pins not touching have no connection
	/// Consecutive-wires-in-same-direction check should happen after this so that any triple-joints that have been abandoned get merged back to a signle wire
	fn update_graphical_pin_to_wire_connections(&self) {
		// Component pins
		for (comp_id, comp_cell) in self.components.borrow().iter() {
			let comp = comp_cell.borrow();
			// Build pin positions before mut borrowing them
			let mut pin_positions = HashMap::<u64, IntV2>::new();
			for pin_id in comp.get_generic().graphic_pins.borrow().keys() {
				pin_positions.insert(*pin_id, comp.get_ui_data().pos_to_parent_coords(comp.get_pin_position_override(*pin_id).unwrap().0));
			}
			for (pin_id, pin) in comp.get_generic().graphic_pins.borrow_mut().iter_mut() {
				let pin_pos: IntV2 = *pin_positions.get(pin_id).unwrap();
				pin.wire_connections = match self.is_point_on_wire(pin_pos, None) {
					Some((wire_id, _, wire_intercept_triple, _)) => {
						// Update wire connection
						Some(self.add_connection_to_wire(WireConnection::Pin(CircuitWideGraphicPinReference::ComponentPin(ComponentGraphicPinReference::new(*comp_id, pin_id.clone()))), wire_id, pin_pos, wire_intercept_triple).0)
					},
					None => {
						None
					}
				};
			}
		}
		// External pins
		for (pin_id, pin) in self.generic_device.graphic_pins.borrow_mut().iter_mut() {
			let pin_pos: IntV2 = pin.ui_data.position;// Do not use `self.get_pin_position_override()`
			pin.wire_connections = match self.is_point_on_wire(pin_pos, None) {
				Some((wire_id, _, wire_intercept_triple, _)) => {
					// Update wire connection
					Some(self.add_connection_to_wire(WireConnection::Pin(CircuitWideGraphicPinReference::ExternalConnection(pin_id.clone())), wire_id, pin_pos, wire_intercept_triple).0)
				},
				None => {
					None
				}
			};
		}
	}
	fn update_splitter_wire_connections(&self) {
		for (splitter_id, splitter) in self.splitters.borrow_mut().iter_mut() {
			for pin_i in 0..(splitter.splits.len() as u16 + 1) {
				let pin_pos = splitter.ui_data.pos_to_parent_coords(Splitter::pin_pos_local(pin_i));
				splitter.set_pin_wire_conns(pin_i, &match self.is_point_on_wire(pin_pos, None) {
					Some((wire_id, _, wire_intercept_triple, _)) => {
						// Update wire connection
						Some(self.add_connection_to_wire(WireConnection::Splitter(*splitter_id, pin_i), wire_id, pin_pos, wire_intercept_triple).0)
					},
					None => {
						None
					}
				});
			}
		}
	}
	fn update_probe_net_connections_and_timing(&self) {
		let mut probes = self.probes.borrow_mut();
		for probe in probes.values_mut() {
			probe.nets_opt = self.is_connection_point(probe.ui_data.position).1;
		}
		// Now update timing diagram to match
		let mut timing = self.timing.borrow_mut();
		let n_samples = timing.n_samples;
		// Check clock
		if timing.signal_groups.len() == 0 {
			timing.signal_groups = vec![(0, vec![vec![]])];
		}
		// Add new probes
		let mut probes_in_use = HashSet::<u64>::new();
		for (group_i, (probe_id, _)) in timing.signal_groups.iter().enumerate() {
			if group_i > 0 {// Ignore clock
				probes_in_use.insert(*probe_id);
			}
		}
		for probe_id in probes.keys() {
			if !probes_in_use.contains(probe_id) {
				timing.signal_groups.push((*probe_id, vec![(0..n_samples).into_iter().map(|_| LogicState::Floating).collect()]));
			}
		}
		// Set timing signal group bit widths according to probes and remove them if the probe is gone
		let mut i: usize = 1;// Ignore first signal group which is the clock
		while i < timing.signal_groups.len() {
			let (probe_id, ref mut signal_group) = &mut timing.signal_groups[i];
			if let Some(probe) = probes.get(probe_id) {
				let diff: isize = probe.nets_opt.len() as isize - (signal_group.len() as isize);
				if diff > 0 {// Probe has more bits than corresponding signal group, make a new one filled with floating states
					for _ in 0..diff {
						signal_group.push((0..n_samples).into_iter().map(|_| LogicState::Floating).collect());
					}
				}
				if diff < 0 {
					for _ in 0..(-diff) {
						signal_group.pop();
					}
				}
			}
			else {// There is no probe corresponding to this group, so delete it
				timing.signal_groups.remove(i);
				i -= 1;
			}
			i += 1;
		}
	}
	/// Almost last step in recomputing circuit connections after the circuit is edited
	/// If any pins (component or external) touch any wire, update the pin's net to the wire's net
	fn update_logical_pin_to_wire_connections(&self) {
		let wires = self.wires.borrow();
		let nets = self.nets.borrow();
		// Component pins
		for (comp_id, comp_cell) in self.components.borrow().iter() {
			let comp = comp_cell.borrow();
			let comp_logical_pins = comp.get_generic().logic_pins.borrow();
			for (_, pin) in comp.get_generic().graphic_pins.borrow_mut().iter_mut() {
				match &pin.wire_connections {
					Some(conns_cell) => {
						let conns = conns_cell.borrow();
						for conn in conns.iter() {
							if let WireConnection::Wire(wire_id) = conn {
								let new_net_ids: &Vec<u64> = &wires.get(wire_id).expect(&format!("self.is_point_on_wire() returned invalid wire ID {}", wire_id)).borrow().nets;
								// Even if bit widths don't match, wires are created to have the highest of all encountered bit widths so we need to iterate over the pin's owned logic pins and NOT the wire
								for (bit_i, comp_logic_pin_id) in pin.owned_pins.iter().enumerate() {
									let net_id = new_net_ids[bit_i];
									comp_logical_pins.get(comp_logic_pin_id).unwrap().borrow_mut().external_source = Some(LogicConnectionPinExternalSource::Net(net_id));
									nets.get(&net_id).expect("Net ID invalid").borrow_mut().edit_component_connection(true, *comp_id, *comp_logic_pin_id);
								}
							}
						}
					},
					None => {
						for comp_logic_pin_id in &pin.owned_pins {
							comp_logical_pins.get(comp_logic_pin_id).unwrap().borrow_mut().external_source = None;
						}
					}
				}
			}
		}
		let logical_pins = self.generic_device.logic_pins.borrow();
		// External pins
		for (_, pin) in self.get_generic().graphic_pins.borrow_mut().iter_mut() {
			match &pin.wire_connections {
				Some(conns_cell) => {
					let conns = conns_cell.borrow();
					for conn in conns.iter() {
						if let WireConnection::Wire(wire_id) = conn {
							let new_net_ids: &Vec<u64> = &wires.get(wire_id).expect(&format!("self.is_point_on_wire() returned invalid wire ID {}", wire_id)).borrow().nets;
							// Even if bit widths don't match, wires are created to have the highest of all encountered bit widths so we need to iterate over the pin's owned logic pins and NOT the wire
							for (bit_i, comp_logic_pin_id) in pin.owned_pins.iter().enumerate() {
								let net_id = new_net_ids[bit_i];
								logical_pins.get(comp_logic_pin_id).unwrap().borrow_mut().internal_source = Some(LogicConnectionPinInternalSource::Net(net_id));
								nets.get(&net_id).expect("Net ID invalid").borrow_mut().edit_external_connection(true, *comp_logic_pin_id);
							}
						}
					}
				},
				None => {
					for comp_logic_pin_id in &pin.owned_pins {
						logical_pins.get(comp_logic_pin_id).unwrap().borrow_mut().internal_source = None;
					}
				}
			}
		}
	}
	/// Checks for consecutive wire segments that are in the same direction and are only connected to each other. Wires like this can be merged into one wire segment
	/// It also takes care of updating the end connections
	/// Should be run AFTER the pin update and wire connection update functions
	/// Assumes that there are no overlapping wires
	fn merge_consecutive_wires(&self) {
		let mut wires = self.wires.borrow_mut();
		let mut wires_to_delete = HashSet::<u64>::new();
		for (wire_id_1, wire_cell_1) in wires.iter() {
			if wires_to_delete.contains(wire_id_1) {
				continue;
			}
			let mut wire_1 = wire_cell_1.borrow_mut();
			let end_conns: Vec<WireConnection> = {
				let binding = wire_1.end_connections.borrow();
				binding.iter().map(|conn| conn.clone()).collect()
			};
			if end_conns.len() == 2 {
				for conn in end_conns {
					if let WireConnection::Wire(wire_id_2) = conn {
						if *wire_id_1 == wire_id_2 {// Avoid self-checking
							continue;
						}
						let wire_2 = wires.get(&wire_id_2).unwrap().borrow();
						let same_forward = wire_1.ui_data.direction == wire_2.ui_data.direction;
						let same_backward = wire_1.ui_data.direction == wire_2.ui_data.direction.opposite_direction();
						if same_forward {
							// Get rid of wire 1 end and replace it with wire 2 end
							// Remove wire 2 from new end and replace with reference to wire 1
							// Wire1(1 start ... 1 end) + Wire2(2 start ... 2 end)
							//           v
							// Wire1(1 start ... 2 end)
							let mut conns_2_end = wire_2.end_connections.borrow_mut();
							conns_2_end.remove(&conn);
							conns_2_end.insert(WireConnection::Wire(*wire_id_1));
							wire_1.end_connections = Rc::clone(&wire_2.end_connections);
						}
						if same_backward {
							// Get rid of wire 1 end and replace it with wire 2 start
							// Remove wire 2 from new end and replace with reference to wire 1
							// Wire1(1 start ... 1 end) + Wire2(2 end ... 2 start)
							//           v
							// Wire1(1 start ... 2 start)
							let mut conns_2_start = wire_2.start_connections.borrow_mut();
							conns_2_start.remove(&conn);
							conns_2_start.insert(WireConnection::Wire(*wire_id_1));
							wire_1.end_connections = Rc::clone(&wire_2.start_connections);
						}
						// Stuff that has to be done either way
						if same_forward || same_backward {
							let new_len = wire_1.get_len() + wire_2.get_len();
							wire_1.set_length(new_len);
							wires_to_delete.insert(wire_id_2);
						}
					}
				}
			}
			let start_conns: Vec<WireConnection> = {
				let binding = wire_1.start_connections.borrow();
				binding.iter().map(|conn| conn.clone()).collect()
			};
			if start_conns.len() == 2 {
				for conn in start_conns {
					if let WireConnection::Wire(wire_id_2) = conn {
						if *wire_id_1 == wire_id_2 {// Avoid self-checking
							continue;
						}
						let wire_2 = wires.get(&wire_id_2).unwrap().borrow();
						let same_forward = wire_1.ui_data.direction == wire_2.ui_data.direction;
						let same_backward = wire_1.ui_data.direction == wire_2.ui_data.direction.opposite_direction();
						if same_forward {
							// Get rid of wire 1 start and replace it with wire 2 start
							// Remove wire 2 from new start and replace with reference to wire 1
							// Wire2(2 start ... 2 end) + Wire1(1 start ... 1 end)
							//           v
							// Wire1(2 start ... 1 end)
							let mut conns_2_start = wire_2.start_connections.borrow_mut();
							conns_2_start.remove(&conn);
							conns_2_start.insert(WireConnection::Wire(*wire_id_1));
							wire_1.start_connections = Rc::clone(&wire_2.end_connections);
						}
						if same_backward {
							// Get rid of wire 1 start and replace it with wire 2 end
							// Remove wire 2 from new start and replace with reference to wire 1
							// Wire2(2 end ... 2 start) + Wire1(1 start ... 1 end)
							//           v
							// Wire1(2 end ... 1 end)
							let mut conns_2_end = wire_2.end_connections.borrow_mut();
							conns_2_end.remove(&conn);
							conns_2_end.insert(WireConnection::Wire(*wire_id_1));
							wire_1.start_connections = Rc::clone(&wire_2.end_connections);
						}
						// Stuff that has to be done either way
						if same_forward || same_backward {
							wire_1.ui_data.position = if same_forward {
								wire_2.ui_data.position
							}
							else {
								wire_2.end_pos()
							};
							let new_len = wire_1.get_len() + wire_2.get_len();
							wire_1.set_length(new_len);
							wires_to_delete.insert(wire_id_2);
						}
					}
				}
			}
		}
		for wire_id in wires_to_delete.iter() {
			wires.remove(wire_id);
		}
	}
	/// Will split a wire into two sections if the joint is somewhere in the middle, otherwise just adds it at one end
	/// Returns: (
	/// 	The shared connection set that should be used in case a wire is what is being connected
	/// 	Optional new wire segment, only when wire has been split in the middle
	/// )
	fn add_connection_to_wire(&self, connection: WireConnection, wire_id: u64, position: IntV2, wire_intercept_triple: (bool, bool, bool)) -> (Rc<RefCell<HashSet<WireConnection>>>, Option<u64>) {
		match wire_intercept_triple {
			(true, false, false) => {
				let conns = Rc::clone(&self.wires.borrow().get(&wire_id).unwrap().borrow().start_connections);
				conns.borrow_mut().insert(connection);
				(conns, None)
			},
			(false, true, false) => {// Break wire in two
				let new_wire_id: u64 = lowest_unused_key(&*self.wires.borrow());
				// Get info from original wire (which stays in same position, length reduced)
				let (middle_conns, end_conns, direction, new_length, nets): (Rc<RefCell<HashSet<WireConnection>>>, Rc<RefCell<HashSet<WireConnection>>>, FourWayDir, u32, Vec<u64>) = {
					let binding = self.wires.borrow();
					let mut wire = binding.get(&wire_id).unwrap().borrow_mut();
					// Calculate new lengths
					let wire_len = (position - wire.ui_data.position).taxicab();
					let new_wire_len = (wire.end_pos() - position).taxicab();
					wire.set_length(wire_len);// Assign this later so `wire.end_pos()` can be used
					// Change old wire's connections to just itself, the new wire, and the new connection
					let end_conns = Rc::clone(&wire.end_connections);
					wire.end_connections = Rc::new(RefCell::new(HashSet::from_iter(vec![
						WireConnection::Wire(wire_id),
						WireConnection::Wire(new_wire_id),
						connection.clone()
					].into_iter())));
					(Rc::clone(&wire.end_connections), end_conns, wire.ui_data.direction.clone(), new_wire_len, wire.nets.clone())
				};
				// Update old end conns (`end_conns`) to remove old wire and add new wire
				{
					let mut end_conns_borrowed = end_conns.borrow_mut();
					end_conns_borrowed.remove(&WireConnection::Wire(wire_id));
					end_conns_borrowed.insert(WireConnection::Wire(new_wire_id));
				}
				// Create new wire
				self.wires.borrow_mut().insert(new_wire_id, RefCell::new(Wire::new(position, new_length, direction, nets, Rc::clone(&middle_conns), end_conns)));
				(middle_conns, Some(new_wire_id))
			},
			(false, false, true) => {
				let conns = Rc::clone(&self.wires.borrow().get(&wire_id).unwrap().borrow().end_connections);
				conns.borrow_mut().insert(connection);
				(conns, None)
			},
			other => panic!("LogicCircuit.add_connection_to_wire() provided with invalid wire intercept triple: {:?}", other)
		}
	}
	fn recompute_internals_bb(&mut self) {
		let mut bb_opt = Option::<(V2, V2)>::None;
		for item_ref in self.get_all_graphics_references() {
			if let GraphicSelectableItemRef::Pin(_) = &item_ref {
				continue;
			}
			self.run_function_on_graphic_item(item_ref.clone(), |item_box| {
				let new_bb = item_box.bounding_box(V2::zeros());
				if let Some(bb) = &mut bb_opt {
					*bb = merge_points_to_bb(vec![bb.0, bb.1, new_bb.0, new_bb.1]);
				}
				else {
					bb_opt = Some(new_bb);
				}
			});
		}
		if let Some(bb) = bb_opt {
			self.circuit_internals_bb = bb;
		}
		else {
			self.circuit_internals_bb = (V2::zeros(), V2::zeros())
		}
	}
	fn was_anything_clicked<'a>(&self, grid_pos: V2, multi_select_key: bool) -> Option<GraphicSelectableItemRef> {
		let mut selected_opt = Option::<GraphicSelectableItemRef>::None;
		for ref_ in self.get_all_graphics_references() {
			self.run_function_on_graphic_item_mut(ref_.clone(), |graphic_item| {
				if graphic_item.is_click_hit(grid_pos, V2::zeros()) {
					if multi_select_key {
						selected_opt = Some(ref_.clone());
					}
					else if !graphic_item.accept_click(graphic_item.get_ui_data().parent_pos_to_local_coords_float(grid_pos)) {
						selected_opt = Some(ref_.clone());
					}
				}
			});
		}
		selected_opt
	}
	/// Returns: Whether it was removed
	fn remove_graphic_item(&self, ref_: &GraphicSelectableItemRef) -> bool {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => self.components.borrow_mut().remove(comp_id).is_some(),
			GraphicSelectableItemRef::Wire(wire_id) => self.wires.borrow_mut().remove(wire_id).is_some(),
			GraphicSelectableItemRef::Pin(pin_id) => self.generic_device.graphic_pins.borrow_mut().remove(pin_id).is_some(),
			GraphicSelectableItemRef::Splitter(splitter_id) => self.splitters.borrow_mut().remove(splitter_id).is_some(),
			GraphicSelectableItemRef::GraphicLabel(label_id) => self.labels.borrow_mut().remove(label_id).is_some(),
			GraphicSelectableItemRef::Probe(probe_index) => self.probes.borrow_mut().remove(probe_index).is_some()
		}
	}
	/*pub fn run_function_on_graphic_item<T>(&self, ref_: GraphicSelectableItemRef, mut func: impl FnMut(Box<&dyn GraphicSelectableItem>) -> T) -> Option<T> {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item(self.components.borrow().get(&comp_id).expect(&error_msg).borrow().deref().as_ref()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(self.wires.borrow().get(&wire_id).expect(&error_msg).borrow().deref())),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(self.generic_device.pins.borrow().get(&pin_id).expect(&error_msg).borrow().deref())),
		}
	}
	pub fn run_function_on_graphic_item_mut<T>(&self, ref_: GraphicSelectableItemRef, mut func: impl FnMut(Box<&mut dyn GraphicSelectableItem>) -> T) -> Option<T> {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item_mut(self.components.borrow().get(&comp_id).expect(&error_msg).borrow_mut().deref_mut().as_mut()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(self.wires.borrow().get(&wire_id).expect(&error_msg).borrow_mut().deref_mut())),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(self.generic_device.pins.borrow().get(&pin_id).expect(&error_msg).borrow_mut().deref_mut())),
		}
	}
	/// Copy something(s) that have been selected and return a `CopiedGraphicItem` that can be put onto the clipboard as JSON
	fn copy_graphic_item(&self, ref_: GraphicSelectableItemRef) -> Option<CopiedGraphicItem> {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => self.components.borrow().get(&comp_id).expect(&error_msg).borrow().copy(),
			GraphicSelectableItemRef::Wire(wire_id) => self.wires.borrow().get(&wire_id).expect(&error_msg).borrow().copy(),
			GraphicSelectableItemRef::Pin(pin_id) => self.generic_device.pins.borrow().get(&pin_id).expect(&error_msg).borrow().copy(),
		}
	}*/
	// Following 3 methods refactored by ChatGPT
	pub fn run_function_on_graphic_item<T>(
		&self,
		ref_: GraphicSelectableItemRef,
		mut func: impl FnMut(Box<&dyn GraphicSelectableItem>) -> T,
	) -> Option<T> {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => {
				let components_ref = self.components.borrow();
				let comp_rc = components_ref.get(&comp_id)?;
				let comp_borrow = comp_rc.borrow();
				let graphic_item = logic_device_to_graphic_item(comp_borrow.deref().as_ref());
				Some(func(Box::new(graphic_item)))
			}
			GraphicSelectableItemRef::Wire(wire_id) => {
				let wires_ref = self.wires.borrow();
				let wire_rc = wires_ref.get(&wire_id)?;
				let wire_borrow = wire_rc.borrow();
				Some(func(Box::new(wire_borrow.deref())))
			}
			GraphicSelectableItemRef::Pin(pin_id) => {
				let pins_ref = self.generic_device.graphic_pins.borrow();
				let pin = pins_ref.get(&pin_id)?;
				Some(func(Box::new(pin)))
			},
			GraphicSelectableItemRef::Splitter(splitter_id) => {
				let splitters = self.splitters.borrow();
				let splitter = splitters.get(&splitter_id)?;
				Some(func(Box::new(splitter)))
			},
			GraphicSelectableItemRef::GraphicLabel(label_id) => {
				let labels = self.labels.borrow();
				let label = labels.get(&label_id)?;
				Some(func(Box::new(label)))
			},
			GraphicSelectableItemRef::Probe(probe_id) => {
				let probes = self.probes.borrow();
				let probe = probes.get(&probe_id)?;
				Some(func(Box::new(probe)))
			}
		}
	}
	pub fn run_function_on_graphic_item_mut<T>(
		&self,
		ref_: GraphicSelectableItemRef,
		mut func: impl FnMut(Box<&mut dyn GraphicSelectableItem>) -> T,
	) -> Option<T> {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => {
				let mut components_ref = self.components.borrow_mut();
				let comp_rc = components_ref.get_mut(&comp_id)?;
				let mut comp_borrow = comp_rc.borrow_mut();
				let graphic_item = logic_device_to_graphic_item_mut(comp_borrow.deref_mut().as_mut());
				Some(func(Box::new(graphic_item)))
			}
			GraphicSelectableItemRef::Wire(wire_id) => {
				let mut wires_ref = self.wires.borrow_mut();
				let wire_rc = wires_ref.get_mut(&wire_id)?;
				let mut wire_borrow = wire_rc.borrow_mut();
				Some(func(Box::new(wire_borrow.deref_mut())))
			}
			GraphicSelectableItemRef::Pin(pin_id) => {
				let mut pins_ref = self.generic_device.graphic_pins.borrow_mut();
				let pin = pins_ref.get_mut(&pin_id)?;
				Some(func(Box::new(pin)))
			},
			GraphicSelectableItemRef::Splitter(splitter_id) => {
				let mut splitters = self.splitters.borrow_mut();
				let splitter = splitters.get_mut(&splitter_id)?;
				Some(func(Box::new(splitter)))
			},
			GraphicSelectableItemRef::GraphicLabel(label_id) => {
				let mut labels = self.labels.borrow_mut();
				let label = labels.get_mut(&label_id)?;
				Some(func(Box::new(label)))
			},
			GraphicSelectableItemRef::Probe(probe_id) => {
				let mut probes = self.probes.borrow_mut();
				let probe = probes.get_mut(&probe_id)?;
				Some(func(Box::new(probe)))
			}
		}
	}
	fn copy_graphic_item(&self, ref_: GraphicSelectableItemRef) -> Option<CopiedGraphicItem> {
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => {
				let components_ref = self.components.borrow();
				let comp_rc = components_ref.get(&comp_id)?;
				let comp_borrow = comp_rc.borrow();
				Some(comp_borrow.copy())
			}
			GraphicSelectableItemRef::Wire(wire_id) => {
				let wires_ref = self.wires.borrow();
				let wire_rc = wires_ref.get(&wire_id)?;
				let wire_borrow = wire_rc.borrow();
				Some(wire_borrow.copy())
			}
			GraphicSelectableItemRef::Pin(pin_id) => {
				let pins_ref = self.generic_device.graphic_pins.borrow();
				let pin = pins_ref.get(&pin_id)?;
				Some(pin.copy())
			},
			GraphicSelectableItemRef::Splitter(splitter_id) => {
				let splitters = self.splitters.borrow();
				let splitter = splitters.get(&splitter_id)?;
				Some(splitter.copy())
			},
			GraphicSelectableItemRef::GraphicLabel(label_id) => {
				let labels = self.labels.borrow();
				let label = labels.get(&label_id)?;
				Some(label.copy())
			},
			GraphicSelectableItemRef::Probe(probe_id) => {
				let probes = self.probes.borrow();
				let probe = probes.get(&probe_id)?;
				Some(probe.copy())
			}
		}
	}
	/*fn delete_graphic_item(&mut self, ref_: GraphicSelectableItem) {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item(self.components.get_item_tuple(&(comp_id.into())).expect(&error_msg).1.borrow().deref().as_ref()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(wire_to_graphic_item(&self.wires.get_item_tuple(&(wire_id.into())).expect(&error_msg).1))),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(pin_to_graphic_item(&self.generic_device.pins.get_item_tuple(&(pin_id.into())).expect(&error_msg).1))),
		}
	}*/
	fn get_all_graphics_references(&self) -> Vec<GraphicSelectableItemRef> {
		let mut out = Vec::<GraphicSelectableItemRef>::new();
		for (ref_, _) in self.components.borrow().iter() {
			out.push(GraphicSelectableItemRef::Component(*ref_));
		}
		for (ref_, _) in self.wires.borrow().iter() {
			out.push(GraphicSelectableItemRef::Wire(*ref_));
		}
		for (ref_, _) in self.generic_device.graphic_pins.borrow().iter() {
			out.push(GraphicSelectableItemRef::Pin(ref_.clone()));
		}
		for id in self.splitters.borrow().keys() {
			out.push(GraphicSelectableItemRef::Splitter(*id));
		}
		for id in self.labels.borrow().keys() {
			out.push(GraphicSelectableItemRef::GraphicLabel(*id));
		}
		for id in self.probes.borrow().keys() {
			out.push(GraphicSelectableItemRef::Probe(*id));
		}
		out
	}
	/// Creates a new flatened circuit and saves it, returning the path to the saved circuit
	pub fn flatten(&self, apply_transform: bool) -> Result<EnumAllLogicDevices, String> {
		let mut save = self.create_save_circuit().unwrap();
		let (wires, comps) = self.flatten_recursive(apply_transform)?;
		save.wires = vec_to_u64_keyed_hashmap(wires);
		save.components = vec_to_u64_keyed_hashmap(comps);
		let save_path = format!("{}_flattened", self.save_path);
		save.type_name = format!("{} (flattened)", save.type_name);
		let raw_string: String = to_string_err(serde_json::to_string(&save))?;
		to_string_err(fs::write(resource_interface::get_circuit_file_path(&save_path), &raw_string))?;
		Ok(EnumAllLogicDevices::SubCircuit(save_path, self.displayed_as_block, self.generic_device.ui_data.position, self.generic_device.ui_data.direction, self.generic_device.name.clone()))
	}
	/// Recursively extracts all sub-circuits that don't have a fixed sub-cycle count
	pub fn flatten_recursive(&self, apply_transform: bool) -> Result<(Vec<(IntV2, FourWayDir, u32)>, Vec<EnumAllLogicDevices>), String> {
		let mut wire_geometry = Vec::<(IntV2, FourWayDir, u32)>::new();
		let mut components = Vec::<EnumAllLogicDevices>::new();
		// Wires
		for (_, wire_cell) in self.wires.borrow().iter() {
			let wire = wire_cell.borrow();
			wire_geometry.push((wire.ui_data.position, wire.ui_data.direction, wire.length));
		}
		// Components
		for (_, comp_cell) in self.components.borrow().iter() {
			let comp = comp_cell.borrow();
			if comp.is_circuit() {
				let circuit: &LogicCircuit = comp.get_circuit();
				if circuit.fixed_sub_cycles_opt.is_some() {
					components.push(circuit.flatten(false)?);
				}
				else {
					let (mut sub_wires, mut sub_comps) = circuit.flatten_recursive(true)?;
					wire_geometry.append(&mut sub_wires);
					components.append(&mut sub_comps);
				}
			}
			else {
				components.push(comp.save().unwrap());
			}
		}
		// Apply transformation for this circuit
		let transform = |pos: &mut IntV2, dir: &mut FourWayDir| {
			// Rotate direction and local pos by circuit direction and add circuit pos
			*dir = self.generic_device.ui_data.direction.rotate_intv2(dir.to_unit_int()).is_along_axis().unwrap();
			*pos = self.generic_device.ui_data.direction.rotate_intv2(*pos) + self.generic_device.ui_data.position;
		};
		if apply_transform {
			for wire in wire_geometry.iter_mut() {
				transform(&mut wire.0, &mut wire.1);
			}
			for comp_save in components.iter_mut() {
				let mut comp = EnumAllLogicDevices::to_dynamic(comp_save.clone()).unwrap();
				let ui_data = comp.get_ui_data_mut();
				transform(&mut ui_data.position, &mut ui_data.direction);
				*comp_save = comp.save().unwrap();
			}
		}
		Ok((wire_geometry, components))
	}
	/// Returns: Whether anything changed
	pub fn compute_immutable(&self, ancestors_above: &AncestryStack, self_component_id: u64, first_propagation_step: bool) -> bool {
		let mut changed = false;
		// Update clock
		if first_propagation_step {
			self.clock.borrow_mut().update();
		}
		let clock_state: bool = self.clock.borrow().state;
		let ancestors = ancestors_above.push((&self, self_component_id));
		// Update net states
		let mut new_net_states = HashMap::<u64, (LogicState, Vec<GlobalSourceReference>)>::new();
		for (net_id, net) in self.nets.borrow().iter() {
			new_net_states.insert(*net_id, net.borrow().update_state(&ancestors, *net_id));
		}
		drop(ancestors);
		for (net_id, (state, sources)) in new_net_states.into_iter() {
			let binding = self.nets.borrow();
			let mut net_mut_ref = binding.get(&net_id).unwrap().borrow_mut();
			changed |= net_mut_ref.state != state;
			net_mut_ref.state = state;
			net_mut_ref.sources = sources;
		}
		// Update pin & wire states from nets
		// External connection pins
		for (pin_id, pin_cell) in self.generic_device.logic_pins.borrow_mut().iter_mut() {
			let int_source_opt = pin_cell.borrow().internal_source.clone();
			if let Some(source) = int_source_opt {
				match source {
					LogicConnectionPinInternalSource::Net(net_id) => pin_cell.borrow_mut().set_drive_internal(self.nets.borrow().get(&net_id).expect(&format!("External connection pin {} has invalid net query {}", pin_id, net_id)).borrow().state),
					LogicConnectionPinInternalSource::ComponentInternal => panic!("External connection pin {} for circuit \"{}\" has the internal source as ComponentInternal which shouldn't happen", pin_id, &self.generic_device.name)
				}
			}
			else {
				pin_cell.borrow_mut().set_drive_internal(LogicState::Floating);
			}
		}
		// Component pins, and propagate through components
		for (comp_id, comp) in self.components.borrow().iter() {
			for (pin_id, pin_cell) in comp.borrow_mut().get_generic_mut().logic_pins.borrow_mut().iter_mut() {
				let ext_source_opt = pin_cell.borrow().external_source.clone();
				if let Some(source) = ext_source_opt {
					match source {
						LogicConnectionPinExternalSource::Global => panic!("Pin {} of component {} has external source 'Global' which doesn't make sense", pin_id, comp_id),
						LogicConnectionPinExternalSource::Net(net_id) => {
							pin_cell.borrow_mut().set_drive_external(self.nets.borrow_mut().get_mut(&net_id).expect(&format!("Pin {} of component {} references net {} which doesn't exist", pin_id, comp_id, net_id)).borrow().state);
						}
					}
				}
				else {
					pin_cell.borrow_mut().set_drive_external(LogicState::Floating);
				}
			}
		}
		// THIS GOES LAST, has to be seperate loop then before
		let ancestors = ancestors_above.push((&self, self_component_id));
		for (comp_id, comp_cell) in self.components.borrow().iter() {
			let is_circuit = comp_cell.borrow().is_circuit();
			if is_circuit {
				let comp = comp_cell.borrow();
				let circuit: &LogicCircuit = comp.get_circuit();
				if let Some(compute_cycles) = circuit.fixed_sub_cycles_opt {
					for _ in 0..compute_cycles {
						circuit.compute_immutable(&ancestors, *comp_id, first_propagation_step);
					}
				}
				else {
					changed |= circuit.compute_immutable(&ancestors, *comp_id, first_propagation_step);
				}
			}
			else {
				comp_cell.borrow_mut().compute(&ancestors, *comp_id, clock_state, first_propagation_step);
			}
		}
		// Done
		changed
	}
	pub fn update_timing_diagram(&self) {
		let mut timing = self.timing.borrow_mut();
		let probes = self.probes.borrow();
		let nets = self.nets.borrow();
		timing.signal_groups[0].1[0].push(self.clock.borrow().state.into());
		for (group_i, (probe_id, group)) in &mut timing.signal_groups.iter_mut().enumerate() {
			if group_i == 0 {// Clock signal
				continue;
			}
			let probe_net_ids = &probes.get(probe_id).unwrap().nets_opt;
			for (i, net_opt) in probe_net_ids.iter().enumerate() {
				group[i].push(match net_opt {
					Some(net_id) => nets.get(net_id).unwrap().borrow().state,
					None => LogicState::Floating
				});
			}
		}
		timing.n_samples += 1;
	}
	pub fn create_save_circuit(&self) -> Result<LogicCircuitSave, String> {
		// Convert components to enum variants to be serialized
		let mut components_save = HashMap::<u64, EnumAllLogicDevices>::new();
		for (ref_, component) in self.components.borrow().iter() {
			components_save.insert(*ref_, component.borrow().save()?);
		}
		// Un-RefCell Logic pins
		let mut logic_pins = HashMap::<u64, LogicConnectionPin>::new();
		for (pin_id, pin_cell) in self.generic_device.logic_pins.borrow().iter() {
			let pin = pin_cell.borrow();
			logic_pins.insert(*pin_id, pin.clone());
		}
		// graphic pins
		let mut graphic_pins = HashMap::<u64, (IntV2, FourWayDir, String, bool, Vec<u64>)>::new();
		for (pin_id, pin) in self.generic_device.graphic_pins.borrow().iter() {
			graphic_pins.insert(*pin_id, (pin.ui_data.position, pin.ui_data.direction, pin.name.clone(), pin.show_name, pin.owned_pins.clone()));
		}
		// Un-RefCell Wires
		let mut wires_save = HashMap::<u64, (IntV2, FourWayDir, u32)>::new();
		for (ref_, wire_cell) in self.wires.borrow().iter() {
			let wire = wire_cell.borrow();
			wires_save.insert(*ref_, (wire.ui_data.position, wire.ui_data.direction, wire.length));
		}
		let clock = self.clock.borrow();
		// First, actually save this circuit
		Ok(LogicCircuitSave {
			logic_pins,
			graphic_pins,
			components: components_save,
			wires: wires_save,
			splitters: HashMap::from_iter(self.splitters.borrow().iter().map(|t| (*t.0, t.1.save()))),
			labels: HashMap::from_iter(self.labels.borrow().iter().map(|t| (*t.0, t.1.save()))),
			block_pin_positions: self.block_pin_positions.clone(),
			type_name: self.type_name.clone(),
			fixed_sub_cycles_opt: self.fixed_sub_cycles_opt,
			clock_enabled: clock.enabled,
			clock_freq: clock.freq,
			clock_state: clock.state,
			probes: HashMap::from_iter(self.probes.borrow().iter().map(|(id, probe)| (*id, probe.save()))),
			timing_probe_order: self.timing.borrow().signal_groups.iter().enumerate().filter(|(i, _)| *i > 0).map(|(_, (probe_id, _))| *probe_id).collect()
		})
	}
	pub fn save_circuit(&self) -> Result<(), String> {
		let save = self.create_save_circuit()?;
		let raw_string: String = to_string_err(serde_json::to_string(&save))?;
		to_string_err(fs::write(resource_interface::get_circuit_file_path(&self.save_path), &raw_string))?;
		Ok(())
	}
	pub fn draw_as_block<'a>(&self, draw: &ComponentDrawInfo<'a>, for_block_layout_edit: bool) {
		// Rectangle
		draw.draw_polyline(
			vec![
				V2::new(self.generic_device.ui_data.local_bb.0.x, self.generic_device.ui_data.local_bb.0.y),
				V2::new(self.generic_device.ui_data.local_bb.1.x, self.generic_device.ui_data.local_bb.0.y),
				V2::new(self.generic_device.ui_data.local_bb.1.x, self.generic_device.ui_data.local_bb.1.y),
				V2::new(self.generic_device.ui_data.local_bb.0.x, self.generic_device.ui_data.local_bb.1.y),
				V2::new(self.generic_device.ui_data.local_bb.0.x, self.generic_device.ui_data.local_bb.0.y)
			],
			draw.styles.color_foreground
		);
		// Pins at alternate locations
		for (pin_id, pin) in self.generic_device.graphic_pins.borrow().iter() {
			let pin_alternate_config = self.block_pin_positions.get(pin_id).expect("Pin missing from block layout");
			let pin_stroke = match for_block_layout_edit {
				true => draw.styles.color_foreground,
				false => draw.styles.color_from_logic_states(&pin.states())
			};
			draw.draw_polyline(
				vec![
					pin_alternate_config.0.to_v2(),
					(pin_alternate_config.0 - pin_alternate_config.1.to_unit_int()).to_v2()
				],
				pin_stroke
			);
			if for_block_layout_edit {
				draw.draw_circle_filled(pin_alternate_config.0.to_v2(), draw.styles.connection_dot_grid_size, draw.styles.color_wire_floating);
			}
			// Pin name
			/*if pin_alternate_config.2 {
				draw.text(
					pin.name.clone(),
					pin_alternate_config.0.to_v2() - (pin_alternate_config.1.to_unit()*1.2),
					pin_alternate_config.1.rotate_intv2(draw.direction.to_unit_int()).is_along_axis().unwrap().to_egui_align2(),
					draw.styles.text_color,
					draw.styles.text_size_grid
				);
			}*/
		}
		// Name
		draw.text(
			self.type_name.clone(),
			V2::zeros(),// Relative
			Align2::CENTER_CENTER,
			draw.styles.text_color,
			draw.styles.text_size_grid,
			false
		);
	}
}

impl LogicDevice for LogicCircuit {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic_device
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic_device
	}
	fn compute_step(&mut self, ancestors_above: &AncestryStack, self_component_id: u64, _: bool, first_propagation_step: bool) {
		self.compute_immutable(ancestors_above, self_component_id, first_propagation_step);
	}
	/// Returns handle to file for inclusion in other circuits
	/// The actual save is done with `LogicCircuit::save_circuit()`
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		// Path to save file
		Ok(EnumAllLogicDevices::SubCircuit(self.save_path.clone(), self.displayed_as_block, self.get_ui_data().position, self.get_ui_data().direction, self.generic_device.name.clone()))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		if self.is_toplevel {
			let mouse_pos_grid_opt: Option<V2> = match draw.mouse_pos {
				Some(pos_px) => Some(draw.mouse_pos2_to_grid(pos_px)),
				None => None
			};
			match self.tool.borrow().deref() {
				Tool::Select{selected_graphics, selected_graphics_state} => {
					match selected_graphics_state {
						SelectionState::Fixed => {},
						SelectionState::Dragging(start_grid, delta_grid) => {
							if selected_graphics.is_empty() {
								let select_bb: (V2, V2) = merge_points_to_bb(vec![*start_grid, *start_grid + *delta_grid]);
								draw.draw_rect(*start_grid, *start_grid + *delta_grid, draw.styles.select_rect_color, draw.styles.select_rect_edge_color);
							}
						},
						SelectionState::FollowingMouse(mouse_pos) => {}
					}
					// Draw selected items BB
					if selected_graphics.len() >= 1 {
						let mut points = Vec::<V2>::new();
						for item_ref in selected_graphics.iter() {
							self.run_function_on_graphic_item(item_ref.clone(), |graphic_item| {
								let new_bb = graphic_item.bounding_box(V2::zeros());
								points.push(new_bb.0);
								points.push(new_bb.1);
							});
						}
						let bb = merge_points_to_bb(points);
						draw.draw_rect(bb.0, bb.1, [0, 0, 0, 0], draw.styles.select_rect_edge_color);
					}
				},
				Tool::HighlightNet(_net_id) => {
					// TODO
				},
				Tool::PlaceWire{perp_pairs} => {
					if let Some(mouse_pos_grid) = mouse_pos_grid_opt {
						let mouse_pos_grid_rounded: IntV2 = round_v2_to_intv2(mouse_pos_grid);
						let n_pairs = perp_pairs.len();
						// Wire has been started
						if n_pairs >= 1 {
							// Display in-progress wire
							for (i, pair) in perp_pairs.iter().enumerate() {
								let end_pos = if i == perp_pairs.len() - 1 {
									mouse_pos_grid_rounded
								}
								else {
									perp_pairs[i+1].0
								};
								let segments = Wire::perpindicular_pair_to_segments(pair, end_pos);
								for segment in segments {
									draw.draw_polyline(vec![
										segment.0.to_v2(),
										segment.0.to_v2() + (segment.1.to_unit() * (segment.2 as f32))
									], draw.styles.color_wire_in_progress);
								}
							}
						}
					}
				}
			}
		}
		if self.displayed_as_block {
			self.draw_as_block(draw, false);
		}
		else {
			// Draws the circuit with wires and everything how you would expect
			// Get wire colors
			let nets = self.nets.borrow();
			for wire_cell in self.wires.borrow().values() {
				let mut wire = wire_cell.borrow_mut();
				let states: Vec<LogicState> = wire.nets.iter().map(|net_id| match nets.get(net_id) {
					Some(net) => net.borrow().state,
					None => LogicState::Floating
				}).collect();
				wire.color = draw.styles.color_from_logic_states(&states);
			}
			// Use graphic item trait
			for ref_ in self.get_all_graphics_references() {
				if let GraphicSelectableItemRef::Pin(_) = ref_ {
					if !self.is_toplevel {
						continue;
					}
				}
				self.run_function_on_graphic_item(ref_, |graphic_item| graphic_item.draw(draw));
			}
		}
	}
	fn get_circuit(&self) -> &Self {
		&self
	}
	fn get_circuit_mut(&mut self) -> &mut Self {
		self
	}
	fn is_toplevel_circuit(&self) -> bool {
		self.is_toplevel
	}
	fn is_circuit(&self) -> bool {
		true
	}
	fn get_pin_position_override(&self, pin_id: u64) -> Option<(IntV2, FourWayDir, f32)> {
		if self.displayed_as_block {
			match self.block_pin_positions.get(&pin_id) {
				Some((pos, dir, _)) => Some((*pos, *dir, 1.0)),
				None => None
			}
		}
		else {
			self.get_pin_position(pin_id)
		}
	}
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::ReloadCircuit(false, self.self_reload_err_opt.clone())
		]
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::ReloadCircuit(reload, _) = property {
			if reload {
				match resource_interface::load_circuit(&self.save_path, self.displayed_as_block, false, self.generic_device.ui_data.position, self.generic_device.ui_data.direction, self.generic_device.name.clone()) {
					Ok(new) => {
						*self = new;
					},
					Err(err) => {
						self.self_reload_err_opt = Some(err);
					}
				}
			}
		}
	}
}