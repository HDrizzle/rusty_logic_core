//! Heavily based off of the logic simulation I wrote in TS for use w/ MotionCanvas, found at https://github.com/HDrizzle/stack_machine/blob/main/presentation/src/logic_sim.tsx

use std::{cell::{RefCell, RefMut}, collections::{HashMap, HashSet}, default::Default, fmt::Debug, fs, ops::{Deref, DerefMut}, rc::Rc};
use serde::{Deserialize, Serialize};
use crate::{circuit_net_computation::NotAWire, prelude::*, resource_interface, clock::Clock};
use resource_interface::LogicCircuitSave;
use common_macros::hash_map;

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
	ExternalConnection(u64),
	/// A set of things connected together
	/// This also depends on context, for example the nets one either side of a sub-circuit pin would be referenced within different "namespaces"
	Net(u64),
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

/// Any Logic pin within a circuit
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CircuitWideLogicPinReference {
	ComponentPin(ComponentLogicPinReference),
	/// External logical pin ID
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

/// Could be a simple gate, or something more complicated like an adder, or another circuit, or maybe even a whole computer
pub trait LogicDevice: Debug + GraphicSelectableItem where Self: 'static {
	fn get_generic(&self) -> &LogicDeviceGeneric;
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric;
	/// One compute step, Returns: Vec of nets that have been changed
	fn compute_step(&mut self, ancestors: &AncestryStack, self_component_id: u64, clock_state: bool, first_propagation_step: bool) -> Vec<u64>;
	fn save(&self) -> Result<EnumAllLogicDevices, String>;
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>);
	/// In CircuitVerse there can be, for example, one AND gate that acts like 8 gates, with 8-bit busses going in and out of it
	fn get_bit_width(&self) -> Option<u16> {None}
	#[allow(unused)]
	fn set_bit_width(&mut self, bit_width: u16) {}
	fn is_toplevel_circuit(&self) -> bool {false}
	fn is_circuit(&self) -> bool {false}
	/// Everything besides what `impl<T: LogicDevice> GraphicSelectableItem for T::get_properties()` generates
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {Vec::new()}
	/// Everything besides what `impl<T: LogicDevice> GraphicSelectableItem for T::set_property()` accepts
	#[cfg(feature = "using_egui")]
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
	fn set_all_logic_pin_states(&mut self, states: Vec<(u64, LogicState, LogicDriveSource)>) -> Result<(), String> {
		for (pin_query, state, _source) in states {
			self.set_logic_pin_external_state(pin_query, state)?;
		}
		Ok(())
	}
	fn get_pin_state_panic(&self, pin_query: u64) -> LogicState {
		self.get_logic_pins_cell().borrow().get(&pin_query).expect(&format!("Pin query {:?} for logic device \"{}\" not valid", &pin_query, &self.get_generic().name)).borrow().state()
	}
	/// `changed_pins` is part of a data structure kept by the main simulation function to update the simulation tree
	fn set_pin_internal_state_panic(&mut self, pin_query: u64, new_state: LogicState, changed_nets: &mut Vec<u64>) {
		let mut binding = self.get_logic_pins_cell().borrow_mut();
		let pin = &mut binding.get_mut(&pin_query).expect(&format!("Pin query {:?} not valid", &pin_query)).borrow_mut();
		let state = &mut pin.internal_state;
		if new_state != *state {
			*state = new_state;
			if let Some(ext_source) = &pin.external_source {
				if let LogicConnectionPinExternalSource::Net(net_id) = ext_source {
					changed_nets.push(*net_id);
				}
			}
		}
	}
	fn into_box(self: Box<Self>) -> Box<dyn LogicDevice> where Self: Sized {
		self as Box<dyn LogicDevice>
	}
	#[allow(unused)]
	fn set_instance_config(&mut self, instance_config: &ComponentInstanceConfig) {}
	fn get_instance_config_opt(&self) -> Option<ComponentInstanceConfig> {None}
	/// Whether to start a new propagation event which starts because of this device changing its output
	/// This should not modify self, since `compute()` will be called soon after
	fn start_of_propagation(&self) -> bool {false}
}

/// Everything that implements `Component` also automatically works with the graphics
impl<T: LogicDevice> GraphicSelectableItem for T {
	fn draw<'a>(&self, draw_parent: &Box<dyn DrawInterface>) {
		let draw = draw_parent.add_grid_pos_and_direction(self.get_generic().ui_data.position, self.get_generic().ui_data.direction);
		self.draw_except_pins(&draw);
		for (pin_id, pin) in self.get_generic().graphic_pins.borrow().iter() {
			let position: (IntV2, FourWayDir, f32) = self.get_pin_position_override(*pin_id).unwrap();
			let global_dir = position.1.rotate_intv2(draw.get_draw_data().direction.to_unit_int()).is_along_axis().unwrap();
			let vertical = global_dir == FourWayDir::N || global_dir == FourWayDir::S;
			if self.is_toplevel_circuit() {
				if pin.show_name {
					draw.text(
						&pin.name,//"test".to_owned(),
						position.0.to_v2() + (position.1.to_unit()*(1.2 + (pin.owned_pins.len() as f32)*2.0)),
						global_dir.opposite_direction().to_align2(),
						draw.styles().text_color,
						draw.styles().text_size_grid,
						vertical
					);
				}
			}
			else {
				draw.draw_polyline(
					vec![
						position.0.to_v2(),
						position.0.to_v2() - (position.1.to_unit() * position.2)
					],
					pin.get_color(&*draw.styles())
				);
				if pin.show_name {
					draw.text(
						&pin.name,//"test".to_owned(),
						position.0.to_v2() - (position.1.to_unit()*1.2),
						global_dir.to_align2(),
						draw.styles().text_color,
						draw.styles().text_size_grid,
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
	#[cfg(feature = "using_egui")]
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
	#[cfg(feature = "using_egui")]
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
	#[cfg(feature = "using_egui")]
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
	#[cfg(feature = "using_egui")]
	fn copy(&self) -> CopiedGraphicItem {
		CopiedGraphicItem::Component(self.save().unwrap(), self.get_instance_config_opt())
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

#[derive(Debug)]
pub struct LogicCircuit {
	pub generic_device: LogicDeviceGeneric,
	pub components: RefCell<HashMap<u64, RefCell<Box<dyn LogicDevice>>>>,
	pub nets: RefCell<HashMap<u64, RefCell<LogicNet>>>,
	pub wires: RefCell<HashMap<u64, RefCell<Wire>>>,
	pub splitters: RefCell<HashMap<u64, Splitter>>,
	pub labels: RefCell<HashMap<u64, GraphicLabel>>,
	pub save_name: String,
	/// Inspired by CircuitVerse, block-diagram version of circuit
	/// {pin ID: (relative position (ending), direction, whether to show name)}
	pub block_pin_positions: HashMap<u64, (IntV2, FourWayDir, bool)>,
	displayed_as_block: bool,
	pub is_toplevel: bool,
	/// Bounding box for the circuit, not the block diagram, relative to this circuit
	/// The block diagram BB can be found at `self.generic_device.bounding_box`
	pub circuit_internals_bb: (V2, V2),
	/// For example, "D Latch", not "Register #7"
	pub type_name: String,
	self_reload_err_opt: Option<String>,
	pub highlighted_net_opt: Option<u64>,
	pub clock: RefCell<Clock>,
	/// Timing diagram probes
	pub probes: RefCell<HashMap<u64, Probe>>,
	/// What order are the timing probes displayed
	/// NOTE: NOT USED AS THE INFORMATION SOURCE FOR TOPLEVEL, will be overruled by actual timing diagram tree. Should only be read when creating the timing diagram tree
	/// When saved (only toplevel circuit's actually get saved), use the timing diagram tree, NOT this
	pub timing_diagram_order: Vec<TimingDiagramTreeRootNodeSave>,
	pub bit_width_errors: Vec<BitWidthError>,
	/// Prevents clock changes when circuit propagation from something else (such as previous clock edge) isn't complete yet
	/// (Any change (propagation in progress), last clock state when timing diagram was updated)
	pub propagation_done: RefCell<(bool, bool)>,
	pub lib_name: String,
	/// If this is a subcircuit, whether to include this circuit in the timing diagram tree
	pub include_in_timing_diagram: bool,
	/// Determines if components including sub circuits have their instance config saved. Not saving can reduce file size by a lot.
	/// Can be overridden by parent if this is a subcircuit.
	pub save_instance_config: bool
}

impl LogicCircuit {
	pub fn new(
		components_not_celled: HashMap<u64, Box<dyn LogicDevice>>,
		external_graphic_pin_config: Vec<(IntV2, FourWayDir, f32, String, Vec<u64>)>,
		type_name: String,
		wires: HashMap<u64, Wire>,
		save_name: String,
		displayed_as_block: bool,
		is_toplevel: bool,
		lib_name: String
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
			save_name,
			block_pin_positions: HashMap::new(),
			displayed_as_block,
			is_toplevel,
			circuit_internals_bb: (V2::zeros(), V2::zeros()),
			type_name,
			self_reload_err_opt: None,
			highlighted_net_opt: None,
			clock: RefCell::new(Clock::default()),
			probes: RefCell::new(HashMap::new()),
			timing_diagram_order: vec![TimingDiagramTreeRootNodeSave::Clk],
			bit_width_errors: Vec::new(),
			propagation_done: RefCell::new((false, false)),
			lib_name: lib_name,
			include_in_timing_diagram: false,
			save_instance_config: true
		};
		new.setup_external_connection_sources();
		new.recompute_default_layout();
		new.check_wire_geometry_and_connections(None);
		Ok(new)
	}
	pub fn new_mostly_default(
		type_name: String,
		save_name: String,
		toplevel: bool,
		lib_name: String
	) -> Self {
		Self::new(
			HashMap::new(),
			Vec::new(),
			type_name,
			HashMap::new(),
			save_name,
			false,
			toplevel,
			lib_name
		).unwrap()
	}
	pub fn from_save(save: LogicCircuitSave, save_name: String, displayed_as_block: bool, toplevel: bool, pos: IntV2, dir: FourWayDir, name: String, lib_name: String, include_in_timing_diagram: bool) -> Result<Self, String> {
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
			(save.block_bb.0.to_v2(), save.block_bb.1.to_v2()),
			displayed_as_block,
			true
		);
		let probes = HashMap::from_iter(save.probes.into_iter().map(|(id, save)| (id, Probe::load(save))));
		let mut out = Self {
			generic_device: generic_device,
			components: RefCell::new(components),
			nets: RefCell::new(hash_map!(0 => RefCell::new(LogicNet::new(Vec::new())))),
			wires: RefCell::new(reconstructed_wires),
			splitters: RefCell::new(HashMap::from_iter(save.splitters.into_iter().map(|t| -> (u64, Splitter) {(t.0, Splitter::load(t.1))}))),
			labels: RefCell::new(HashMap::from_iter(save.labels.into_iter().map(|t| -> (u64, GraphicLabel) {(t.0, GraphicLabel::load(t.1))}))),
			save_name,
			block_pin_positions: save.block_pin_positions,
			displayed_as_block,
			is_toplevel: toplevel,
			circuit_internals_bb: (V2::zeros(), V2::zeros()),
			type_name: save.type_name,
			self_reload_err_opt: None,
			highlighted_net_opt: None,
			clock: RefCell::new(Clock::load(save.clock_enabled, save.clock_freq, save.clock_state)),
			probes: RefCell::new(probes),
			timing_diagram_order: save.timing_diagram_order,
			bit_width_errors: Vec::new(),
			propagation_done: RefCell::new((false, false)),
			lib_name,
			include_in_timing_diagram,
			save_instance_config: save.save_instance_config
		};
		out.setup_external_connection_sources();
		out.check_wire_geometry_and_connections(None);
		out.update_pin_block_positions();
		if let Some(instance_config) = save.instance_config_opt {
			out.set_instance_config_circuit(&instance_config);
		}
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
	/// Makes sure the the block layout stays valid
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
	pub fn insert_component(&self, comp_save: &EnumAllLogicDevices, instance_config_opt: Option<&ComponentInstanceConfig>) -> GraphicSelectableItemRef {
		let mut components = self.components.borrow_mut();
		let new_comp_id = lowest_unused_key(&components);
		let mut new_comp_box = EnumAllLogicDevices::to_dynamic(comp_save.clone()).unwrap();
		if let Some(instance_config) = instance_config_opt {
			new_comp_box.set_instance_config(instance_config);
		}
		components.insert(new_comp_id, RefCell::new(new_comp_box));
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
	pub fn set_graphic_item_following_mouse(&self, item_ref: GraphicSelectableItemRef, tool: &mut Tool) {
		*tool = Tool::Select{
			selected_graphics: HashSet::from_iter(vec![item_ref].into_iter()),
			selected_graphics_state: SelectionState::FollowingMouse(V2::zeros())
		};
	}
	/// Checks: All wires, external pins, component pins
	/// Returns: (Is termination point, Vec<Optional nets corresponding to bit width>, Optional shared wire connection set)
	pub fn is_connection_point(&self, point: IntV2) -> (bool, Vec<Option<u64>>, Option<Rc<RefCell<HashSet<WireConnection>>>>) {
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
	pub fn add_wire_geometry(
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
	/// Fixes everything, should be run when a new circuit is created/loaded or when anything is moved, deleted, or placed
	pub fn check_wire_geometry_and_connections(&mut self, timing_tree_opt: Option<&mut Vec<TimingDiagramTreeNode>>) {
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
		// Compute nets within this circuit, has to be done after all geometry and connection fixes
		self.bit_width_errors = self.recompute_nets_within_circuit();
		// Flatten all nets across all layers, only if toplevel
		if self.is_toplevel {
			self.flatten_nets_toplevel();
		}
		// Logic probes
		if let Some(timing_tree) = timing_tree_opt {
			self.update_probe_net_connections_and_timing(timing_tree);
		}
		// Last net computation setep
		self.update_logical_pin_to_wire_connections();
		// Recompute BB for circuit internals
		#[cfg(feature = "using_egui")]
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
	/// Run as part of circuit reconnection process after edit or on load
	/// Could be run as toplevel or as subcircuit, so `timing_tree` could be the whole timing tree (toplevel) or a subtree (if this is a sub-circuit)
	/// Does not edit `self.timing_diagram_order`, instead directly modifies timing diagram tree
	pub fn update_probe_net_connections_and_timing(&self, timing_tree: &mut Vec<TimingDiagramTreeNode>) {
		let mut probes = self.probes.borrow_mut();
		let components = self.components.borrow();
		// Set probe connections
		for probe in probes.values_mut() {
			probe.nets_opt = self.is_connection_point(probe.ui_data.position).1;
		}
		// Now update timing diagram to match
		// Add new probes & subcircuits
		let mut probes_already_in_tree = HashSet::<u64>::new();
		let mut circuits_already_in_tree = HashSet::<u64>::new();
		let mut has_clock = false;
		for node in &mut *timing_tree {
			match node {
				TimingDiagramTreeNode::Leaf(source, signal_group) => {
					match source {
						TimingDiagramSignalGroupSource::Probe(probe_id) => {
							probes_already_in_tree.insert(*probe_id);
						},
						TimingDiagramSignalGroupSource::Clk => {
							// Make sure length is one
							if signal_group.len() == 0 {
								signal_group.push(Vec::new());
							}
							while signal_group.len() > 1 {
								signal_group.pop();
							}
							has_clock = true;
						}
					}
				},
				TimingDiagramTreeNode::Branch(comp_id, _) => {
					circuits_already_in_tree.insert(*comp_id);
				}
			}
		}
		// Add missing probes
		for probe_id in probes.keys() {
			if !probes_already_in_tree.contains(probe_id) {
				timing_tree.push(TimingDiagramTreeNode::Leaf(TimingDiagramSignalGroupSource::Probe(*probe_id), Vec::new()));
			}
		}
		// Add missing sub circuits
		for (comp_id, comp_cell) in &*components {
			if !circuits_already_in_tree.contains(comp_id) {
				let comp = comp_cell.borrow();
				if comp.is_circuit() {// Fail-safe
					let circuit = comp.get_circuit();
					if circuit.include_in_timing_diagram {
						timing_tree.push(TimingDiagramTreeNode::Branch(*comp_id, circuit.build_timing_diagram_tree()));
					}
				}
			}
		}
		// Add clock if missing
		if !has_clock {
			timing_tree.insert(0, TimingDiagramTreeNode::Leaf(TimingDiagramSignalGroupSource::Clk, vec![Vec::<(TimingDiagramTimestamp, LogicState)>::new()]));
		}
		// Set timing signal group bit widths according to probes and remove them if the probe is gone
		let mut i: usize = 0;
		while i < timing_tree.len() {
			let mut valid_tree_node = true;
			match &mut timing_tree[i] {
				TimingDiagramTreeNode::Leaf(source, ref mut signal_group) => {
					if let TimingDiagramSignalGroupSource::Probe(probe_id) = source {
						if let Some(probe) = probes.get(probe_id) {
							let diff: isize = probe.nets_opt.len() as isize - (signal_group.len() as isize);
							if diff > 0 {// Probe has more bits than corresponding signal group, make a new one filled with floating states
								for _ in 0..diff {
									signal_group.push(Vec::new());
								}
							}
							if diff < 0 {
								for _ in 0..(-diff) {
									signal_group.pop();
								}
							}
						}
						else {
							valid_tree_node = false;
						}
					}
					// Otherwise it is the clock which is always valid
				},
				TimingDiagramTreeNode::Branch(comp_id,  sub_tree) => {
					// Sub-circuit, check if it should be in the tree
					if let Some(comp_cell) = components.get(&comp_id) {
						let comp = comp_cell.borrow();
						if comp.is_circuit() {
							let circuit = comp.get_circuit();
							if circuit.include_in_timing_diagram {
								// Recursively run this function in sub circuit
								circuit.update_probe_net_connections_and_timing(sub_tree);
							}
							else {
								// Not set to be included in timing diagram
								valid_tree_node = false;
							}
						}
						else {// Not a circuit
							valid_tree_node = false;
						}
					}
					else {// Doesn't exist
						valid_tree_node = false;
					}
				}
			}
			if !valid_tree_node {// This tree node is invalid, remove and decrement count to avoid skipping the next item
				timing_tree.remove(i);
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
								// TODO: FIX: When a wire is drawn with an elbow connecting to a pin, then back on itself to the pin, the wire ID is invalid
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
	/// Returns: Whether it was removed
	pub fn remove_graphic_item(&self, ref_: &GraphicSelectableItemRef) -> bool {
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
	/*fn delete_graphic_item(&mut self, ref_: GraphicSelectableItem) {
		let error_msg = format!("Graphic item reference {:?} cannot be found", &ref_);
		match ref_ {
			GraphicSelectableItemRef::Component(comp_id) => func(Box::new(logic_device_to_graphic_item(self.components.get_item_tuple(&(comp_id.into())).expect(&error_msg).1.borrow().deref().as_ref()))),
			GraphicSelectableItemRef::Wire(wire_id) => func(Box::new(wire_to_graphic_item(&self.wires.get_item_tuple(&(wire_id.into())).expect(&error_msg).1))),
			GraphicSelectableItemRef::Pin(pin_id) => func(Box::new(pin_to_graphic_item(&self.generic_device.pins.get_item_tuple(&(pin_id.into())).expect(&error_msg).1))),
		}
	}*/
	pub fn get_all_graphics_references(&self) -> Vec<GraphicSelectableItemRef> {
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
	#[cfg(feature = "using_filesystem")]
	pub fn flatten(&self, apply_transform: bool) -> Result<EnumAllLogicDevices, String> {
		let mut save = self.create_save_circuit(None).unwrap();
		let (wires, comps) = self.flatten_recursive(apply_transform)?;
		save.wires = vec_to_u64_keyed_hashmap(wires);
		save.components = vec_to_u64_keyed_hashmap(comps);
		let save_path = format!("{}_flattened", self.save_name);
		save.type_name = format!("{} (flattened)", save.type_name);
		let raw_string: String = to_string_err(serde_json::to_string(&save))?;
		to_string_err(fs::write(resource_interface::get_circuit_file_path(&save_path, &self.lib_name)?, &raw_string))?;
		Ok(EnumAllLogicDevices::SubCircuit(save_path, self.displayed_as_block, self.generic_device.ui_data.position, self.generic_device.ui_data.direction, self.generic_device.name.clone(), self.lib_name.clone(), self.include_in_timing_diagram))
	}
	/// Recursively extracts all sub-circuits that don't have a fixed sub-cycle count
	#[cfg(feature = "using_filesystem")]
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
				let (mut sub_wires, mut sub_comps) = circuit.flatten_recursive(true)?;
				wire_geometry.append(&mut sub_wires);
				components.append(&mut sub_comps);
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
	/// Toplevel compute function, keeps track of what needs to be updated to avoid wasting time
	/// Returns: Whether anything changed
	pub fn compute_toplevel(&self, first_propagation_step: bool, component_update_tree: &mut Vec<ComponentUpdateTreeNode>, update_all: bool) -> bool {
		assert!(self.is_toplevel);
		self.compute_immutable(&AncestryStack::new(), 0, first_propagation_step, component_update_tree, update_all).0
	}
	/// Main simulation function
	/// TODO: Trigger update on toplevel pin input change
	/// 1. Get components to update, If `first_propagation_step`:
	///        Check all components if `component.start_of_propagation()` returns true
	///    else:
	///        Use `self.components_to_update_next_step`
	/// 2. Update each component that needs updating and record the nets of their pins that were internally changed
	/// 3. Update nets attached to the changed pins
	/// 4. Update component pins and external pins
	/// 5. For each net that was changed, update all connections to it and make new hash set of components to be updated, save that to `self.components_to_update_next_step`
	/// Returns: (Whether anything changed, Vec of changed output nets)
	pub fn compute_immutable(
		&self,
		ancestors_above: &AncestryStack,
		self_component_id: u64,
		first_propagation_step: bool,
		component_update_tree: &mut Vec<ComponentUpdateTreeNode>,
		update_all: bool
	) -> (bool, Vec<u64>) {
		let mut propagation_states = self.propagation_done.borrow_mut();
		let mut changed = false;
		// Update clock, only if propagation is done
		if first_propagation_step && !(*propagation_states).0 {
			changed |= self.clock.borrow_mut().update();
		}
		let clock_state: bool = self.clock.borrow().state;
		// ------------------------------ NEW ------------------------------
		let components = self.components.borrow();
		// 1. Get components to update
		if first_propagation_step {
			component_update_tree.clear();
			for (id, comp_cell) in &*components {
				let comp = comp_cell.borrow();
				if comp.start_of_propagation() {
					component_update_tree.push(ComponentUpdateTreeNode{component_id: *id, sub_nodes: None});
				}
			}
		}
		// 2. Update components and record changed nets
		let mut changed_nets = HashSet::<u64>::new();
		let mut component_update_tree_indices_to_delete = Vec::<usize>::new();// Only delete component update nodes that are not sub circuits
		let ancestors = ancestors_above.push((&self, self_component_id));
		for (i, node_to_update) in component_update_tree.iter_mut().enumerate() {
			let comp_id = &node_to_update.component_id;
			let comp_cell = components.get(comp_id).unwrap();
			let is_circuit = comp_cell.borrow().is_circuit();
			let changed_nets_this_comp: Vec<u64> = if is_circuit {
				let comp = comp_cell.borrow();
				let circuit: &LogicCircuit = comp.get_circuit();
				// Add sub nodes to this node if it is `None`
				if node_to_update.sub_nodes.is_none() {
					(*node_to_update).sub_nodes = Some(Vec::new());
				}
				let (sub_circuit_changed, out) = circuit.compute_immutable(&ancestors, *comp_id, first_propagation_step, &mut node_to_update.sub_nodes.as_mut().unwrap(), update_all);
				changed |= sub_circuit_changed;
				out
			}
			else {
				let out = comp_cell.borrow_mut().compute_step(&ancestors, *comp_id, clock_state, first_propagation_step);
				component_update_tree_indices_to_delete.push(i);
				out
			};
			changed_nets.extend(changed_nets_this_comp);
		}
		// Clear component update tree
		for i_to_delete in component_update_tree_indices_to_delete.iter().rev() {
			component_update_tree.remove(*i_to_delete);
		}
		// 3. Update nets attached to the changed pins, the current code that uses the net's deep searching feature maybe outdated and overkill, TODO
		let nets = self.nets.borrow();
		let mut new_net_states = HashMap::<u64, (LogicState, Vec<GlobalSourceReference>)>::new();
		// Use the update everything flag
		if update_all {
			for net_id in nets.keys() {
				let net = nets.get(&net_id).unwrap();
				new_net_states.insert(*net_id, net.borrow().update_state(&ancestors, *net_id));
			}
		}
		else {
			for net_id in &changed_nets {
				let net = nets.get(&net_id).unwrap();
				new_net_states.insert(*net_id, net.borrow().update_state(&ancestors, *net_id));
			}
		}
		drop(ancestors);
		for (net_id, (state, sources)) in new_net_states.into_iter() {
			let binding = self.nets.borrow();
			let mut net_mut_ref = binding.get(&net_id).unwrap().borrow_mut();
			changed |= net_mut_ref.state != state;
			net_mut_ref.state = state;
			net_mut_ref.sources = sources;
		}
		// 4 & 5. Update component pins and external pins, update connections from nets (components and circuit outputs)
		// TODO
		let mut changed_parent_nets = Vec::<u64>::new();
		let ext_pins = self.generic_device.logic_pins.borrow_mut();
		for net_id in changed_nets {
			let net_cell = nets.get(&net_id).unwrap();
			let net = net_cell.borrow();
			for net_conn in &net.connections {
				match net_conn {
					CircuitWideLogicPinReference::ComponentPin(comp_pin_ref) => {
						// Find component pin and change it
						let comp_cell = components.get(&comp_pin_ref.component_id).unwrap();
						let comp = comp_cell.borrow();
						let pins_cell = &comp.get_generic().logic_pins;
						let pins = pins_cell.borrow();
						let pin_cell = pins.get(&comp_pin_ref.pin_id).unwrap();
						let mut pin = pin_cell.borrow_mut();
						pin.set_drive_external(net.state);
						// Record that this component needs to be updated on the next iteration
						component_update_tree.push(ComponentUpdateTreeNode::new(comp_pin_ref.component_id, None));
					},
					CircuitWideLogicPinReference::ExternalConnection(ext_pin_id) => {
						let pin_cell = ext_pins.get(ext_pin_id).unwrap();
						let mut pin = pin_cell.borrow_mut();
						pin.set_drive_internal(net.state);
						if let Some(ext_conn) = &pin.external_source {
							if let LogicConnectionPinExternalSource::Net(parent_net_id) = ext_conn {
								changed_parent_nets.push(*parent_net_id);
							}
						}
					}
				}
			}
		}
		// ------------------------------ /NEW ------------------------------
		/*
		// ------------------------------ OLD ------------------------------
		// Update net states
		let ancestors = ancestors_above.push((&self, self_component_id));
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
		// Done
		changed
		// ------------------------------ /OLD ------------------------------
		*/
		// Update propagation states
		(*propagation_states).0 = changed;
		(changed, changed_parent_nets)
	}
	/// Adds one time increment to the timing diagram data
	/// Also responsible for updating sub-circuits
	pub fn update_timing_diagram_toplevel(&self, propagation_states: &mut RefMut<'_, (bool, bool)>, timing: &mut TimingDiagram, mut first_propagation_step: bool) {
		if timing.running == TimingTiagramRunningState::Off {
			return;
		}
		if (!propagation_states.0) && timing.running == TimingTiagramRunningState::AnyChange {
			return;
		}
		// Ony if clock changed
		let clock = self.clock.borrow();
		if timing.running == TimingTiagramRunningState::Clk {
			// Clock changed, update clock recorded state and set clock flag
			if clock.state != propagation_states.1 {
				timing.current_event_started_by_clock = true;
				propagation_states.1 = clock.state;
			}
			if !timing.current_event_started_by_clock {
				if propagation_states.0 {
					first_propagation_step = false;// Instead of returning, still record changes but no new events
				}
				else {
					return;
				}
			}
			// Propagation is done, cancel flag
			if !propagation_states.0 {
				timing.current_event_started_by_clock = false;
			}
		}
		propagation_states.1 = clock.state;
		// Update timing diagram timestamp, last step for first step of timing so incremental timing diagram looks better
		timing.update_timestamp(first_propagation_step);// !propagation_states.0);
		// Update net states
		self.update_timing_diagram_recursive(timing.current_timestamp, &mut timing.tree, clock.state);
		timing.n_samples += 1;
		// Its late in the night I am typing from the light of the keyboard
		// I can't fall asleep so im working on this. I wonder what I will get up to in the future. I an a nerd and I love computers (obviously because I am writing my own logic simulator
		// to redisign a computer I built myself in high school because I wanted to). However I also believe that video games are the biggest waste of life and I value being fit and being outside in nature,
		// or "touching grass" as they would say. I am good at what I do which has gotten me to being an ECE major at WPI, but the more I'm here the more I miss everything which has nothing to do with technology.
		// There's a fireship video "How to flex as a programmer" and in the end the programmer blows up his (valid gender assumption) computer and becomes a farmer. He notices dew on a spiderweb which reminds him of a silicon wafer which can compute billions of operations per second to run models we don't even understand. But the spider has just been flexing on him.
		// I want to go bikepacking really far (like across Canada) with some basic supplies, a ham radio (ok this is technology), and no internet.
		// Rant 2: No matter how advanced technology gets, such as AI, DoorDash, Cities, we will never get away from grass which we will always have to touch
		// This means that the less you touch grass the less prepared you are for life.
	}
	/// Recursive timing diagram tree update, actually does the stuff
	fn update_timing_diagram_recursive(&self, current_timestamp: TimingDiagramTimestamp, tree: &mut Vec<TimingDiagramTreeNode>, clock_state: bool) {
		let probes = self.probes.borrow();
		let nets = self.nets.borrow();
		for node in tree {
			match node {
				TimingDiagramTreeNode::Leaf(source, signal_group) => {
					match source {
						TimingDiagramSignalGroupSource::Probe(probe_id) => {
							let probe_net_ids = &probes.get(&probe_id).unwrap().nets_opt;
							for (i, net_opt) in probe_net_ids.iter().enumerate() {
								let state: LogicState = match net_opt {
									Some(net_id) => nets.get(net_id).unwrap().borrow().state,
									None => LogicState::Floating
								};
								TimingDiagram::push_state_if_different(&current_timestamp, signal_group, i, state);
							}
						},
						TimingDiagramSignalGroupSource::Clk => {
							assert!(signal_group.len() == 1);
							TimingDiagram::push_state_if_different(&current_timestamp, signal_group, 0, clock_state.into());
						}
					}
				}
				TimingDiagramTreeNode::Branch(comp_id, sub_tree) => {
					let components = self.components.borrow();
					let comp_cell = components.get(&comp_id).expect("Timing diagram references sub circuit which does not exist");
					let comp = comp_cell.borrow();
					assert!(comp.is_circuit(), "Timing diagram references sub circuit which is not actually a circuit");
					let circuit = comp.get_circuit();
					let sub_clock_state = circuit.clock.borrow().state;
					// I love recursion its so confusing and awesome
					circuit.update_timing_diagram_recursive(current_timestamp, sub_tree, sub_clock_state);
				}
			}
		}
	}
	/// Creates blank timing recording
	pub fn build_timing_diagram_tree(&self) -> Vec<TimingDiagramTreeNode> {
		let mut out = Vec::<TimingDiagramTreeNode>::new();
		// Iterate toplevel order
		for tree_node in &self.timing_diagram_order {
			match tree_node {
				//	i love my boyfriend , love haley <3
				TimingDiagramTreeRootNodeSave::Clk => {out.push(TimingDiagramTreeNode::Leaf(TimingDiagramSignalGroupSource::Clk, vec![Vec::<(TimingDiagramTimestamp, LogicState)>::new()]));},
				TimingDiagramTreeRootNodeSave::Probe(probe_id) => {out.push(TimingDiagramTreeNode::Leaf(TimingDiagramSignalGroupSource::Probe(*probe_id), Vec::new()));},
				TimingDiagramTreeRootNodeSave::Branch(component_id) => {// Subcircuit branch
					let components = self.components.borrow();
					if let Some(comp_cell) = components.get(component_id) {
						let component = comp_cell.borrow();
						// Check that this component is a circuit, its good to be fault tolerant
						if component.is_circuit() {
							out.push(TimingDiagramTreeNode::Branch(*component_id, component.get_circuit().build_timing_diagram_tree()));
							// 2nd day of initiation is tonight, more top secret stuff
						}
					}
				}
			}
		}
		// Done
		out
	}
	pub fn create_save_circuit(&self, timing_diagram_opt: Option<&TimingDiagram>) -> Result<LogicCircuitSave, String> {
		// Convert components to enum variants to be serialized
		let mut components_save = HashMap::<u64, EnumAllLogicDevices>::new();
		for (ref_, component) in self.components.borrow().iter() {
			// Because this is the toplevel circuit, use `self.save_component_instance_config` to determine if all lower circuits save stuff
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
		// Instance config
		let instance_config_opt: Option<CircuitInstanceConfig> = match self.save_instance_config {
			true => Some(self.get_instance_config_circuit()),
			false => None
		};
		// Timing diagram order, if timing diagram is supplied then use that, otherwise copy existing display order
		let timing_diagram_order: Vec<TimingDiagramTreeRootNodeSave> = match timing_diagram_opt {
			Some(timing) => {
				let mut out = Vec::<TimingDiagramTreeRootNodeSave>::new();
				for node in &timing.tree {
					out.push(match node {
						TimingDiagramTreeNode::Branch(sub_circuit_comp_id, _) => TimingDiagramTreeRootNodeSave::Branch(*sub_circuit_comp_id),
						TimingDiagramTreeNode::Leaf(source, _) => match source {
							TimingDiagramSignalGroupSource::Clk => TimingDiagramTreeRootNodeSave::Clk,
							TimingDiagramSignalGroupSource::Probe(probe_id) => TimingDiagramTreeRootNodeSave::Probe(*probe_id)
						}
					});
				}
				out
			},
			None => self.timing_diagram_order.clone()
		};
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
			clock_enabled: clock.enabled,
			clock_freq: clock.freq,
			clock_state: clock.state,
			probes: HashMap::from_iter(self.probes.borrow().iter().map(|(id, probe)| (*id, probe.save()))),
			timing_diagram_order,//self.timing.borrow().signal_groups.iter().enumerate().filter(|(i, _)| *i > 0).map(|(_, (probe_id, _))| *probe_id).collect(),
			block_bb: (round_v2_to_intv2(self.generic_device.ui_data.local_bb.0), round_v2_to_intv2(self.generic_device.ui_data.local_bb.1)),
			instance_config_opt,
			save_instance_config: self.save_instance_config
		})
	}
	#[cfg(feature = "using_filesystem")]
	pub fn save_circuit_toplevel(&self, timing_diagram_opt: Option<&TimingDiagram>) -> Result<(), String> {
		let save = self.create_save_circuit(timing_diagram_opt)?;
		let raw_string: String = to_string_err(serde_json::to_string(&save))?;
		to_string_err(fs::write(resource_interface::get_circuit_file_path(&self.save_name, &self.lib_name)?, &raw_string))?;
		Ok(())
	}
	pub fn draw_as_block<'a>(&self, draw: &Box<dyn DrawInterface>, for_block_layout_edit: bool) {
		let draw_data = draw.get_draw_data();
		let styles = &draw.get_draw_data().styles;
		// Rectangle
		draw.draw_polyline(
			vec![
				V2::new(self.generic_device.ui_data.local_bb.0.x, self.generic_device.ui_data.local_bb.0.y),
				V2::new(self.generic_device.ui_data.local_bb.1.x, self.generic_device.ui_data.local_bb.0.y),
				V2::new(self.generic_device.ui_data.local_bb.1.x, self.generic_device.ui_data.local_bb.1.y),
				V2::new(self.generic_device.ui_data.local_bb.0.x, self.generic_device.ui_data.local_bb.1.y),
				V2::new(self.generic_device.ui_data.local_bb.0.x, self.generic_device.ui_data.local_bb.0.y)
			],
			styles.color_foreground
		);
		// Pins at alternate locations
		for (pin_id, pin) in self.generic_device.graphic_pins.borrow().iter() {
			let pin_alternate_config = self.block_pin_positions.get(pin_id).expect("Pin missing from block layout");
			let pin_stroke = match for_block_layout_edit {
				true => styles.color_foreground,
				false => styles.color_from_logic_states(&pin.states())
			};
			draw.draw_polyline(
				vec![
					pin_alternate_config.0.to_v2(),
					(pin_alternate_config.0 - pin_alternate_config.1.to_unit_int()).to_v2()
				],
				pin_stroke
			);
			if for_block_layout_edit {
				draw.draw_circle_filled(pin_alternate_config.0.to_v2(), styles.connection_dot_grid_size, styles.color_wire_floating);
			}
			// Pin name
			if pin.show_name {
				draw.text(
					&pin.name,
					pin_alternate_config.0.to_v2() - (pin_alternate_config.1.to_unit()*1.2),
					pin_alternate_config.1.rotate_intv2(draw_data.direction.to_unit_int()).is_along_axis().unwrap().to_align2(),
					draw.styles().text_color,
					draw.styles().text_size_grid,
					!draw_data.direction.is_horizontal()
				);
			}
		}
		// Name
		// TODO: Fix alignment
		//let direction = draw.get_draw_data().direction;
		draw.text(
			&self.type_name,
			V2::zeros(),//direction.rotate_v2(V2::new(0.0, -draw.text_size(&self.type_name, styles.text_size_grid).x/2.0)),// Relative
			GenericAlign2::CENTER_CENTER,//direction.to_align2(),//,
			styles.text_color,
			styles.text_size_grid,
			false// !direction.is_horizontal()
		);
	}
	/// Most functionality is here, `LogicDevice::set_instance_config()` is a wrapper
	pub fn set_instance_config_circuit(&mut self, instance_config: &CircuitInstanceConfig) {
		// Set component pin states
		let components = self.components.borrow();
		for (comp_id, pin_states) in &instance_config.component_pin_states {
			if let Some(component_cell) = components.get(comp_id) {
				let binding = component_cell.borrow();
				let pins = binding.get_generic().logic_pins.borrow();
				for (pin_id, pin_state) in pin_states {
					if let Some(pin_cell) = pins.get(pin_id) {
						pin_cell.borrow_mut().set_drive_internal(*pin_state);
					}
				}
			}
		}
		// Other component states
		for (comp_id, comp_config) in &instance_config.component_save_states {
			if let Some(component_cell) = components.get(comp_id) {
				let mut binding = component_cell.borrow_mut();
				binding.set_instance_config(comp_config);
			}
		}
	}
	/// Most functionality is here, `LogicDevice::get_instance_config_opt()` is a wrapper
	/// Generates a tree of everything
	pub fn get_instance_config_circuit(&self) -> CircuitInstanceConfig {
		// Iterate components
		let mut component_pin_states = HashMap::<u64, HashMap::<u64, LogicState>>::new();
		let mut component_save_states = HashMap::<u64, ComponentInstanceConfig>::new();
		let components = self.components.borrow();
		for (comp_id, comp_cell) in &*components {
			// Get component pin states
			let binding = comp_cell.borrow();
			let pins = binding.get_generic().logic_pins.borrow();
			let mut pin_states = HashMap::<u64, LogicState>::new();
			for (pin_id, pin_cell) in &*pins {
				pin_states.insert(*pin_id, pin_cell.borrow().internal_state);
			}
			component_pin_states.insert(*comp_id, pin_states);
			// Get component generic config
			if let Some(config) = binding.get_instance_config_opt() {
				component_save_states.insert(*comp_id, config);
			}
		}
		// Get component generic config
		CircuitInstanceConfig {
			component_pin_states,
			component_save_states
		}
	}
	/// The only thing special is it uses the tool to draw what the user is doing (drawing a new wire, selecting items, etc), and draws bit width errors
	pub fn draw_toplevel<'a>(&self, draw: &Box<dyn DrawInterface>, tool: &mut Tool) {
		let mouse_pos_grid_opt: Option<V2> = match draw.get_draw_data().mouse_pos {
			Some(pos_px) => Some(draw.get_draw_data().mouse_pos2_to_grid(pos_px)),
			None => None
		};
		match &tool {
			Tool::Select{selected_graphics, selected_graphics_state} => {
				match selected_graphics_state {
					SelectionState::Fixed => {},
					SelectionState::Dragging(start_grid, delta_grid) => {
						if selected_graphics.is_empty() {
							draw.draw_rect(*start_grid, *start_grid + *delta_grid, draw.styles().select_rect_color, draw.styles().select_rect_edge_color);
						}
					},
					SelectionState::FollowingMouse(_) => {}
				}
				// Draw selected items BB
				#[cfg(feature = "using_egui")]
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
					draw.draw_rect(bb.0, bb.1, [0, 0, 0, 0], draw.styles().select_rect_edge_color);
				}
			},
			Tool::HighlightNet => {},
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
								], draw.styles().color_wire_in_progress);
							}
						}
					}
				}
			}
		}
		for bit_width_error in &self.bit_width_errors {
			// Get positions of all connections involved in the error
			let mut connection_positions_and_bws = Vec::<(IntV2, u16)>::new();
			// if the mouse position is hovering
			let mut connection_hover_index = Option::<usize>::None;
			for (not_wire_conn, bw) in &bit_width_error.0 {
				let pos: IntV2 = match not_wire_conn {
					NotAWire::Pin(graphic_pin_id) => match graphic_pin_id {
						CircuitWideGraphicPinReference::ComponentPin(comp_pin_ref) => {
							let components = self.components.borrow();
							let comp = components.get(&comp_pin_ref.component_id).unwrap().borrow();
							let comp_local_pos: IntV2 = comp.get_pin_position_override(comp_pin_ref.pin_id).unwrap().0;
							comp.get_ui_data().pos_to_parent_coords(comp_local_pos)
						},
						CircuitWideGraphicPinReference::ExternalConnection(ext_conn_graphic_id) => self.generic_device.graphic_pins.borrow().get(&ext_conn_graphic_id).unwrap().ui_data.position
					},
					NotAWire::Splitter(splitter_id, splitter_pin_index) => {
						let splitters = self.splitters.borrow();
						let splitter = splitters.get(&splitter_id).unwrap();
						splitter.ui_data.pos_to_parent_coords(Splitter::pin_pos_local(*splitter_pin_index))
					}
				};
				if let Some(mouse_pos) = mouse_pos_grid_opt {
					if (pos.to_v2() - mouse_pos).magnitude() < 0.707 {
						connection_hover_index = Some(connection_positions_and_bws.len());
					}
				}
				connection_positions_and_bws.push((pos, *bw));
			}
			// Display them
			for (i, (pos, bw)) in connection_positions_and_bws.iter().enumerate() {
				let pos_v2 = pos.to_v2();
				draw.draw_polyline(vec![IntV2(-1, -1), IntV2(1, 1)].iter().map(|intv| pos_v2 + intv.to_v2() / 2.0).collect(), draw.styles().color_error_x);
				draw.draw_polyline(vec![IntV2(1, -1), IntV2(-1, 1)].iter().map(|intv| pos_v2 + intv.to_v2() / 2.0).collect(), draw.styles().color_error_x);
				draw.text(&format!("{} Bits", bw), pos.to_v2() + V2::new(0.7, 0.0), GenericAlign2::LEFT_CENTER, draw.styles().text_color, draw.styles().text_size_grid, false);
				if let Some(hovered_conn_i) = connection_hover_index {
					if hovered_conn_i == i {
						for (i2, (pos2, _)) in connection_positions_and_bws.iter().enumerate() {
							if i2 != i {
								draw.draw_polyline(vec![pos.to_v2(), pos2.to_v2()], draw.styles().color_error_x);
							}
						}
					}
				}
			}
		}
		// Everything else
		self.draw(draw);
	}
}

impl LogicDevice for LogicCircuit {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic_device
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic_device
	}
	fn compute_step(&mut self, _: &AncestryStack, _: u64, _: bool, _: bool) -> Vec<u64> {
		panic!("Logic circuit `compute_step()` method should not be used, instead directly call `compute_immutable()` or `compute_toplevel()`");
		//self.compute_immutable(ancestors_above, self_component_id, first_propagation_step).1
	}
	/// Returns handle to file for inclusion in other circuits
	/// The actual save is done with `LogicCircuit::save_circuit()`
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		// Path to save file
		Ok(EnumAllLogicDevices::SubCircuit(self.save_name.clone(), self.displayed_as_block, self.get_ui_data().position, self.get_ui_data().direction, self.generic_device.name.clone(), self.lib_name.clone(), self.include_in_timing_diagram))
	}
	fn draw_except_pins<'a>(&self, draw: &Box<dyn DrawInterface>) {
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
				wire.color = draw.styles().color_from_logic_states(&states);
			}
			// Use graphic item trait
			for ref_ in self.get_all_graphics_references() {
				if let GraphicSelectableItemRef::Pin(_) = ref_ {
					if !self.is_toplevel {
						continue;
					}
				}
				// TODO: Tell thingy if it is connected to a highlighted net
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
	#[cfg(feature = "using_egui")]
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::ReloadCircuit(false, self.self_reload_err_opt.clone()),
			SelectProperty::IncludeInTimingDiagram(self.include_in_timing_diagram),
			SelectProperty::Name(self.generic_device.name.clone())
		]
	}
	#[cfg(feature = "using_egui")]
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::ReloadCircuit(reload, _) = property {
			if reload {
				match resource_interface::load_circuit(&self.save_name, self.displayed_as_block, false, self.generic_device.ui_data.position, self.generic_device.ui_data.direction, self.generic_device.name.clone(), self.lib_name.clone(), self.include_in_timing_diagram) {
					Ok(new) => {
						*self = new;
					},
					Err(err) => {
						self.self_reload_err_opt = Some(err);
					}
				}
			}
		}
		if let SelectProperty::IncludeInTimingDiagram(state) = property {
			self.include_in_timing_diagram = state;
		}
		if let SelectProperty::Name(new_name) = property {
			self.generic_device.name = new_name;
		}
	}
	fn set_instance_config(&mut self, instance_config_generic: &ComponentInstanceConfig) {
		if let ComponentInstanceConfig::Circuit(instance_config) = instance_config_generic {
			self.set_instance_config_circuit(instance_config);
		}
	}
	fn get_instance_config_opt(&self) -> Option<ComponentInstanceConfig> {
		Some(ComponentInstanceConfig::Circuit(self.get_instance_config_circuit()))
	}
}

/// Saved along with a toplevel circuit save file to keep data specific to each instance of every component / sub circuit so two different uses of the circuit cannot conflict with each other when saved
/// Instead the memory contents will be saved in an instance config for "SubCircuit A" so it can be different for both "Circuit A" and "Circuit B":
/// 
/// Toplevel Circuit A
/// |- SubCircuit A
///    |- Nonvolatile memory
///    |- SubCircuit B
///       |- Nonvolatile memory
/// |- CircuitInstanceConfig
///    |- Memory contents
///    |- SubCircuit B instance config
///       |- Memory contents
/// 
/// Toplevel Circuit B
/// |- SubCircuit A
///    |- Nonvolatile memory
///    |- SubCircuit B
///       |- Nonvolatile memory
/// |- CircuitInstanceConfig
///    |- Memory contents
///    |- SubCircuit B instance config
///       |- Memory contents
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct CircuitInstanceConfig {
	/// Internal states, {Comp ID: {Logical pin ID: State}}
	/// There are no global pin states because the higher-up will have this circuit as a component anyway
	pub component_pin_states: HashMap<u64, HashMap<u64, LogicState>>,
	/// {Comp ID: Instance config}
	pub component_save_states: HashMap<u64, ComponentInstanceConfig>
}

/// Represents ONLY the electrical state of a component, such as Nonvolatile memory contents or the state of a flip flop. DOES NOT represent configuration such as bit width
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ComponentInstanceConfig {
	/// Sub circuit
	Circuit(CircuitInstanceConfig),
	/// Up to 256 bits for a latch
	Latch(u128, u128),
	/// Nonvolatile memory contents
	Memory(Vec<u8>),
	/// Address, Data, Pixel data
	PxDisplayState(u8, u8, Vec<u8>)
}

/// For keeping track of what components/components in sub-circuits need to be updated for each propagation step
#[derive(Debug, Clone)]
pub struct ComponentUpdateTreeNode {
	pub component_id: u64,
	/// In case this is a sub circuit
	pub sub_nodes: Option<Vec<ComponentUpdateTreeNode>>
}

impl ComponentUpdateTreeNode {
	pub fn new(component_id: u64, sub_nodes: Option<Vec<ComponentUpdateTreeNode>>) -> Self {
		Self {
			component_id,
			sub_nodes
		}
	}
}