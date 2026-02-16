//! For loading and saving things using JSON

use std::collections::HashMap;
#[cfg(feature = "using_filesystem")]
use std::fs;
use crate::{prelude::*, builtin_components::{self, BusLayoutSave}};
use serde::{Deserialize, Serialize};
#[cfg(feature = "using_filesystem")]
use serde_json;


// STATICS
#[cfg(feature = "using_filesystem")]
pub static RESOURCES_DIR: &str = "resources/";
#[cfg(feature = "using_filesystem")]
pub static CIRCUIT_LIBS_FILE: &str = "resources/circuit_libraries.json";
#[cfg(feature = "using_filesystem")]
pub static STYLES_FILE: &str = "resources/styles.json";

/// Loads circuit, path is relative to `CIRCUITS_DIR` and does not include .json extension
/// Example: File is located at `resources/circuits/sequential/d_latch.json` -> circuit path is `sequential/d_latch`
pub fn load_circuit(circuit_rel_path: &str, displayed_as_block: bool, toplevel: bool, pos: IntV2, dir: FourWayDir, name: String, mut lib_name: String, instance_config_opt: Option<&CircuitInstanceConfig>) -> Result<LogicCircuit, String> {
	if lib_name.is_empty() {
		lib_name = DEFAULT_CIRCUIT_LIB.to_owned();
	}
	#[allow(unused)]
	let mut save_opt: Option<LogicCircuitSave> = None;
	#[cfg(feature = "using_filesystem")]
	{
		let path = get_circuit_file_path(circuit_rel_path, &lib_name)?;
		let string_raw = to_string_err_with_message(load_file_with_better_error(&path), &format!("Could not load circuit file (toplevel={})", toplevel))?;
		save_opt = Some(match to_string_err_with_message(serde_json::from_str::<LogicCircuitSave>(&string_raw), &format!("Could not deserialize circuit file at \"{}\"", path)) {
			Ok(circuit) => Ok(circuit),
			Err(err_new_format) => {
				//println!("Load error: {}, attempting to load old format", &err_new_format);
				match restore_old_files::attempt_restore_file(&path) {
					Ok(circuit) => Ok(circuit),
					Err(err_old_format) => Err(format!("New format error: {}. Old format error for circuit \"{}\": {}", err_new_format, circuit_rel_path, err_old_format))
				}
			}
		}?);
	}
	#[cfg(feature = "using_wasm")]
	{
		let raw_string_opt = crate::js_get_circuit_save_file(circuit_rel_path);
		save_opt = match raw_string_opt {
			Some(string_raw) =>Some(to_string_err(serde_json::from_str::<LogicCircuitSave>(&string_raw))?),
			None => {return Err(format!("JS external code did not supply raw JSON for circuit \"{}\"", circuit_rel_path));}
		};
	}
	match save_opt {
		Some(save) => {
			let mut out = LogicCircuit::from_save(save, circuit_rel_path.to_string(), displayed_as_block, toplevel, pos, dir, name.clone(), lib_name.clone())?;
			if let Some(instance_config) = instance_config_opt {
				out.set_instance_config_circuit(instance_config);
			}
			Ok(out)
		},
		None => Err("Both `using_filesystem` and `using_wasm` features are disabled, therefore there is no mechanism for loading sub-circuits".to_owned())
	}
}

/// Like LogicCircuit but serializable
#[derive(Debug, Serialize, Deserialize)]
pub struct LogicCircuitSave {
	pub logic_pins: HashMap<u64, LogicConnectionPin>,
	/// {Pin ID: (Pos, Dir, Name, Show name, Owned logical pins)}
	pub graphic_pins: HashMap<u64, (IntV2, FourWayDir, String, bool, Vec<u64>)>,
	pub components: HashMap<u64, EnumAllLogicDevices>,
	pub wires: HashMap<u64, (IntV2, FourWayDir, u32)>,
	pub splitters: HashMap<u64, SplitterSave>,
	pub labels: HashMap<u64, GraphicLabelSave>,
	pub block_pin_positions: HashMap<u64, (IntV2, FourWayDir, bool)>,
	pub type_name: String,
	#[serde(default)]
	pub fixed_sub_cycles_opt: Option<usize>,
	#[serde(default)]
	pub clock_enabled: bool,
	#[serde(default)]
	pub clock_freq: f32,
	#[serde(default)]
	pub clock_state: bool,
	#[serde(default)]
	pub probes: HashMap<u64, ProbeSave>,
	#[serde(default)]
	pub timing_probe_order: Vec<u64>,
	#[serde(default)]
	pub block_bb: (IntV2, IntV2)
}

#[cfg(feature = "using_filesystem")]
mod restore_old_files {
	use super::*;
	/// Like LogicCircuit but serializable
	#[derive(Debug, Serialize, Deserialize)]
	struct LogicCircuitSaveOld {
		pub generic_device: GenericDeviceOld,
		pub components: HashMap<u64, EnumAllLogicDevices>,
		pub wires: HashMap<u64, (IntV2, FourWayDir, u32)>,
		pub block_pin_positions: HashMap<u64, (IntV2, FourWayDir, bool)>,
		pub type_name: String,
		#[serde(default)]
		pub fixed_sub_cycles_opt: Option<usize>
	}
	/// LogicDeviceGeneric shouldn't be serialized, so this struct has fields to be compatible with old save files
	#[derive(Debug, Serialize, Deserialize)]
	struct GenericDeviceOld {
		pub pins: HashMap<u64, LogicConnectionPinOld>,
		#[serde(default)]
		pub graphic_pins: HashMap<u64, (IntV2, FourWayDir, f32, String, bool, Vec<u64>)>,
		pub ui_data: UIData,
		pub name: String,
		pub bit_width: Option<u32>,
		pub show_name: bool
	}
	#[derive(Clone, Debug, Default, Serialize, Deserialize)]
	struct LogicConnectionPinOld {
		pub internal_source: Option<LogicConnectionPinInternalSource>,
		internal_state: LogicState,
		pub external_source: Option<LogicConnectionPinExternalSource>,
		pub external_state: LogicState,
		/// Usually 1, may be something else if theres a curve on an OR input or something
		pub length: f32,
		pub ui_data: UIData,
		pub bit_width: u32,
		/// Only for user, defaults to ""
		pub name: String,
		#[serde(default)]
		pub show_name: bool
	}
	impl Into<LogicCircuitSave> for LogicCircuitSaveOld {
		fn into(self) -> LogicCircuitSave {
			let mut graphic_pins = HashMap::<u64, (IntV2, FourWayDir, String, bool, Vec<u64>)>::new();
			let mut logic_pins = HashMap::<u64, LogicConnectionPin>::new();
			for (pin_id, pin) in self.generic_device.pins {
				graphic_pins.insert(pin_id, (
					pin.ui_data.position,
					pin.ui_data.direction,
					pin.name,
					pin.show_name,
					vec![pin_id]
				));
				logic_pins.insert(pin_id, LogicConnectionPin::new(pin.internal_source, pin.external_source));
			}
			LogicCircuitSave {
				logic_pins,
				graphic_pins,
				components: self.components,
				wires: self.wires,
				splitters: HashMap::new(),
				labels: HashMap::new(),
				block_pin_positions: self.block_pin_positions,
				type_name: self.type_name,
				fixed_sub_cycles_opt: self.fixed_sub_cycles_opt,
				clock_enabled: false,
				clock_freq: 1.0,
				clock_state: false,
				probes: HashMap::new(),
				timing_probe_order: Vec::new(),
				block_bb: (IntV2(0, 0), IntV2(0, 0))
			}
		}
	}
	pub fn attempt_restore_file(path: &str) -> Result<LogicCircuitSave, String> {
		let string_raw = load_file_with_better_error(&path)?;
		Ok(to_string_err(serde_json::from_str::<LogicCircuitSaveOld>(&string_raw))?.into())
	}
}

/// Not great but I can't think of anything else
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnumAllLogicDevices {
	/// (Relative path of circuit, Whether to use block diagram, Position, Orientation, Name, Library name, Instance-specific data)
	SubCircuit(String, bool, IntV2, FourWayDir, #[serde(default)]String, #[serde(default)]String, #[serde(default)]CircuitInstanceConfig),
	GateAnd(LogicDeviceSave),
	GateNand(LogicDeviceSave),
	GateNot(LogicDeviceSave),
	GateNotNew(LogicDeviceSave),
	GateOr(LogicDeviceSave),
	GateNor(LogicDeviceSave),
	GateXor(LogicDeviceSave),
	GateXnor(LogicDeviceSave),
	Clock(LogicDeviceSave),
	FixedSource(LogicDeviceSave, bool),
	EncoderOrDecoder(LogicDeviceSave, u8, bool, #[serde(default)]BusLayoutSave),
	Memory(
		LogicDeviceSave,
		u8,
		/// If this is Some then it is nonvolotile memory, otherwise it is RAM
		Option<Vec<u8>>,
		#[serde(default)]BusLayoutSave
	),
	TriStateBuffer(LogicDeviceSave),
	Adder(LogicDeviceSave, BusLayoutSave, u16),
	DLatch(LogicDeviceSave, BusLayoutSave, u16, u128, u128, bool, #[serde(default)]bool),
	Counter(LogicDeviceSave, BusLayoutSave, u16, u128, u128, bool),
	TriStateBufferNew(LogicDeviceSave, BusLayoutSave, u16),
	SRLatch(LogicDeviceSave, BusLayoutSave, bool),
	VectorCRT(LogicDeviceSave, BusLayoutSave),
	/// Generic save, Bus layout, Px size
	LED32Square(LogicDeviceSave, BusLayoutSave, u8)
}

impl EnumAllLogicDevices {
	pub fn to_dynamic(self_instance: Self) -> Result<Box<dyn LogicDevice>, String> {
		match self_instance {
			Self::SubCircuit(circuit_rel_path, displayed_as_block, pos, dir, name, lib_name, instance_config) => Ok(Box::new(to_string_err_with_message(load_circuit(&circuit_rel_path, displayed_as_block, false, pos, dir, name, lib_name, Some(&instance_config)), "While loading sub circuit")?)),
			Self::GateAnd(gate) => Ok(Box::new(builtin_components::GateAnd::from_save(gate))),
			Self::GateNand(gate) => Ok(Box::new(builtin_components::GateNand::from_save(gate))),
			Self::GateNot(gate) => Ok(Box::new(builtin_components::GateNot::from_save(gate))),
			Self::GateNotNew(gate) => Ok(Box::new(builtin_components::GateNotNew::from_save(gate))),
			Self::GateOr(gate) => Ok(Box::new(builtin_components::GateOr::from_save(gate))),
			Self::GateNor(gate) => Ok(Box::new(builtin_components::GateNor::from_save(gate))),
			Self::GateXor(gate) => Ok(Box::new(builtin_components::GateXor::from_save(gate))),
			Self::GateXnor(gate) => Ok(Box::new(builtin_components::GateXnor::from_save(gate))),
			Self::Clock(save) => Ok(Box::new(builtin_components::ClockSymbol::from_save(save))),
			Self::FixedSource(save, state) => Ok(Box::new(builtin_components::FixedSource::from_save(save, state))),
			Self::EncoderOrDecoder(save, addr_size, is_encoder, layout) => Ok(Box::new(builtin_components::EncoderOrDecoder::from_save(save, addr_size, is_encoder, layout))),
			Self::Memory(save, addr_size, data_opt, layout) => Ok(Box::new(builtin_components::Memory::from_save(save, addr_size, data_opt, layout))),
			Self::TriStateBuffer(save) => Ok(Box::new(builtin_components::TriStateBufferOld::from_save(save))),
			Self::Adder(save, layout, bw) => Ok(Box::new(builtin_components::Adder::from_save(save, layout, bw))),
			Self::DLatch(save, layout, bw, low, high, oe, sr) => Ok(Box::new(builtin_components::DLatch::from_save(save, layout, bw, low, high, oe, sr))),
			Self::Counter(save, layout, bw, low, high, oe) => Ok(Box::new(builtin_components::Counter::from_save(save, layout, bw, low, high, oe))),
			Self::TriStateBufferNew(save, layout, bw) => Ok(Box::new(builtin_components::TriStateBuffer::from_save(save, layout, bw))),
			Self::SRLatch(save, layout, state) => Ok(Box::new(builtin_components::SRLatch::from_save(save, layout, state))),
			Self::VectorCRT(save, layout) => Ok(Box::new(builtin_components::VectorCRT::from_save(save, layout))),
			Self::LED32Square(save, layout, px_size) => Ok(Box::new(builtin_components::LED32Square::from_save(save, layout, px_size)))
		}
	}
	/// Example: "AND Gate" or "D Latch", for the component search UI
	pub fn type_name(&self) -> String {
		match self {
			Self::SubCircuit(circuit_rel_path, _, _, _, _, _, _) => circuit_rel_path.clone(),
			Self::GateAnd(_) => "AND Gate".to_owned(),
			Self::GateNand(_) => "NAND Gate".to_owned(),
			Self::GateNot(_) => "NOT Gate (OLD)".to_owned(),
			Self::GateNotNew(_) => "NOT Gate".to_owned(),
			Self::GateOr(_) => "OR Gate".to_owned(),
			Self::GateNor(_) => "NOR Gate".to_owned(),
			Self::GateXor(_) => "XOR Gate".to_owned(),
			Self::GateXnor(_) => "XNOR Gate".to_owned(),
			Self::Clock(_) => "Clock Source".to_owned(),
			Self::FixedSource(_, state) => match state {true => "V+", false => "GND"}.to_owned(),
			Self::EncoderOrDecoder(_, _, is_encoder, _) => match *is_encoder {true => "Encoder", false => "Decoder"}.to_owned(),
			Self::Memory(_, _, _, _) => "Memory".to_owned(),
			Self::TriStateBuffer(_) => "Tri-State Buffer (old)".to_owned(),
			Self::Adder(_, _, _) => "Adder".to_owned(),
			Self::DLatch(_, _, _, _, _, _, _) => "Data Latch".to_owned(),
			Self::Counter(_, _, _, _, _, _) => "Counter".to_owned(),
			Self::TriStateBufferNew(_, _, _) => "Tri-State Buffer (new)".to_owned(),
			Self::SRLatch(_, _, _) => "SR Latch".to_owned(),
			Self::VectorCRT(_, _) => "Vector CRT".to_owned(),
			Self::LED32Square(_, _, _) => "32 x 32 px display".to_owned()
		}
	}
}

/// Lists all circuits from all libraries
/// Returns: Vec<(Lib name, Circuit name)>
#[cfg(feature = "using_filesystem")]
pub fn list_all_circuit_files() -> Result<Vec<(String, String)>, String> {
	let (libs, ordered_keys) = load_circuit_libraries()?;
	let mut out = Vec::<(String, String)>::new();
	for lib_name in &ordered_keys {
		let dir_ = libs.get(lib_name).unwrap();
		let mut this_dir_ordered = Vec::<String>::new();
		for dir_entry_result in fs::read_dir(dir_).unwrap() {
			let dir_entry = to_string_err(dir_entry_result)?;
			if to_string_err(dir_entry.metadata())?.is_file() {
				let mut with_extention = dir_entry.file_name().into_string().unwrap();
				if with_extention == MAC_DS_STORE {
					continue;
				}
				with_extention.truncate(with_extention.len() - 5);
				this_dir_ordered.push(with_extention);
			}
		}
		this_dir_ordered.sort_by(|a, b| a.partial_cmp(b).unwrap());
		for file_name in this_dir_ordered {
			out.push((lib_name.clone(), file_name));
		}
	}
	Ok(out)
}

#[cfg(feature = "using_filesystem")]
pub fn get_circuit_file_path(circuit_rel_path: &str, lib_name: &str) -> Result<String, String> {
	let libs = load_circuit_libraries()?.0;
	match libs.get(lib_name) {
		Some(lib_dir) => Ok(lib_dir.to_owned() + circuit_rel_path + ".json"),
		None => Err(format!("Could not get full path because circuit library \"{}\" doesn't exist", lib_name))
	}
}

#[cfg(feature = "using_filesystem")]
pub fn load_file_with_better_error(path: &str) -> Result<String, String> {// With help from ChatGPT because I'm lasy
	match fs::read_to_string(path) {
		Ok(contents) => Ok(contents),
		Err(err) => {
			// Combine the error with the path information
			Err(format!("Error reading file '{}': {}", path, err))
		}
	}
}

#[cfg(feature = "using_filesystem")]
pub fn load_circuit_libraries() -> Result<(HashMap<String, String>, Vec<String>), String> {
	let raw = load_file_with_better_error(CIRCUIT_LIBS_FILE)?;
	let libs = to_string_err(serde_json::from_str::<HashMap<String, String>>(&raw))?;
	// Check that they are valid directories and end with "/"
	for (lib_name, dir_) in &libs {
		if !(to_string_err_with_message(fs::metadata(dir_), &format!("Could not read metadata for \"{}\"", dir_))?.is_dir()) {
			return Err(format!("\"{dir_}\" for circuit library \"{lib_name}\" is not a directory"));
		}
		if !dir_.ends_with('/') {
			return Err(format!("\"{dir_}\" for circuit library \"{lib_name}\" does not end with \"/\""));
		}
	}
	// Check that the default library exists
	if !libs.contains_key(DEFAULT_CIRCUIT_LIB) {
		return Err(format!("There is no circuit library with the name \"{}\", which is required", DEFAULT_CIRCUIT_LIB));
	}
	// Get ordered key list
	let mut ordered_keys: Vec<String> = libs.keys().map(|s| s.clone()).collect::<Vec<String>>();
	ordered_keys.sort_by(|a, b| a.partial_cmp(b).unwrap());
	Ok((libs, ordered_keys))
}

/// Moves/Renames a circuit and possibly moves it between libraries, also reads all circuits from all libraries and updates references to the moved circuit
#[cfg(feature = "using_filesystem")]
pub fn move_circuit(old_lib: &str, old_name: &str, new_lib: &str, new_name: &str) -> Result<(), String> {
	let circuit_libs: HashMap<String, String> = load_circuit_libraries()?.0;
	// Check old and new lib names
	if !circuit_libs.contains_key(old_lib) {
		return Err(format!("Existing circuit's library \"{}\" does not exist", old_lib));
	}
	if !circuit_libs.contains_key(new_lib) {
		return Err(format!("Destination library \"{}\" does not exist", new_lib));
	}
	// Check that new name won't be a duplicate in the destination library
	let new_full_path = format!("{}{}.json", circuit_libs.get(new_lib).unwrap(), new_name);
	if to_string_err(fs::exists(&new_full_path))? {
		return Err(format!("Destination path \"{}\" already exists", &new_full_path));
	}
	let old_full_path = format!("{}{}.json", circuit_libs.get(old_lib).unwrap(), old_name);
	// Copy for now, because deleting the old file will make some circuits unable to load
	to_string_err(fs::copy(&old_full_path, new_full_path))?;
	// Go through all circuits in all files and make sure any reference to this circuit is updated
	for (lib_name, file_name) in list_all_circuit_files()? {
		// Check that its not checking the moved circuit
		if file_name == new_name && lib_name == new_lib {
			continue;
		}
		// Load circuit
		let circuit = to_string_err_with_message(load_circuit(&file_name, false, false, IntV2(0, 0), FourWayDir::default(), "".to_owned(), lib_name, None), "Could not load circuit during circuit move")?;
		// Check components
		let mut save = false;
		for (_, component_cell) in circuit.components.borrow_mut().iter() {
			let mut component = component_cell.borrow_mut();
			let comp_save = component.save()?;
			// (Relative path of circuit, Whether to use block diagram, Position, Orientation, Name, Library name)
			if let EnumAllLogicDevices::SubCircuit(comp_file_name, block, pos, dir, name, comp_lib_name, instance_config) = comp_save {
				if comp_file_name == old_name && comp_lib_name == old_lib {
					save = true;
					*component = EnumAllLogicDevices::to_dynamic(EnumAllLogicDevices::SubCircuit(new_name.to_owned(), block, pos, dir, name, new_lib.to_owned(), instance_config))?;
				}
			}
		}
		if save {
			to_string_err_with_message(circuit.save_circuit_toplevel(), "Could not save circuit during circuit move")?;
		}
	}
	// Delete old file
	to_string_err(fs::remove_file(old_full_path))?;
	Ok(())
}