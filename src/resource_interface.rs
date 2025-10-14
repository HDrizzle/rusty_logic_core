//! For loading and saving things using JSON

use std::collections::HashMap;
#[cfg(feature = "using_filesystem")]
use std::fs;
use crate::prelude::*;
use serde::{Deserialize, Serialize};
#[cfg(feature = "using_filesystem")]
use serde_json;
use crate::builtin_components::{self, BusLayoutSave};


// STATICS
#[cfg(feature = "using_filesystem")]
pub static RESOURCES_DIR: &str = "resources/";
#[cfg(feature = "using_filesystem")]
pub static CIRCUITS_DIR: &str = "resources/circuits/";
#[cfg(feature = "using_filesystem")]
pub static STYLES_FILE: &str = "resources/styles.json";

/// Loads circuit, path is relative to `CIRCUITS_DIR` and does not include .json extension
/// Example: File is located at `resources/circuits/sequential/d_latch.json` -> circuit path is `sequential/d_latch`
pub fn load_circuit(circuit_rel_path: &str, displayed_as_block: bool, toplevel: bool, pos: IntV2, dir: FourWayDir, name: String) -> Result<LogicCircuit, String> {
	#[allow(unused)]
	let mut save_opt: Option<LogicCircuitSave> = None;
	#[cfg(feature = "using_filesystem")]
	{
		let path = get_circuit_file_path(circuit_rel_path);
		let string_raw = load_file_with_better_error(&path)?;
		save_opt = Some(match to_string_err(serde_json::from_str::<LogicCircuitSave>(&string_raw)) {
			Ok(circuit) => Ok(circuit),
			Err(err_new_format) => {
				//println!("Load error: {}, attempting to load old format", &err_new_format);
				match restore_old_files::attempt_restore_file(circuit_rel_path) {
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
		Some(save) => LogicCircuit::from_save(save, circuit_rel_path.to_string(), displayed_as_block, toplevel, pos, dir, name.clone()),
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
	pub fn attempt_restore_file(circuit_rel_path: &str) -> Result<LogicCircuitSave, String> {
		let path = get_circuit_file_path(circuit_rel_path);
		let string_raw = load_file_with_better_error(&path)?;
		Ok(to_string_err(serde_json::from_str::<LogicCircuitSaveOld>(&string_raw))?.into())
	}
}

/// Not great but I can't think of anything else
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnumAllLogicDevices {
	/// (Relative path of circuit, Whether to use block diagram, Position, Orientation, Name)
	SubCircuit(String, bool, IntV2, FourWayDir, #[serde(default)]String),
	GateAnd(LogicDeviceSave),
	GateNand(LogicDeviceSave),
	GateNot(LogicDeviceSave),
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
	DLatch(LogicDeviceSave, BusLayoutSave, u16, u128, u128, bool),
	Counter(LogicDeviceSave, BusLayoutSave, u16, u128, u128, bool),
	TriStateBufferNew(LogicDeviceSave, BusLayoutSave, u16),
	SRLatch(LogicDeviceSave, BusLayoutSave, bool)
}

impl EnumAllLogicDevices {
	pub fn to_dynamic(self_instance: Self) -> Result<Box<dyn LogicDevice>, String> {
		match self_instance {
			Self::SubCircuit(circuit_rel_path, displayed_as_block, pos, dir, name) => Ok(Box::new(load_circuit(&circuit_rel_path, displayed_as_block, false, pos, dir, name)?)),
			Self::GateAnd(gate) => Ok(Box::new(builtin_components::GateAnd::from_save(gate))),
			Self::GateNand(gate) => Ok(Box::new(builtin_components::GateNand::from_save(gate))),
			Self::GateNot(gate) => Ok(Box::new(builtin_components::GateNot::from_save(gate))),
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
			Self::DLatch(save, layout, bw, low, high, oe) => Ok(Box::new(builtin_components::DLatch::from_save(save, layout, bw, low, high, oe))),
			Self::Counter(save, layout, bw, low, high, oe) => Ok(Box::new(builtin_components::Counter::from_save(save, layout, bw, low, high, oe))),
			Self::TriStateBufferNew(save, layout, bw) => Ok(Box::new(builtin_components::TriStateBuffer::from_save(save, layout, bw))),
			Self::SRLatch(save, layout, state) => Ok(Box::new(builtin_components::SRLatch::from_save(save, layout, state)))
		}
	}
	/// Example: "AND Gate" or "D Latch", for the component search UI
	pub fn type_name(&self) -> String {
		match self {
			Self::SubCircuit(circuit_rel_path, _, _, _, _) => circuit_rel_path.clone(),
			Self::GateAnd(_) => "AND Gate".to_owned(),
			Self::GateNand(_) => "NAND Gate".to_owned(),
			Self::GateNot(_) => "NOT Gate".to_owned(),
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
			Self::DLatch(_, _, _, _, _, _) => "Data Latch".to_owned(),
			Self::Counter(_, _, _, _, _, _) => "Counter".to_owned(),
			Self::TriStateBufferNew(_, _, _) => "Tri-State Buffer (new)".to_owned(),
			Self::SRLatch(_, _, _) => "SR Latch".to_owned()
		}
	}
}

#[cfg(feature = "using_filesystem")]
pub fn list_all_circuit_files() -> Result<Vec<String>, String> {
	let mut out = Vec::<String>::new();
	for dir_entry_result in fs::read_dir(CIRCUITS_DIR).expect("Cannot find circuits directory") {
		let dir_entry = to_string_err(dir_entry_result)?;
		if to_string_err(dir_entry.metadata())?.is_file() {
			let mut with_extention = dir_entry.file_name().into_string().unwrap();
			with_extention.truncate(with_extention.len() - 5);
			out.push(with_extention);
		}
	}
	Ok(out)
}

#[cfg(feature = "using_filesystem")]
pub fn get_circuit_file_path(circuit_rel_path: &str) -> String {
	CIRCUITS_DIR.to_owned() + circuit_rel_path + ".json"
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