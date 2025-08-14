//! For loading and saving things using JSON

use std::{fs, collections::HashMap};
use crate::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json;
use crate::builtin_components;


// STATICS
pub static RESOURCES_DIR: &str = "resources/";
pub static CIRCUITS_DIR: &str = "resources/circuits/";
pub static STYLES_FILE: &str = "resources/styles.json";

/// Loads circuit, path is relative to `CIRCUITS_DIR` and does not include .json extension
/// Example: File is located at `resources/circuits/sequential/d_latch.json` -> circuit path is `sequential/d_latch`
pub fn load_circuit(circuit_rel_path: &str, displayed_as_block: bool, toplevel: bool, pos: IntV2, dir: FourWayDir) -> Result<LogicCircuit, String> {
	let path = get_circuit_file_path(circuit_rel_path);
	let string_raw = load_file_with_better_error(&path)?;
	LogicCircuit::from_save(to_string_err(serde_json::from_str(&string_raw))?, circuit_rel_path.to_string(), displayed_as_block, toplevel, pos, dir)
}

/// Like LogicCircuit but serializable
#[derive(Debug, Serialize, Deserialize)]
pub struct LogicCircuitSave {
	pub generic_device: LogicCircuitSaveGenericDevice,
	pub components: HashMap<u64, EnumAllLogicDevices>,
	pub wires: HashMap<u64, (IntV2, FourWayDir, u32)>,
	pub block_pin_positions: HashMap<u64, (IntV2, FourWayDir, bool)>,
	pub type_name: String,
	#[serde(default)]
	pub fixed_sub_cycles_opt: Option<usize>
}

/// LogicDeviceGeneric shouldn't be serialized, so this struct has feilds to be compatible with old save files
#[derive(Debug, Serialize, Deserialize)]
pub struct LogicCircuitSaveGenericDevice {
	pub pins: HashMap<u64, LogicConnectionPin>,
	#[serde(default)]
	pub graphic_pins: HashMap<u64, (IntV2, FourWayDir, f32, String, bool, Vec<u64>)>,
	pub ui_data: UIData,
	pub name: String,
	pub bit_width: Option<u32>,
	pub show_name: bool
}

impl Into<LogicDeviceGeneric> for LogicCircuitSaveGenericDevice {
	fn into(self) -> LogicDeviceGeneric {
		// TODO
	}
}

/// Not great but I can't think of anything else
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EnumAllLogicDevices {
	/// (Relative path of circuit, Whether to use block diagram, Position, Orientation)
	SubCircuit(String, bool, IntV2, FourWayDir),
	GateAnd(LogicDeviceSave),
	GateNand(LogicDeviceSave),
	GateNot(LogicDeviceSave),
	GateOr(LogicDeviceSave),
	GateNor(LogicDeviceSave),
	GateXor(LogicDeviceSave),
	GateXnor(LogicDeviceSave),
	Clock {
		enabled: bool,
		state: bool,
		freq: f32,
		position_grid: IntV2,
		direction: FourWayDir,
		name: String
	},
	FixedSource(LogicDeviceSave, bool),
	EncoderOrDecoder(LogicDeviceSave, u8, bool),
	Memory(
		LogicDeviceSave,
		u8,
		/// If this is Some then it is nonvolotile memory, otherwise it is RAM
		Option<Vec<u8>>
	),
	TriStateBuffer(LogicDeviceSave)
}

impl EnumAllLogicDevices {
	pub fn to_dynamic(self_instance: Self) -> Result<Box<dyn LogicDevice>, String> {
		match self_instance {
			Self::SubCircuit(circuit_rel_path, displayed_as_block, pos, dir) => Ok(Box::new(load_circuit(&circuit_rel_path, displayed_as_block, false, pos, dir)?)),
			Self::GateAnd(gate) => Ok(Box::new(builtin_components::GateAnd::from_save(gate))),
			Self::GateNand(gate) => Ok(Box::new(builtin_components::GateNand::from_save(gate))),
			Self::GateNot(gate) => Ok(Box::new(builtin_components::GateNot::from_save(gate))),
			Self::GateOr(gate) => Ok(Box::new(builtin_components::GateOr::from_save(gate))),
			Self::GateNor(gate) => Ok(Box::new(builtin_components::GateNor::from_save(gate))),
			Self::GateXor(gate) => Ok(Box::new(builtin_components::GateXor::from_save(gate))),
			Self::GateXnor(gate) => Ok(Box::new(builtin_components::GateXnor::from_save(gate))),
			Self::Clock{enabled, state, freq, position_grid, direction, name} => Ok(Box::new(builtin_components::Clock::from_save(enabled, state, freq, position_grid, direction, name))),
			Self::FixedSource(save, state) => Ok(Box::new(builtin_components::FixedSource::from_save(save, state))),
			Self::EncoderOrDecoder(save, addr_size, is_encoder) => Ok(Box::new(builtin_components::EncoderOrDecoder::from_save(save, addr_size, is_encoder))),
			Self::Memory(save, addr_size, data_opt) => Ok(Box::new(builtin_components::Memory::from_save(save, addr_size, data_opt))),
			Self::TriStateBuffer(save) => Ok(Box::new(builtin_components::TriStateBuffer::from_save(save)))
		}
	}
	/// Example: "AND Gate" or "D Latch", for the component search UI
	pub fn type_name(&self) -> String {
		match self {
			Self::SubCircuit(circuit_rel_path, _, _, _) => circuit_rel_path.clone(),
			Self::GateAnd(_) => "AND Gate".to_owned(),
			Self::GateNand(_) => "NAND Gate".to_owned(),
			Self::GateNot(_) => "NOT Gate".to_owned(),
			Self::GateOr(_) => "OR Gate".to_owned(),
			Self::GateNor(_) => "NOR Gate".to_owned(),
			Self::GateXor(_) => "XOR Gate".to_owned(),
			Self::GateXnor(_) => "XNOR Gate".to_owned(),
			Self::Clock{enabled: _, state: _, freq: _, position_grid: _, direction: _, name: _} => "Clock Source".to_owned(),
			Self::FixedSource(_, state) => match state {true => "V+", false => "GND"}.to_owned(),
			Self::EncoderOrDecoder(_, _, is_encoder) => match *is_encoder {true => "Encoder", false => "Decoder"}.to_owned(),
			Self::Memory(_, _, _) => "Memory".to_owned(),
			Self::TriStateBuffer(_) => "Tri-State Buffer".to_owned()
		}
	}
}

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

pub fn get_circuit_file_path(circuit_rel_path: &str) -> String {
	CIRCUITS_DIR.to_owned() + circuit_rel_path + ".json"
}

pub fn load_file_with_better_error(path: &str) -> Result<String, String> {// With help from ChatGPT because I'm lasy
	match fs::read_to_string(path) {
		Ok(contents) => Ok(contents),
		Err(err) => {
			// Combine the error with the path information
			Err(format!("Error reading file '{}': {}", path, err))
		}
	}
}