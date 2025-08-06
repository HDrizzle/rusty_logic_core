use crate::{prelude::*, simulator::AncestryStack};
use serde::{Deserialize, Serialize};
use common_macros::hash_map;
use serde_json::to_string;
use std::{collections::HashMap, time::{Duration, Instant}};

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
		Clock::new().save().unwrap(),
		FixedSource::new().save().unwrap(),
		EncoderOrDecoder::new().save().unwrap(),
		Memory::new().save().unwrap()
	]
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateAnd(LogicDeviceGeneric);

impl GateAnd {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false),
				2 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateNand(LogicDeviceGeneric);

impl GateNand {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false),
				2 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateNot(LogicDeviceGeneric);

impl GateNot {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, 0), FourWayDir::W, 1.0, "a".to_owned(), false),
				1 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateOr(LogicDeviceGeneric);

impl GateOr {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false),
				2 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateNor(LogicDeviceGeneric);

impl GateNor {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 1.0, "a".to_owned(), false),
				1 => (IntV2(-3, 1), FourWayDir::W, 1.0, "b".to_owned(), false),
				2 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateXor(LogicDeviceGeneric);

impl GateXor {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 0.7, "a".to_owned(), false),
				1 => (IntV2(-3, 1), FourWayDir::W, 0.7, "b".to_owned(), false),
				2 => (IntV2(3, 0), FourWayDir::E, 1.0, "q".to_owned(), false),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateXnor(LogicDeviceGeneric);

impl GateXnor {
	pub fn new() -> Self {
		Self::from_save(LogicDeviceSave::default())
	}
	pub fn from_save(save: LogicDeviceSave) -> Self {
		Self(LogicDeviceGeneric::load(
			save,
			hash_map!(
				0 => (IntV2(-3, -1), FourWayDir::W, 0.7, "a".to_owned(), false),
				1 => (IntV2(-3, 1), FourWayDir::W, 0.7, "b".to_owned(), false),
				2 => (IntV2(4, 0), FourWayDir::E, 1.0, "q".to_owned(), false),
			),
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
			1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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
pub struct Clock {
	pub generic: LogicDeviceGeneric,
	pub enabled: bool,
	pub freq: f32,
	pub last_change: Instant
}

impl Clock {
	pub fn new() -> Self {
		Self::from_save(false, false, 1.0, IntV2(0, 0), FourWayDir::default(), String::new())
	}
	pub fn from_save(
		enabled: bool,
		state: bool,
		freq: f32,
		position_grid: IntV2,
		direction: FourWayDir,
		name: String
	) -> Self {
		let mut out = Self {
			generic: LogicDeviceGeneric::load(
				LogicDeviceSave::default(),
				hash_map!(
					0 => (IntV2(0, 0), FourWayDir::W, 1.0, "CLK".to_owned(), false)
				),
				(V2::new(-2.0, -2.0), V2::new(2.0, 2.0)),
				1,
			false
			),
			enabled,
			freq,
			last_change: Instant::now()
		};
		out.set_pin_internal_state_panic(0, state.into());
		out.generic.ui_data.position = position_grid;
		out.generic.ui_data.direction = direction;
		out.generic.name = name;
		out
	}
}

impl LogicDevice for Clock {
	fn get_generic(&self) -> &LogicDeviceGeneric {
		&self.generic
	}
	fn get_generic_mut(&mut self) -> &mut LogicDeviceGeneric {
		&mut self.generic
	}
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
		if self.enabled && self.last_change.elapsed() > Duration::from_secs_f32(0.5 / self.freq) {// The frequency is based on a whole period, it must change twice per period, so 0.5/f not 1/f
			self.set_pin_internal_state_panic(0, (!self.get_pin_state_panic(0).to_bool()).into());
			self.last_change = Instant::now();
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::Clock{enabled: self.enabled, state: self.get_pin_state_panic(0).to_bool(), freq: self.freq, position_grid: self.generic.ui_data.position, direction: self.generic.ui_data.direction, name: self.generic.name.clone()})
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
			].iter().map(|p| self.generic.ui_data.direction.rotate_v2_reverse(*p) + V2::new(2.0, 0.0)).collect(),
			draw.styles.color_foreground
		);
	}
	fn device_get_special_select_properties(&self) -> Vec<SelectProperty> {
		vec![
			SelectProperty::ClockEnabled(self.enabled),
			SelectProperty::ClockFreq(self.freq),
			SelectProperty::ClockState(self.get_pin_state_panic(0).to_bool())
		]
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::ClockEnabled(enable) => {self.enabled = enable;},
			SelectProperty::ClockFreq(freq) => {self.freq = freq;},
			SelectProperty::ClockState(state) => self.set_pin_internal_state_panic(0, state.into()),
			_ => {}
		}
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
				hash_map!(0 => (IntV2(0, 0), direction, 1.0, name, false)),
				bb,
				1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
		self.set_pin_internal_state_panic(0, self.state.into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::FixedSource(self.generic.save(), self.state))
	}
	fn draw_except_pins<'a>(&self, draw: &ComponentDrawInfo<'a>) {
		match self.state {
			true => {
				draw.draw_polyline(vec![V2::new(0.0, 1.0), V2::new(1.0, 1.0), V2::new(-1.0, 1.0)], draw.styles.color_foreground);
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
				pin_config,
				bb,
				1,
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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
	pub nonvolatile: bool
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
				pin_config,
				(bb_int.0.to_v2(), bb_int.1.to_v2()),
				1,
			true
			),
			addr_size,
			data: Self::format_data(data_opt, addr_size),
			nonvolatile
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
	fn compute_step(&mut self, _ancestors: &AncestryStack) {
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
			SelectProperty::MemoryNonvolatile(self.nonvolatile)
		]
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		if let SelectProperty::AddressWidth(new_addr_size, _) = property {
			*self = Self::from_save(self.generic.save(), new_addr_size, Some(self.data.clone()));
		}
		if let SelectProperty::MemoryNonvolatile(nonvolatile) = property {
			self.nonvolatile = nonvolatile;
		}
	}
}