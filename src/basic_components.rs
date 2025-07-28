use crate::{prelude::*, simulator::{AncestryStack, LogicConnectionPinInternalSource}};
use serde::{Deserialize, Serialize};
use common_macros::hash_map;
use std::time::{Instant, Duration};

/// For the component search popup
pub fn list_all_basic_components() -> Vec<EnumAllLogicDevices> {
	vec![
		GateAnd::new(IntV2(0, 0), "", FourWayDir::default()).save().unwrap(),
		GateNand::new(IntV2(0, 0), "", FourWayDir::default()).save().unwrap(),
		GateNot::new(IntV2(0, 0), "", FourWayDir::default()).save().unwrap(),
		GateOr::new(IntV2(0, 0), "", FourWayDir::default()).save().unwrap(),
		GateNor::new(IntV2(0, 0), "", FourWayDir::default()).save().unwrap(),
		GateXor::new(IntV2(0, 0), "", FourWayDir::default()).save().unwrap(),
		GateXnor::new(IntV2(0, 0), "", FourWayDir::default()).save().unwrap(),
		Clock::new(false, false, 1.0, IntV2(0, 0), FourWayDir::default()).save().unwrap()
	]
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateAnd(LogicDeviceGeneric);

impl GateAnd {
	pub fn new(
		position_grid: IntV2,
		unique_name: &str,
		rotation: FourWayDir
	) -> Self {
		Self(LogicDeviceGeneric::new(
			hash_map!{
				"a".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, -1), FourWayDir::W, 1.0),
				"b".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, 1), FourWayDir::W, 1.0),
				"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(3, 0), FourWayDir::E, 1.0),
			},
			position_grid,
			unique_name.to_owned(),
			1,
			rotation,
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0))
		).unwrap())
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
		self.set_pin_internal_state_panic("q", (self.get_pin_state_panic("a").to_bool() && self.get_pin_state_panic("b").to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateAnd(self.clone()))
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
	pub fn new(
		position_grid: IntV2,
		unique_name: &str,
		rotation: FourWayDir
	) -> Self {
		Self(LogicDeviceGeneric::new(
			hash_map!{
				"a".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, -1), FourWayDir::W, 1.0),
				"b".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, 1), FourWayDir::W, 1.0),
				"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(4, 0), FourWayDir::E, 1.0),
			},
			position_grid,
			unique_name.to_owned(),
			1,
			rotation,
			(V2::new(-2.0, -2.0), V2::new(3.0, 2.0))
		).unwrap())
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
		let and: bool = self.get_pin_state_panic("a").to_bool() && self.get_pin_state_panic("b").to_bool();
		self.set_pin_internal_state_panic("q", (!and).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateNand(self.clone()))
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
	pub fn new(
		position_grid: IntV2,
		unique_name: &str,
		rotation: FourWayDir
	) -> Self {
		Self(LogicDeviceGeneric::new(
			hash_map!{
				"a".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, 0), FourWayDir::W, 1.0),
				"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(4, 0), FourWayDir::E, 1.0),
			},
			position_grid,
			unique_name.to_owned(),
			1,
			rotation,
			(V2::new(-2.0, -2.0), V2::new(2.0, 2.0))
		).unwrap())
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
		self.set_pin_internal_state_panic("q", (!self.get_pin_state_panic("a").to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateNot(self.clone()))
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
	pub fn new(
		position_grid: IntV2,
		unique_name: &str,
		rotation: FourWayDir
	) -> Self {
		Self(LogicDeviceGeneric::new(
			hash_map!{
				"a".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, -1), FourWayDir::W, 1.0),
				"b".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, 1), FourWayDir::W, 1.0),
				"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(3, 0), FourWayDir::E, 1.0),
			},
			position_grid,
			unique_name.to_owned(),
			1,
			rotation,
			(V2::new(-2.0, -2.0), V2::new(3.0, 2.0))
		).unwrap())
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
		self.set_pin_internal_state_panic("q", (self.get_pin_state_panic("a").to_bool() || self.get_pin_state_panic("b").to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateOr(self.clone()))
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
	pub fn new(
		position_grid: IntV2,
		unique_name: &str,
		rotation: FourWayDir
	) -> Self {
		Self(LogicDeviceGeneric::new(
			hash_map!{
				"a".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, -1), FourWayDir::W, 1.0),
				"b".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, 1), FourWayDir::W, 1.0),
				"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(4, 0), FourWayDir::E, 1.0),
			},
			position_grid,
			unique_name.to_owned(),
			1,
			rotation,
			(V2::new(-2.0, -2.0), V2::new(3.0, 2.0))
		).unwrap())
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
		self.set_pin_internal_state_panic("q", (!(self.get_pin_state_panic("a").to_bool() || self.get_pin_state_panic("b").to_bool())).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateNor(self.clone()))
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
	pub fn new(
		position_grid: IntV2,
		unique_name: &str,
		rotation: FourWayDir
	) -> Self {
		Self(LogicDeviceGeneric::new(
			hash_map!{
				"a".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, -1), FourWayDir::W, 0.7),
				"b".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, 1), FourWayDir::W, 0.7),
				"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(3, 0), FourWayDir::E, 1.0),
			},
			position_grid,
			unique_name.to_owned(),
			1,
			rotation,
			(V2::new(-2.0, -2.0), V2::new(3.0, 2.0))
		).unwrap())
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
		self.set_pin_internal_state_panic("q", (self.get_pin_state_panic("a").to_bool() != self.get_pin_state_panic("b").to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateXor(self.clone()))
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
	pub fn new(
		position_grid: IntV2,
		unique_name: &str,
		rotation: FourWayDir
	) -> Self {
		Self(LogicDeviceGeneric::new(
			hash_map!{
				"a".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, -1), FourWayDir::W, 0.7),
				"b".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(-3, 1), FourWayDir::W, 0.7),
				"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(4, 0), FourWayDir::E, 1.0),
			},
			position_grid,
			unique_name.to_owned(),
			1,
			rotation,
			(V2::new(-2.0, -2.0), V2::new(3.0, 2.0))
		).unwrap())
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
		self.set_pin_internal_state_panic("q", (self.get_pin_state_panic("a").to_bool() == self.get_pin_state_panic("b").to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::GateXnor(self.clone()))
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
	pub fn new(
		enabled: bool,
		state: bool,
		freq: f32,
		position_grid: IntV2,
		direction: FourWayDir
	) -> Self {
		let mut out = Self {
			generic: LogicDeviceGeneric::new(
				hash_map!{
					"q".to_owned() => LogicConnectionPin::new(Some(LogicConnectionPinInternalSource::ComponentInternal), None, IntV2(0, 0), FourWayDir::W, 1.0),
				},
				position_grid,
				"CLK".to_owned(),
				1,
				direction,
				(V2::new(1.0, -1.0), V2::new(3.0, 1.0))
			).unwrap(),
			enabled,
			freq,
			last_change: Instant::now()
		};
		out.set_pin_internal_state_panic("q", state.into());
		out.generic.ui_data.position = position_grid;
		out.generic.ui_data.direction = direction;
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
			self.set_pin_internal_state_panic("q", (!self.get_pin_state_panic("q").to_bool()).into());
			self.last_change = Instant::now();
		}
	}
	fn save(&self) -> Result<EnumAllLogicDevices, String> {
		Ok(EnumAllLogicDevices::Clock{enabled: self.enabled, state: self.get_pin_state_panic("q").to_bool(), freq: self.freq, position_grid: self.generic.ui_data.position, direction: self.generic.ui_data.direction})
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
			draw.styles.color_from_logic_state(self.get_pin_state_panic("q"))
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
			SelectProperty::ClockState(self.get_pin_state_panic("q").to_bool())
		]
	}
	fn device_set_special_select_property(&mut self, property: SelectProperty) {
		match property {
			SelectProperty::ClockEnabled(enable) => {self.enabled = enable;},
			SelectProperty::ClockFreq(freq) => {self.freq = freq;},
			SelectProperty::ClockState(state) => self.set_pin_internal_state_panic("q", state.into()),
			_ => {}
		}
	}
}