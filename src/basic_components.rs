use crate::prelude::*;
use serde::{Deserialize, Serialize};
use crate::simulator::{AncestryStack, LogicConnectionPinInternalSource};
use common_macros::hash_map;

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
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String> {
		Ok(EnumAllLogicDevicesSave::GateAnd(self.clone()))
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
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String> {
		Ok(EnumAllLogicDevicesSave::GateNand(self.clone()))
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