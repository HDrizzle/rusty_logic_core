use crate::prelude::*;
use serde::{Deserialize, Serialize};
use crate::simulator::AncestryStack;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct GateAnd(LogicDeviceGeneric);

impl GateAnd {
    pub fn new(
        position_grid: IntV2,
        unique_name: &str,
        rotation: FourWayDir
    ) -> Self {
        Self(LogicDeviceGeneric::new(
            vec![
                (LogicConnectionPin::new(IntV2(-2, -1), FourWayDir::W, 1.0), "a"),
                (LogicConnectionPin::new(IntV2(-2, 1), FourWayDir::W, 1.0), "b"),
                (LogicConnectionPin::new(IntV2(2, 0), FourWayDir::E, 1.0), "q"),
            ].into(),
            position_grid,
            unique_name.to_owned(),
            1,
            rotation
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
		self.set_pin_internal_state_panic(&"q".into(), (self.get_pin_state_panic(&"a".into()).to_bool() && self.get_pin_state_panic(&"b".into()).to_bool()).into());
	}
	fn save(&self) -> Result<EnumAllLogicDevicesSave, String> {
		Ok(EnumAllLogicDevicesSave::GateAnd(self.clone()))
	}
}