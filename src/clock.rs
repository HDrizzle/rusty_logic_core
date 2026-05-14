//! Circuit's clock

use std::{default::Default, fmt::Debug};
use web_time::{Duration, Instant};

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
		let last_change = Instant::now();
		Self {
			enabled,
			freq,
			last_change,
			state
		}
	}
	/// If the clock is able to update, returns Some(new last_change time)
	pub fn would_update(&self) -> Option<Instant> {
		if self.enabled {
			if self.freq == 0.0 {
				Some(Instant::now())
			}
			else if self.last_change.elapsed() > Duration::from_secs_f32(0.5 / self.freq) {// The frequency is based on a whole period, it must change twice per period, so 0.5/f not 1/f
				Some(Instant::now())
			}
			else {
				None
			}
		}
		else {
			None
		}
	}
	/// Returns: Whether it changed
	pub fn update(&mut self) -> bool {
		if let Some(new_last_change) = self.would_update() {
			self.last_change = new_last_change;
			self.state = !self.state;
			true
		}
		else {
			false
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