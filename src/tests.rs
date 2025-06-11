#[test]
fn logic_state_merge() {
    use crate::simulator::{merge_logic_states, LogicState};
    // Floating vs ?
    assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Floating), LogicState::Floating);
    assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Contested), LogicState::Contested);
    assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Floating), LogicState::Contested);
    assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Driven(true)), LogicState::Driven(true));
    assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Floating), LogicState::Driven(true));
    assert_eq!(merge_logic_states(LogicState::Floating, LogicState::Driven(false)), LogicState::Driven(false));
    assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Floating), LogicState::Driven(false));
    // Contested vs ?
    assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Driven(false)), LogicState::Contested);
    assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Contested), LogicState::Contested);
    assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Driven(true)), LogicState::Contested);
    assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Contested), LogicState::Contested);
    assert_eq!(merge_logic_states(LogicState::Contested, LogicState::Contested), LogicState::Contested);
    // Creating contention
    assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Driven(true)), LogicState::Contested);
    assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Driven(false)), LogicState::Contested);
    // Successful merging
    assert_eq!(merge_logic_states(LogicState::Driven(false), LogicState::Driven(false)), LogicState::Driven(false));
    assert_eq!(merge_logic_states(LogicState::Driven(true), LogicState::Driven(true)), LogicState::Driven(true));
}