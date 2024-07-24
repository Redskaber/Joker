//! This is idea for great impl scanner.
//!
//! - automata:
//!     - finite automaton（有限自动机）：
//!         有限自动机是一种数学模型，它是一种描述状态转移的自动机。
//!         通过事件驱动的处理，自动机根据输入的符号序列，按照一定的规则，从一个状态转移到另一个状态，直到接受或拒绝输入序列。
//!         
//!         -- DFA（Deterministic Finite Automaton） 确定性有限自动机
//!         -- NFA（Non-Deterministic Finite Automaton） 非确定性有限自动机
//!         -- EFA（Epsilon-NFA） 空转有限自动机
//!         
//!         - Status Move: 状态转移
//!             - DFA: 确定性的状态转移，即每个状态只对应唯一的下一状态
//!             - NFA: 非确定性的状态转移，即每个状态可以对应多个下一状态
//!             - EFA: 空转有限自动机，即状态转移可以为空
//!
//! - finite automaton
//! - wireless automaton

// finite automaton

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum StrStatus {
    Start,
    Zero,
    Match,
}

fn zero_one_finite_automaton_has_01(source: &str) -> StrStatus {
    let mut status: StrStatus = StrStatus::Start;
    let chars: std::str::Chars = source.chars();
    for ch in chars {
        match (status, ch) {
            (StrStatus::Start, '0') => status = StrStatus::Zero,
            (StrStatus::Zero, '1') => status = StrStatus::Match,
            (StrStatus::Start, '1') | (StrStatus::Zero, _) => {
                status = StrStatus::Start;
            }
            _ => {}
        }
        if status == StrStatus::Match {
            break;
        }
    }
    status
}

fn simple_string_main() {
    let match_source: &str = "1010101001";
    let no_match_source: &str = "1111110";
    let status_match: StrStatus = zero_one_finite_automaton_has_01(match_source);
    let status_no_match: StrStatus = zero_one_finite_automaton_has_01(no_match_source);
    println!("Match Source Status: {:#?}", status_match);
    println!("No Match Source Status: {:#?}", status_no_match);
}

mod finite_automaton {}

pub fn automata_main() {
    // simple_string_main();
}
