mod pastel_;

use pastel_::parse;

// expose a function to the outside world
#[allow(dead_code)]
pub fn pastel(text: &str) -> String {
    let parts = parse(text);
    "".to_string()
}
