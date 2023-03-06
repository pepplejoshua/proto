mod pastel_;

use pastel_::parse;

// expose a function to the outside world
#[allow(dead_code)]
pub fn pastel(text: &str) -> String {
    let parts = parse(text);

    let mut result = String::new();
    for part in parts {
        match part {
            // if part is a RegText, then just push it to the result
            pastel_::SliceOf::RegText(text) => result.push_str(&text),
            // if part is an AugText, then push the formatted text to the result
            pastel_::SliceOf::AugText(text, option) => {
                result.push_str(&option.format_text(&text));
            }
        }
    }
    result
}
