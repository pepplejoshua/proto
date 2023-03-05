mod frontend;

use frontend::colors::ColorOption;

fn main() {
    let col = ColorOption::bold_and(Some("l:white".to_string()), Some("l:magenta".to_string()));

    println!(
        "{}",
        col.format_text("Bold white text on magenta background!")
    );

    let underline =
        ColorOption::underline_and(Some("d:black".to_string()), Some("d:white".to_string()));

    println!(
        "\n{}",
        underline.format_text("Underlined black text on white background!")
    );
}
