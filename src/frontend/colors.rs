use phf::phf_map;

#[allow(dead_code)]
#[derive(Debug, Clone, Default)]
pub struct Color {
    pub name: &'static str,
    pub dark_foreground: &'static str,
    pub dark_background: &'static str,
    pub light_foreground: &'static str,
    pub light_background: &'static str,
}

const _BLACK: Color = Color {
    name: "black",
    dark_foreground: "\x1b[30m",
    dark_background: "\x1b[40m",
    light_foreground: "\x1b[90m",
    light_background: "\x1b[100m",
};

const _RED: Color = Color {
    name: "red",
    dark_foreground: "\x1b[31m",
    dark_background: "\x1b[41m",
    light_foreground: "\x1b[91m",
    light_background: "\x1b[101m",
};

const _GREEN: Color = Color {
    name: "green",
    dark_foreground: "\x1b[32m",
    dark_background: "\x1b[42m",
    light_foreground: "\x1b[92m",
    light_background: "\x1b[102m",
};

const _YELLOW: Color = Color {
    name: "yellow",
    dark_foreground: "\x1b[33m",
    dark_background: "\x1b[43m",
    light_foreground: "\x1b[93m",
    light_background: "\x1b[103m",
};

const _BLUE: Color = Color {
    name: "blue",
    dark_foreground: "\x1b[34m",
    dark_background: "\x1b[44m",
    light_foreground: "\x1b[94m",
    light_background: "\x1b[104m",
};

const _MAGENTA: Color = Color {
    name: "magenta",
    dark_foreground: "\x1b[35m",
    dark_background: "\x1b[45m",
    light_foreground: "\x1b[95m",
    light_background: "\x1b[105m",
};

const _CYAN: Color = Color {
    name: "cyan",
    dark_foreground: "\x1b[36m",
    dark_background: "\x1b[46m",
    light_foreground: "\x1b[96m",
    light_background: "\x1b[106m",
};

const _WHITE: Color = Color {
    name: "white",
    dark_foreground: "\x1b[37m",
    dark_background: "\x1b[47m",
    light_foreground: "\x1b[97m",
    light_background: "\x1b[107m",
};

const _RESET: &str = "\x1b[0m";
const _BOLD: &str = "\x1b[1m";
const _UNDERLINE: &str = "\x1b[4m";

// color hash map with key being color name and value being a tuple of foreground and background
pub const _COLORS: phf::Map<&str, Color> = phf_map!(
    "black" => _BLACK,
    "red" => _RED,
    "green" => _GREEN,
    "yellow" => _YELLOW,
    "blue" => _BLUE,
    "magenta" => _MAGENTA,
    "cyan" => _CYAN,
    "white" => _WHITE,
);

#[allow(dead_code)]
#[derive(Debug, Clone, Default)]
pub struct ColorOption {
    pub bold: bool,
    pub underline: bool,
    pub fore_color: Option<String>,
    pub back_color: Option<String>,
}

#[allow(dead_code)]
impl ColorOption {
    pub fn reg() -> ColorOption {
        ColorOption {
            bold: false,
            underline: false,
            fore_color: None,
            back_color: None,
        }
    }

    pub fn bold_and(fore: Option<String>, back: Option<String>) -> ColorOption {
        ColorOption {
            bold: true,
            underline: false,
            fore_color: fore,
            back_color: back,
        }
    }

    pub fn bold_underline_and(fore: Option<String>, back: Option<String>) -> ColorOption {
        ColorOption {
            bold: true,
            underline: true,
            fore_color: fore,
            back_color: back,
        }
    }

    pub fn underline_and(fore: Option<String>, back: Option<String>) -> ColorOption {
        ColorOption {
            bold: false,
            underline: true,
            fore_color: fore,
            back_color: back,
        }
    }

    pub fn format_text(&self, text: &str) -> String {
        let mut result = String::new();
        if self.bold {
            result.push_str(_BOLD);
        }
        if self.underline {
            result.push_str(_UNDERLINE);
        }

        // fore_color and back_color will come in the form of:
        // "type_of_color:color_name", where:
        // type_of_color = "l" or "d" for light or dark
        // color_name = "black", "red", "green", "yellow", "blue", "magenta", "cyan", or "white"
        if let Some(fore_color) = &self.fore_color {
            let split_res = fore_color.split(':').collect::<Vec<&str>>();
            let fore_color = _COLORS.get(split_res[1]).unwrap();
            if split_res[0] == "l" {
                result.push_str(fore_color.light_foreground);
            } else {
                result.push_str(fore_color.dark_foreground);
            }
        }

        if let Some(back_color) = &self.back_color {
            let back_color = back_color.split(':').collect::<Vec<&str>>();
            let back_color = _COLORS.get(back_color[1]).unwrap();
            if back_color.name == "l" {
                result.push_str(back_color.light_background);
            } else {
                result.push_str(back_color.dark_background);
            }
        }

        result.push_str(text);
        result.push_str(_RESET);
        result
    }
}
