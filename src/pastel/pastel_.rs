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
#[derive(serde::Deserialize, serde::Serialize, Debug, Default)]
pub struct PastelOption {
    pub bold: bool,
    pub underline: bool,
    pub fore_color: Option<String>,
    pub back_color: Option<String>,
}

#[allow(dead_code)]
impl PastelOption {
    pub fn reg() -> Self {
        PastelOption {
            bold: false,
            underline: false,
            fore_color: None,
            back_color: None,
        }
    }

    pub fn bold_and(fore: Option<String>, back: Option<String>) -> Self {
        PastelOption {
            bold: true,
            underline: false,
            fore_color: fore,
            back_color: back,
        }
    }

    pub fn bold_underline_and(fore: Option<String>, back: Option<String>) -> Self {
        PastelOption {
            bold: true,
            underline: true,
            fore_color: fore,
            back_color: back,
        }
    }

    pub fn underline_and(fore: Option<String>, back: Option<String>) -> Self {
        PastelOption {
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

        let mut do_reset = false;

        // fore_color and back_color will come in the form of:
        // "type_of_color:color_name", where:
        // type_of_color = "l" or "d" for light or dark
        // color_name = "black", "red", "green", "yellow", "blue", "magenta", "cyan", or "white"
        if let Some(fore_color) = &self.fore_color {
            let split_res = fore_color.split('_').collect::<Vec<&str>>();
            let fore_color = _COLORS.get(split_res[1]).unwrap();
            if split_res[0] == "l" {
                result.push_str(fore_color.light_foreground);
            } else {
                result.push_str(fore_color.dark_foreground);
            }
            do_reset = true;
        }

        if let Some(back_color) = &self.back_color {
            let back_color = back_color.split('_').collect::<Vec<&str>>();
            let back_color = _COLORS.get(back_color[1]).unwrap();
            if back_color.name == "l" {
                result.push_str(back_color.light_background);
            } else {
                result.push_str(back_color.dark_background);
            }
            do_reset = true;
        }

        result.push_str(text);

        if do_reset {
            result.push_str(_RESET);
        }
        result
    }
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug)]
pub enum SliceOf {
    RegText(String),
    AugText(String, PastelOption),
}

#[allow(dead_code)]
impl SliceOf {
    fn new(text: String, option: Option<PastelOption>) -> Self {
        match option {
            Some(opt) => SliceOf::AugText(text, opt),
            None => SliceOf::RegText(text),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
struct Pastel {
    pub text: String,
    pub slices: Vec<SliceOf>,
}

#[allow(dead_code)]
impl Pastel {
    pub fn new(text: String) -> Self {
        Pastel {
            text,
            slices: Vec::new(),
        }
    }

    fn add_slice(&mut self, slice: SliceOf) {
        self.slices.push(slice);
    }

    pub fn format_text(&self) -> String {
        let mut formatted_text = String::new();

        for slice in &self.slices {
            match slice {
                SliceOf::RegText(text) => formatted_text.push_str(text),
                SliceOf::AugText(text, option) => {
                    formatted_text.push_str(&option.format_text(text));
                }
            }
        }
        formatted_text
    }
}

// a pastel directive is a portion of the string that can be described as:
// *[option, option, ...]regular text[]*
// where:
// *[ = starts the first half of the directive
// ] = ends the first half of the directive
// [ = starts the second half of the directive
// ]* = ends the second half of the directive
// if there is no second half, it will not be included as a valid directive
fn find_split_spans(text: &String) -> Vec<(usize, (usize, usize), usize)> {
    let mut splits: Vec<(usize, (usize, usize), usize)> = Vec::new();
    let mut in_directive = false;

    let mut i = 0;
    let mut true_start = 0;
    let mut start: Option<(usize, usize)> = None;
    loop {
        if i >= text.len() {
            break;
        }
        let cur = text.chars().nth(i).unwrap();
        if cur == '*' && !in_directive {
            // might be the start of a directive
            // look for the next character to see if it is a '['
            match text.chars().nth(i + 1) {
                Some(c) => {
                    if c == '[' {
                        let potential_true_start = i;
                        let potential_options_start = i + 2;
                        i += 2;
                        loop {
                            let cur = text.chars().nth(i);
                            if let Some(chr) = cur {
                                if chr == ']' {
                                    true_start = potential_true_start;
                                    let options_start = potential_options_start;
                                    let options_end = i;
                                    start = Some((options_start, options_end));
                                    in_directive = true;
                                    break;
                                }
                                i += 1;
                            }
                        }
                    }
                }
                None => break, // end of string, no more directives
            };
            continue;
        } else if cur == '[' && in_directive {
            // might be the start of the second half of the directive
            // look for the next character to see if it is a ']'
            match text.chars().nth(i + 1) {
                Some(c) => {
                    if c == '/' {
                        // update i and look to see if the next character is a '*'
                        i += 2;
                        let cur = text.chars().nth(i);
                        if let Some(chr) = cur {
                            if chr == ']' {
                                let directive_end = i + 1; // just past the '*';
                                splits.push((true_start, start.unwrap(), directive_end));
                                in_directive = false;
                            }
                            i += 1;
                        }
                    }
                }
                None => break, // end of string, no more directives
            };
        }
        i += 1;
    }
    splits
}

#[test]
fn test_find_split_spans() {
    let text = "Some Random Text\n*[*, l_white:d_magenta]This is a test of Pastel's parser.[/]";
    insta::assert_yaml_snapshot!(find_split_spans(&text.to_string()), @r###"
    ---
    - - 17
      - - 19
        - 39
      - 77
    "###);

    let text = "*[*]Bolden and *[_]underline[/][/]";
    insta::assert_yaml_snapshot!(find_split_spans(&text.to_string()), @r###"
    ---
    - - 0
      - - 2
        - 3
      - 31
    "###);
}

pub fn parse(text: &str) -> Vec<SliceOf> {
    let content = text.to_string();

    // find all locations of pastel directives in text
    let locs = find_split_spans(&content);

    let cur_start = 0;

    // if there is any regular text before the first directive, add it to the slices
    let mut slices: Vec<SliceOf> = Vec::new();
    if !locs.is_empty() {
        let (true_start, _, _) = locs[0];
        if true_start > 0 {
            let reg_text = content[cur_start..true_start].to_string();
            slices.push(SliceOf::new(reg_text, None));
        }
    } else {
        // there are no directives, so just return the text
        slices.push(SliceOf::new(content, None));
        return slices;
    }

    // create a vector of slices of the text
    // TODO: Rewrite loop to use range instead of iteration
    // this will allow me avoid searching for the next directive
    for direc in &locs {
        let (true_start, (options_start, options_end), directive_end) = direc;
        let options_span = content[*options_start..*options_end].to_string();

        let options = options_span.split(',').collect::<Vec<&str>>();

        // go through options and determine what they are
        // then build up a PastelOption struct
        let mut pastel_option = PastelOption::default();
        for option in options {
            let option = option.trim();

            match option {
                "*" => pastel_option.bold = true,
                "_" => pastel_option.underline = true,
                _ if option.starts_with("l_") || option.starts_with("d_") => {
                    // it is a color option
                    // it could specified as shade_foregroundcolor:shade_backgroundcolor
                    // or just shade_foreground
                    // where shade is one of the following: l or d
                    // and color is one of the following: black, red, green, yellow, blue, magenta, cyan, white
                    let mut split = option.split(':');
                    let foreground = Some(split.next().unwrap().to_string());
                    let background = split.next();
                    let background = background.map(|b| Some(b.to_string())).unwrap_or(None);
                    pastel_option.fore_color = foreground;
                    pastel_option.back_color = background;
                }
                _ => {
                    // it is an invalid option
                    panic!("Invalid option in directive: {true_start}");
                }
            }
        }
        // add the slice to the slices vector
        // the slice is the text between the 1 character after the options end and
        // 2 characters before the directive end
        let reg_text = content[*options_end + 1..*directive_end - 3].to_string();
        slices.push(SliceOf::new(reg_text, Some(pastel_option)));

        // if there is any regular text after this directive, add it to the slices
        let next_directive = locs.iter().find(|d| d.0 > *true_start);
        if let Some((next_true_start, _, _)) = next_directive {
            let reg_text = content[*directive_end..*next_true_start].to_string();
            slices.push(SliceOf::new(reg_text, None));
        } else {
            // there are no more directives, so add the rest of the text
            let reg_text = content[*directive_end..].to_string();
            slices.push(SliceOf::new(reg_text, None));
        }
    }
    slices
}

#[test]
fn test_pastel_parse() {
    let text = "Some Random Text\n*[*, l_white:d_magenta]This is a test of Pastel's parser.[/]";
    insta::assert_yaml_snapshot!(parse(text), @r###"
    ---
    - RegText: "Some Random Text\n"
    - AugText:
        - "This is a test of Pastel's parser."
        - bold: true
          underline: false
          fore_color: l_white
          back_color: d_magenta
    - RegText: ""
    "###);

    let text = "Some Random Text\n*[*, l_white:d_magenta]Magenta![/]\nAnother line\n*[*, _, l_white:d_red]Red![/]";
    insta::assert_yaml_snapshot!(parse(text), @r###"
    ---
    - RegText: "Some Random Text\n"
    - AugText:
        - Magenta!
        - bold: true
          underline: false
          fore_color: l_white
          back_color: d_magenta
    - RegText: "\nAnother line\n"
    - AugText:
        - Red!
        - bold: true
          underline: true
          fore_color: l_white
          back_color: d_red
    - RegText: ""
    "###);

    let text = "*[*]Bolden and *[_]underline[/][/]";
    insta::assert_yaml_snapshot!(parse(text), @r###"
    ---
    - AugText:
        - "Bolden and *[_]underline"
        - bold: true
          underline: false
          fore_color: ~
          back_color: ~
    - RegText: "[/]"
    "###);
}

// TODO:
// - add support for italics: *[~]italics[/], italics ansi code: \x1b[3m
// - add support for strikethrough: *[-]strikethrough[/], strikethrough ansi code: \x1b[9m

/*
This example is akin to writing the following in html+css:
    <div style="font-weight: bold; color: white; background-color: black;">
        Welcome
    </div>
    <div style="font-weight: bold; color: white; background-color: cyan;">
        To
    </div>
    <u style="color: white; background-color: magenta;">
        Pastel!
    </u>
*/
// let text = r"
//     *[*, l_white:d_black]Welcome[/]
//     *[*, d_white:d_cyan]To[/]
//     *[_, l_white:d_magenta]Pastel![/]
// ";
// println!("{}", pastel(text));
