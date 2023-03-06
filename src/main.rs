mod frontend;
mod pastel;

use pastel::pastel;

fn main() {
    let text = r"
        *[*, l_white:d_black]Welcome[/]
        *[*, d_white:d_cyan]To[/] 
        *[_, l_white:d_magenta]Pastel![/]
    ";
    println!("{}", pastel(text));
}
