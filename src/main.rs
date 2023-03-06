mod frontend;
mod pastel;

use pastel::pastel;

fn main() {
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
    let text = r"
        *[*, l_white:d_black]Welcome[/]
        *[*, d_white:d_cyan]To[/] 
        *[_, l_white:d_magenta]Pastel![/]
    ";
    println!("{}", pastel(text));
}
