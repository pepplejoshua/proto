// This is a sample for user defined structs

// simple struct that uses defaults
struct SourceFile {
    path str,
    text str = "",
    flat_index usize = 0,
    col usize = 0,
    line usize = 0,
}

// regular function
fn jump_to(s SourceFile, line usize, col usize) void {
    s.line = line;
    s.col = col;
}

fn main() void {
    // since we allow defaults, we can focus on the
    // members we care about initializing
    mut src = SourceFile { path = "./using_constructor.pr" };

    // regular function call
    jump_to(src, 20, 15);

    // faux method call, as it is semantically
    // the same as the call above
    src.jump_to(0, 0);
}
