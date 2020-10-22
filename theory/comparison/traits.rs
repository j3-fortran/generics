// Compile with:
// rustc traits.rs

trait Stringer {
    fn string(&self) -> &'static str;
}

fn stringify<T : Stringer>(s: Vec<T>) -> String {
    let mut ret = String::new();
    for x in s.iter() {
        ret.push_str(x.string());
    }
    ret
}

struct MyT {
}

impl Stringer for MyT {
    fn string(&self) -> &'static str {
        "X"
    }
}

fn main() {
    let v = vec![MyT{}, MyT{}, MyT{}];
    println!("{}", stringify(v));
}
