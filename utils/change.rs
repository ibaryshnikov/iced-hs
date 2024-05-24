//
// change from
// A,
// B,
// ...
// to
// A => 0,
// B => 1,
// ...
//
// used to rewrite large enums such as Named key or KeyCode
// in match statements such as
// match code {
//   ...
//   KeyA => 19,
//   KeyB => 20,
//   KeyC => 21,
//   KeyD => 22,
//   KeyE => 23,
//   ...
// }
//

use std::fs;

fn main() {
	let contents = fs::read_to_string("in_file.rs").unwrap();
	let (i, results) = contents.split(',')
        .fold((0, String::new()), |(i, acc), item| (i + 1, format!("{acc}{item} => {i},")));
	println!("Processed {i} items");
	fs::write("out_file.rs", results).unwrap();
}
