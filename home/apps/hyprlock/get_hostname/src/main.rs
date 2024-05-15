use std::env;
use std::process::Command;

/// Retrieves the hostname of the operating system.
///
/// # Returns
/// A `String` containing the hostname.
fn get_hostname() -> String {
    let output = Command::new("hostname")
        .output()
        .expect("failed to execute process");
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

/// The main entry point of the program.
///
/// This function takes the first command-line argument if provided.
/// It ignores any other arguments passed. If no argument is provided,
/// it uses the hostname of the operating system.
///
/// # Output
/// It converts the input name into a stirng where the first character is
/// an uppercase character and the rest of the characters are lowercase
fn main() {
    // If a name is given at the command line, use it
    let name = match env::args().nth(1) {
        Some(name) => name,
        None => get_hostname(),
    };

    // Convert the name to the desired format
    let mut formatted_name = String::with_capacity(name.len());
    formatted_name.push(name.chars().next().unwrap().to_ascii_uppercase());
    formatted_name.push_str(&name[1..].to_ascii_lowercase());

    println!("{}", formatted_name);
}
