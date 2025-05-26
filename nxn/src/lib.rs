//
// NXN Compiler
//

const BYE: &str = r#"(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧"#;

pub fn bye() {
    println!("{}", BYE);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dev() {
        bye();
    }
}
