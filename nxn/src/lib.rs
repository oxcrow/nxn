//
// NXN Compiler
//

#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::needless_lifetimes)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::let_and_return)]

pub mod lexer;

pub fn bye() {
    const BYE: &str = r#"(ﾉ-ヮ-)ﾉ*:･ﾟ✧"#;
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
