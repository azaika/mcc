#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub grammer);

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
