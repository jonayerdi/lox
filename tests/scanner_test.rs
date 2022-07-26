use lox::{
    position::Position,
    result::LoxError,
    scanner::{Scanner, ScannerItem},
    token::Token,
};

use lazy_static::lazy_static;

lazy_static! {
    pub static ref TEST_DATA: Vec<(String, Vec<ScannerItem>)> = vec![
        (format!(""), vec![]),
        (
            format!(";"),
            vec![ScannerItem::Ok((Token::Semicolon, Position(1, 1)).into()),]
        ),
        (
            format!("1 +1== 2;"),
            vec![
                ScannerItem::Ok((Token::Number(format!("1")), Position(1, 1)).into()),
                ScannerItem::Ok((Token::Plus, Position(1, 3)).into()),
                ScannerItem::Ok((Token::Number(format!("1")), Position(1, 4)).into()),
                ScannerItem::Ok((Token::EqualEqual, Position(1, 5)).into()),
                ScannerItem::Ok((Token::Number(format!("2")), Position(1, 8)).into()),
                ScannerItem::Ok((Token::Semicolon, Position(1, 9)).into()),
            ]
        ),
        (
            format!("1 * 12.5 / 2 > 2;"),
            vec![
                ScannerItem::Ok((Token::Number(format!("1")), Position(1, 1)).into()),
                ScannerItem::Ok((Token::Star, Position(1, 3)).into()),
                ScannerItem::Ok((Token::Number(format!("12.5")), Position(1, 5)).into()),
                ScannerItem::Ok((Token::Slash, Position(1, 10)).into()),
                ScannerItem::Ok((Token::Number(format!("2")), Position(1, 12)).into()),
                ScannerItem::Ok((Token::Greater, Position(1, 14)).into()),
                ScannerItem::Ok((Token::Number(format!("2")), Position(1, 16)).into()),
                ScannerItem::Ok((Token::Semicolon, Position(1, 17)).into()),
            ]
        ),
        (
            format!("var language = \"lox\";"),
            vec![
                ScannerItem::Ok((Token::Var, Position(1, 1)).into()),
                ScannerItem::Ok((Token::Identifier(format!("language")), Position(1, 5)).into()),
                ScannerItem::Ok((Token::Equal, Position(1, 14)).into()),
                ScannerItem::Ok((Token::String(format!("lox")), Position(1, 16)).into()),
                ScannerItem::Ok((Token::Semicolon, Position(1, 21)).into()),
            ]
        ),
        (
            [
                "var breakfast = \"bagels\";",
                "print breakfast; // \"bagels\".",
                "breakfast = \"beignets\";",
                "print breakfast; // \"beignets\".",
            ].join("\n"),
            vec![
                // Line 1
                ScannerItem::Ok((Token::Var, Position(1, 1)).into()),
                ScannerItem::Ok((Token::Identifier(format!("breakfast")), Position(1, 5)).into()),
                ScannerItem::Ok((Token::Equal, Position(1, 15)).into()),
                ScannerItem::Ok((Token::String(format!("bagels")), Position(1, 17)).into()),
                ScannerItem::Ok((Token::Semicolon, Position(1, 25)).into()),
                // Line 2
                ScannerItem::Ok((Token::Print, Position(2, 1)).into()),
                ScannerItem::Ok((Token::Identifier(format!("breakfast")), Position(2, 7)).into()),
                ScannerItem::Ok((Token::Semicolon, Position(2, 16)).into()),
                // Line 3
                ScannerItem::Ok((Token::Identifier(format!("breakfast")), Position(3, 1)).into()),
                ScannerItem::Ok((Token::Equal, Position(3, 11)).into()),
                ScannerItem::Ok((Token::String(format!("beignets")), Position(3, 13)).into()),
                ScannerItem::Ok((Token::Semicolon, Position(3, 23)).into()),
                // Line 4
                ScannerItem::Ok((Token::Print, Position(4, 1)).into()),
                ScannerItem::Ok((Token::Identifier(format!("breakfast")), Position(4, 7)).into()),
                ScannerItem::Ok((Token::Semicolon, Position(4, 16)).into()),
            ]
        ),
        (format!("var hello = \"hello;"), vec![
            ScannerItem::Ok((Token::Var, Position(1, 1)).into()),
            ScannerItem::Ok((Token::Identifier(format!("hello")), Position(1, 5)).into()),
            ScannerItem::Ok((Token::Equal, Position(1, 11)).into()),
            ScannerItem::Err(LoxError::scan("Reached EOF in match_string before string termination", Position(1, 13))),
        ]),
        (format!("var _pi = 3.1416;"), vec![
            ScannerItem::Ok((Token::Var, Position(1, 1)).into()),
            ScannerItem::Err(LoxError::scan("Unexpected character: '_'", Position(1, 5))),
            ScannerItem::Ok((Token::Identifier(format!("pi")), Position(1, 6)).into()),
            ScannerItem::Ok((Token::Equal, Position(1, 9)).into()),
            ScannerItem::Ok((Token::Number("3.1416".parse().unwrap()), Position(1, 11)).into()),
            ScannerItem::Ok((Token::Semicolon, Position(1, 17)).into()),
        ]),
    ];
}

#[test]
fn test() {
    for (code, expected_items) in TEST_DATA.iter() {
        let scanned_items = Scanner::new(code.chars()).collect::<Vec<_>>();
        assert_eq!(
            &scanned_items, expected_items,
            "Unexpected scanner results for the code:\n{code}"
        );
    }
}
