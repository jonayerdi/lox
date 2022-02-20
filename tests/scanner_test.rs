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
            vec![ScannerItem::Token(Token::Semicolon, Position(1, 1)),]
        ),
        (
            format!("1 +1== 2;"),
            vec![
                ScannerItem::Token(Token::Number(1.0), Position(1, 1)),
                ScannerItem::Token(Token::Plus, Position(1, 3)),
                ScannerItem::Token(Token::Number(1.0), Position(1, 4)),
                ScannerItem::Token(Token::EqualEqual, Position(1, 5)),
                ScannerItem::Token(Token::Number(2.0), Position(1, 8)),
                ScannerItem::Token(Token::Semicolon, Position(1, 9)),
            ]
        ),
        (
            format!("var language = \"lox\";"),
            vec![
                ScannerItem::Token(Token::Var, Position(1, 1)),
                ScannerItem::Token(Token::Identifier(format!("language")), Position(1, 5)),
                ScannerItem::Token(Token::Equal, Position(1, 14)),
                ScannerItem::Token(Token::String(format!("lox")), Position(1, 16)),
                ScannerItem::Token(Token::Semicolon, Position(1, 21)),
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
                ScannerItem::Token(Token::Var, Position(1, 1)),
                ScannerItem::Token(Token::Identifier(format!("breakfast")), Position(1, 5)),
                ScannerItem::Token(Token::Equal, Position(1, 15)),
                ScannerItem::Token(Token::String(format!("bagels")), Position(1, 17)),
                ScannerItem::Token(Token::Semicolon, Position(1, 25)),
                // Line 2
                ScannerItem::Token(Token::Print, Position(2, 1)),
                ScannerItem::Token(Token::Identifier(format!("breakfast")), Position(2, 7)),
                ScannerItem::Token(Token::Semicolon, Position(2, 16)),
                // Line 3
                ScannerItem::Token(Token::Identifier(format!("breakfast")), Position(3, 1)),
                ScannerItem::Token(Token::Equal, Position(3, 11)),
                ScannerItem::Token(Token::String(format!("beignets")), Position(3, 13)),
                ScannerItem::Token(Token::Semicolon, Position(3, 23)),
                // Line 4
                ScannerItem::Token(Token::Print, Position(4, 1)),
                ScannerItem::Token(Token::Identifier(format!("breakfast")), Position(4, 7)),
                ScannerItem::Token(Token::Semicolon, Position(4, 16)),
            ]
        ),
        (format!("var hello = \"hello;"), vec![
            ScannerItem::Token(Token::Var, Position(1, 1)),
            ScannerItem::Token(Token::Identifier(format!("hello")), Position(1, 5)),
            ScannerItem::Token(Token::Equal, Position(1, 11)),
            ScannerItem::Error(LoxError::scan("Reached EOF in match_string before string termination", Position(1, 13))),
        ]),
        (format!("var _pi = 3.1416;"), vec![
            ScannerItem::Token(Token::Var, Position(1, 1)),
            ScannerItem::Error(LoxError::scan("Unexpected character: '_'", Position(1, 5))),
            ScannerItem::Token(Token::Identifier(format!("pi")), Position(1, 6)),
            ScannerItem::Token(Token::Equal, Position(1, 9)),
            ScannerItem::Token(Token::Number("3.1416".parse().unwrap()), Position(1, 11)),
            ScannerItem::Token(Token::Semicolon, Position(1, 17)),
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
