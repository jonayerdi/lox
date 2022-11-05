use lox::{
    context::Position,
    result::LoxError,
    scanner::{self, ScannerItem},
    token::TokenValue,
};

use lazy_static::lazy_static;

lazy_static! {
    pub static ref TEST_DATA: Vec<(String, Vec<ScannerItem<Position>>)> = vec![
        (format!(""), vec![]),
        (
            format!(";"),
            vec![ScannerItem::Ok((TokenValue::Semicolon, Position(1, 1)).into()),]
        ),
        (
            format!("1 +1== 2;"),
            vec![
                ScannerItem::Ok((TokenValue::Number(format!("1")), Position(1, 1)).into()),
                ScannerItem::Ok((TokenValue::Plus, Position(1, 3)).into()),
                ScannerItem::Ok((TokenValue::Number(format!("1")), Position(1, 4)).into()),
                ScannerItem::Ok((TokenValue::EqualEqual, Position(1, 5)).into()),
                ScannerItem::Ok((TokenValue::Number(format!("2")), Position(1, 8)).into()),
                ScannerItem::Ok((TokenValue::Semicolon, Position(1, 9)).into()),
            ]
        ),
        (
            format!("1 * 12.5 / 2 > 2;"),
            vec![
                ScannerItem::Ok((TokenValue::Number(format!("1")), Position(1, 1)).into()),
                ScannerItem::Ok((TokenValue::Star, Position(1, 3)).into()),
                ScannerItem::Ok((TokenValue::Number(format!("12.5")), Position(1, 5)).into()),
                ScannerItem::Ok((TokenValue::Slash, Position(1, 10)).into()),
                ScannerItem::Ok((TokenValue::Number(format!("2")), Position(1, 12)).into()),
                ScannerItem::Ok((TokenValue::Greater, Position(1, 14)).into()),
                ScannerItem::Ok((TokenValue::Number(format!("2")), Position(1, 16)).into()),
                ScannerItem::Ok((TokenValue::Semicolon, Position(1, 17)).into()),
            ]
        ),
        (
            format!("var language = \"lox\";"),
            vec![
                ScannerItem::Ok((TokenValue::Var, Position(1, 1)).into()),
                ScannerItem::Ok((TokenValue::Identifier(format!("language")), Position(1, 5)).into()),
                ScannerItem::Ok((TokenValue::Equal, Position(1, 14)).into()),
                ScannerItem::Ok((TokenValue::String(format!("lox")), Position(1, 16)).into()),
                ScannerItem::Ok((TokenValue::Semicolon, Position(1, 21)).into()),
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
                ScannerItem::Ok((TokenValue::Var, Position(1, 1)).into()),
                ScannerItem::Ok((TokenValue::Identifier(format!("breakfast")), Position(1, 5)).into()),
                ScannerItem::Ok((TokenValue::Equal, Position(1, 15)).into()),
                ScannerItem::Ok((TokenValue::String(format!("bagels")), Position(1, 17)).into()),
                ScannerItem::Ok((TokenValue::Semicolon, Position(1, 25)).into()),
                // Line 2
                ScannerItem::Ok((TokenValue::Print, Position(2, 1)).into()),
                ScannerItem::Ok((TokenValue::Identifier(format!("breakfast")), Position(2, 7)).into()),
                ScannerItem::Ok((TokenValue::Semicolon, Position(2, 16)).into()),
                // Line 3
                ScannerItem::Ok((TokenValue::Identifier(format!("breakfast")), Position(3, 1)).into()),
                ScannerItem::Ok((TokenValue::Equal, Position(3, 11)).into()),
                ScannerItem::Ok((TokenValue::String(format!("beignets")), Position(3, 13)).into()),
                ScannerItem::Ok((TokenValue::Semicolon, Position(3, 23)).into()),
                // Line 4
                ScannerItem::Ok((TokenValue::Print, Position(4, 1)).into()),
                ScannerItem::Ok((TokenValue::Identifier(format!("breakfast")), Position(4, 7)).into()),
                ScannerItem::Ok((TokenValue::Semicolon, Position(4, 16)).into()),
            ]
        ),
        (format!("var hello = \"hello;"), vec![
            ScannerItem::Ok((TokenValue::Var, Position(1, 1)).into()),
            ScannerItem::Ok((TokenValue::Identifier(format!("hello")), Position(1, 5)).into()),
            ScannerItem::Ok((TokenValue::Equal, Position(1, 11)).into()),
            ScannerItem::Err(LoxError::scan("Reached EOF in match_string before string termination", Position(1, 13))),
        ]),
        (format!("var _pi = 3.1416;"), vec![
            ScannerItem::Ok((TokenValue::Var, Position(1, 1)).into()),
            ScannerItem::Err(LoxError::scan("Unexpected character: '_'", Position(1, 5))),
            ScannerItem::Ok((TokenValue::Identifier(format!("pi")), Position(1, 6)).into()),
            ScannerItem::Ok((TokenValue::Equal, Position(1, 9)).into()),
            ScannerItem::Ok((TokenValue::Number("3.1416".parse().unwrap()), Position(1, 11)).into()),
            ScannerItem::Ok((TokenValue::Semicolon, Position(1, 17)).into()),
        ]),
    ];
}

#[test]
fn test() {
    for (code, expected_items) in TEST_DATA.iter() {
        let scanned_items = scanner::with_positions(code.chars()).collect::<Vec<_>>();
        assert_eq!(
            &scanned_items, expected_items,
            "Unexpected scanner results for the code:\n{code}"
        );
    }
}
