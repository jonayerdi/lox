use std::fmt::Display;

use lox::{
    context::Position,
    result::LoxError,
    scanner::{Scanner, ScannerItem},
    token::{Token, TokenContext, TokenValue},
};

use lazy_static::lazy_static;

fn ok(value: TokenValue, begin: (usize, usize)) -> ScannerItem {
    ScannerItem::Ok(Token {
        value,
        context: TokenContext::new(Position(begin.0, begin.1)),
    })
}

fn err<D: Display>(msg: D, begin: (usize, usize)) -> ScannerItem {
    ScannerItem::Err(LoxError::scan(
        msg,
        TokenContext::new(Position(begin.0, begin.1)),
    ))
}

lazy_static! {
    pub static ref TEST_DATA: Vec<(String, Vec<ScannerItem>)> = vec![
        (format!(""), vec![]),
        (
            format!(";"),
            vec![ok(TokenValue::Semicolon, (1, 1)),]
        ),
        (
            format!("1 +1== 2;"),
            vec![
                ok(TokenValue::Number(format!("1")), (1, 1)),
                ok(TokenValue::Plus, (1, 3)),
                ok(TokenValue::Number(format!("1")), (1, 4)),
                ok(TokenValue::EqualEqual, (1, 5)),
                ok(TokenValue::Number(format!("2")), (1, 8)),
                ok(TokenValue::Semicolon, (1, 9)),
            ]
        ),
        (
            format!("1 * 12.5 / 2 > 2;"),
            vec![
                ok(TokenValue::Number(format!("1")), (1, 1)),
                ok(TokenValue::Star, (1, 3)),
                ok(TokenValue::Number(format!("12.5")), (1, 5)),
                ok(TokenValue::Slash, (1, 10)),
                ok(TokenValue::Number(format!("2")), (1, 12)),
                ok(TokenValue::Greater, (1, 14)),
                ok(TokenValue::Number(format!("2")), (1, 16)),
                ok(TokenValue::Semicolon, (1, 17)),
            ]
        ),
        (
            format!("var language = \"lox\";"),
            vec![
                ok(TokenValue::Var, (1, 1)),
                ok(TokenValue::Identifier(format!("language")), (1, 5)),
                ok(TokenValue::Equal, (1, 14)),
                ok(TokenValue::String(format!("lox")), (1, 16)),
                ok(TokenValue::Semicolon, (1, 21)),
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
                ok(TokenValue::Var, (1, 1)),
                ok(TokenValue::Identifier(format!("breakfast")), (1, 5)),
                ok(TokenValue::Equal, (1, 15)),
                ok(TokenValue::String(format!("bagels")), (1, 17)),
                ok(TokenValue::Semicolon, (1, 25)),
                // Line 2
                ok(TokenValue::Print, (2, 1)),
                ok(TokenValue::Identifier(format!("breakfast")), (2, 7)),
                ok(TokenValue::Semicolon, (2, 16)),
                // Line 3
                ok(TokenValue::Identifier(format!("breakfast")), (3, 1)),
                ok(TokenValue::Equal, (3, 11)),
                ok(TokenValue::String(format!("beignets")), (3, 13)),
                ok(TokenValue::Semicolon, (3, 23)),
                // Line 4
                ok(TokenValue::Print, (4, 1)),
                ok(TokenValue::Identifier(format!("breakfast")), (4, 7)),
                ok(TokenValue::Semicolon, (4, 16)),
            ]
        ),
        (format!("var hello = \"hello;"), vec![
            ok(TokenValue::Var, (1, 1)),
            ok(TokenValue::Identifier(format!("hello")), (1, 5)),
            ok(TokenValue::Equal, (1, 11)),
            err("Reached EOF in match_string before string termination", (1, 13)),
        ]),
        (format!("var _pi = 3.1416;"), vec![
            ok(TokenValue::Var, (1, 1)),
            err("Unexpected character: '_'", (1, 5)),
            ok(TokenValue::Identifier(format!("pi")), (1, 6)),
            ok(TokenValue::Equal, (1, 9)),
            ok(TokenValue::Number("3.1416".parse().unwrap()), (1, 11)),
            ok(TokenValue::Semicolon, (1, 17)),
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
