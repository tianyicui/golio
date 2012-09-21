let parse str =
    Parser.parse Lexer.tokens (Lexing.from_string str)
;;
