let lexbuf = Lexing.from_channel stdin in 
let program = Parser.program Scanner.token lexbuf in
print_endline (Translate.translate program);
