{

(*
   written by Oliver Bandel. => Urheber / COpyright Oliver Bandel

   This is free software and it can be redistributed under the
   General Public License (GPL) version 2.
*)

let sbuf = Buffer.create 1024


exception Scanner_error
let string_unquote s = String.sub s 1 (String.length s -2) (* remove first and last char *)
}


rule token = parse
    | ['"'] [^'"']* ['"']  as string { (string_unquote string) }
    | ['a'-'z' 'A'-'Z' '_' '-' '+' '.' '0'-'9' ':' '/' ]+ as word { (word) }
    | [' '  '\t' '\n' ] { token lexbuf }
    | ['-']             { "-" }
    | eof    { raise End_of_file }
    | _ as unknown     { prerr_string "unknown token: "; prerr_char unknown; prerr_newline(); raise Scanner_error }

and empty = parse
    | ['-']+ as word { (word) }
    | [' '  '\t' '\n' ] { empty lexbuf }
    | eof    { raise End_of_file }
    | _ as unknown     { prerr_string "wrong empty: "; prerr_char unknown; prerr_newline(); raise Scanner_error }

and host = parse
    | ['a'-'z' 'A'-'Z' '_' '-' '+' '.' '0'-'9' ':' ]+ as word { (word) }
    | [' '  '\t' '\n' ] { host lexbuf }
    | eof    { raise End_of_file }
    | _ as unknown     { prerr_string "wrong host: "; prerr_char unknown; prerr_newline(); raise Scanner_error }

and date = parse
    | ['['] [^']']* [']']  as string { (string_unquote string) }
    | [' '  '\t' '\n' ] { date lexbuf }
    | eof    { raise End_of_file }
    | _ as unknown     { prerr_string "wrong date: "; prerr_char unknown; prerr_newline(); raise Scanner_error }

and integer = parse
    | ['0'-'9']+ as word { (word) }
    | [' '  '\t' '\n' ]* { integer lexbuf }
    | eof    { raise End_of_file }
    | _ as unknown     { prerr_string "wrong integer: "; prerr_char unknown; prerr_newline(); raise Scanner_error }


and size = parse
    | [' '  '\t' '\n' ]* { size lexbuf }
    | ['0'-'9']+ as word { (word) }
    | '-'                { String.make 1 '-' }
    | eof                { raise End_of_file }
    | _ as unknown       { prerr_string "wrong integer: "; prerr_char unknown; prerr_newline(); raise Scanner_error }



and rest_string = parse
    | [^ '"' '\\']+  as str { Buffer.add_string sbuf str; rest_string lexbuf }
    | "\\\""         as str { Buffer.add_string sbuf str; rest_string lexbuf }
    | ['"']                 { let s = Buffer.contents sbuf in Buffer.clear sbuf; s }
    | _ as ch               { Buffer.add_char sbuf ch; rest_string lexbuf }
    | eof    { raise End_of_file }

and stringval = parse
    | [' '  '\t' '\n' ]* ['"'] { rest_string lexbuf }
    | eof    { raise End_of_file }
    | _ as unknown     { prerr_string "wrong stringval: "; prerr_char unknown; prerr_newline(); raise Scanner_error }




{
 (* nothing *)
}
