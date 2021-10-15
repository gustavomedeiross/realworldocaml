open Base
open Stdio

let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row ->
        List.map2_exn ~f:Int.max acc (lengths row))

let render_separator widths =
  let pieces = List.map widths 
    ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ (String.concat ~sep: "+" pieces) ^ "|"

let pad str length =
  " " ^ str ^ (String.make (length - String.length str + 1) ' ')

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ (String.concat ~sep:"|" padded) ^ "|"

let render_table header rows =
  let widths = max_widths header rows in
  let header = render_row header widths in
  let separators = render_separator widths in
  let rows = List.map rows ~f:(fun r -> render_row r widths) in
  String.concat ~sep:"\n" (header :: separators :: rows)

let () =
  Stdio.print_endline
    (render_table
      ["language";"architect";"first release"] [
        ["Lisp" ;"John McCarthy" ;"1958"];
        ["C"    ;"Dennis Ritchie";"1969"];
        ["ML"   ;"Robin Milner"  ;"1973"];
        ["OCaml";"Xavier Leroy"  ;"1996"];
    ])
