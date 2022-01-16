open Core
open Out_channel

type value = [
  | `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string
]

let rec output_value _outc value = printf "%s" (value_to_string value)

and value_to_string = function
  | `Assoc obj -> "{ " ^ (assoc_to_string obj) ^ " }"
  | `List l -> "[" ^ (list_to_string l) ^ "]"
  | `String s -> sprintf "\"%s\"" s
  | `Int x -> sprintf "%d" x
  | `Float x -> sprintf "%f" x
  | `Bool true -> sprintf "true"
  | `Bool false -> sprintf "false"
  | `Null -> sprintf "null"

and assoc_to_string obj =
  obj
  |> List.map ~f:(fun (k, v) -> sprintf "\"%s\": %s" k (value_to_string v))
  |> String.concat ~sep:",\n"

and list_to_string lst =
  lst
  |> List.map ~f:(value_to_string)
  |> String.concat ~sep:", "
