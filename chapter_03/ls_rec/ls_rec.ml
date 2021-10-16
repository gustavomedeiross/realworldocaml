open Core
open Stdio

let rec ls_rec str =
  if Sys.is_file_exn ~follow_symlinks:true str
  then [str]
  else
    Sys.ls_dir str
    |> List.concat_map ~f:(fun sub -> ls_rec (Filename.concat str sub))

let () = 
  In_channel.input_line In_channel.stdin
  |> Option.filter ~f:(fun s -> not (String.is_empty s))
  |> Option.value ~default:(".")
  |> ls_rec
  |> String.concat ~sep:"\n"
  |> Out_channel.print_endline
