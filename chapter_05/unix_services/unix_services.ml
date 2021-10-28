open Core
open Stdio

type service_info = { service_name : string; port : int; protocol : string }

let unix_service_pat = Re.Posix.compile_pat "([a-zA-Z0-9_.-]+)[ \t]+([0-9]+)/([a-zA-Z]+)"

let service_info_of_string line =
  let matches = Re.exec unix_service_pat line in
  {
    service_name = Re.Group.get matches 1;
    port = Int.of_string (Re.Group.get matches 2);
    protocol = Re.Group.get matches 3;
  }

let service_info_to_string { service_name; port; protocol } =
  sprintf "%s %i %s" service_name port protocol

let () =
  In_channel.read_all "/etc/services"
  |> String.split ~on:'\n'
  |> List.filter ~f:(fun line -> Re.execp unix_service_pat line)
  |> List.map ~f:service_info_of_string
  |> List.map ~f:service_info_to_string
  |> String.concat ~sep:"\n"
  |> Out_channel.print_endline
