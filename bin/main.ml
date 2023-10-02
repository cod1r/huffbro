(*type node =
    | Leaf of { symbol : char; frequency : int }
    | Internal of { sum : int; left : node; right : node }
    | None
  ;;*)

let usage_msg = "Usage: huffbro <filename>"
let filename = ref ""

let set_filename filename_parameter =
  match !filename with
  | "" -> filename := filename_parameter
  | _ -> raise (Arg.Bad usage_msg)

let () = Arg.parse [] set_filename ""
let ic = In_channel.open_bin !filename
let file_contents_str = In_channel.input_all ic;;

print_endline file_contents_str
