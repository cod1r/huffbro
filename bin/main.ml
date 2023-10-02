type node =
  | Leaf of { symbol : char; frequency : int }
  | Internal of { sum : int; left : node; right : node }
  | None

let usage_msg = "Usage: huffbro <filename>"
let filename = ref ""

let set_filename filename_parameter =
  match !filename with
  | "" -> filename := filename_parameter
  | _ -> raise (Arg.Bad usage_msg)

let () = Arg.parse [] set_filename ""
let ic = In_channel.open_bin !filename
let file_contents_str = In_channel.input_all ic

let f a c =
  let is_equal leaf =
    match leaf with
    | Leaf { symbol; _ } -> if symbol = c then true else false
    | _ ->
        raise
          (Invalid_argument "Only Leafs should be passed into this function")
  in
  if List.exists is_equal a then
    let item_found = List.find (fun i -> is_equal i) a in
    let filtered = List.filter (fun i -> is_equal i != true) a in
    match item_found with
    | Leaf { symbol = item_found_sym; frequency = item_found_freq } ->
        Leaf { symbol = item_found_sym; frequency = item_found_freq + 1 }
        :: filtered
    | _ ->
        raise
          (Invalid_argument "Only Leafs should be passed into this function")
  else Leaf { symbol = c; frequency = 1 } :: a

let nodes = String.fold_left f [] file_contents_str

let print_leaf leaf =
  match leaf with
  | Leaf { symbol; frequency } ->
      print_endline (Char.escaped symbol ^ " " ^ string_of_int frequency)
  | _ -> ()

let () = List.iter print_leaf nodes
