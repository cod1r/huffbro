type node =
  | Leaf of { symbol : char; frequency : int }
  | Internal of { sum : int; left : node; right : node }

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

let leaves = String.fold_left f [] file_contents_str

let sort_nodes n1 n2 =
  match n1 with
  | Leaf { frequency = frequency1; _ } -> (
      match n2 with
      | Leaf { frequency = frequency2; _ } -> frequency1 - frequency2
      | Internal { sum = sum2; _ } -> frequency1 - sum2)
  | Internal { sum = sum1; _ } -> (
      match n2 with
      | Leaf { frequency = frequency2; _ } -> sum1 - frequency2
      | Internal { sum = sum2; _ } -> sum1 - sum2)

let rec huffin lst =
  match lst with
  | [ _ ] -> lst
  | _ -> (
      let sorted_lst = List.sort sort_nodes lst in
      match sorted_lst with
      | hd :: hd2 :: tl ->
          let internal =
            match (hd, hd2) with
            | Leaf { frequency = f; _ }, Leaf { frequency = f2; _ } ->
                Internal { sum = f + f2; left = hd; right = hd2 }
            | Leaf { frequency = f; _ }, Internal { sum = s; _ } ->
                Internal { sum = s + f; left = hd; right = hd2 }
            | Internal { sum = s; _ }, Leaf { frequency = f; _ } ->
                Internal { sum = s + f; left = hd; right = hd2 }
            | Internal { sum = s; _ }, Internal { sum = s2; _ } ->
                Internal { sum = s + s2; left = hd; right = hd2 }
          in
          huffin (internal :: tl)
      | _ -> failwith "List cannot be length one here")

let tree =
  let huffed = huffin leaves in
  match huffin leaves with
  | [ hd ] -> hd
  | _ ->
      failwith
        ("List returned cannot be greater than length one: length is "
        ^ string_of_int (List.length huffed))

let rec get_codes tree str =
  match tree with
  | Internal { left = l; right = r; _ } ->
      get_codes l (str ^ "0") @ get_codes r (str ^ "1")
  | Leaf { symbol = s; _ } -> [ (s, str) ]

let codes = get_codes tree ""

let () =
  List.iter
    (fun p -> match p with s, c -> print_endline (Char.escaped s ^ " " ^ c))
    codes
