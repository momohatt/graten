open Base

type t =
  { input_lang : Lang.t
  ; output_lang : Lang.t
  ; pairs : list (string * string)
  }

let normalize str =
  String.lowercase str
  |> String.concat_map ~f:(fun c ->
         if Char.is_alphanum c
         then String.of_char c
         else (
           match c with
           | '!' -> " !"
           | '.' -> " ."
           | '?' -> " ?"
           | _ -> " "))

(* We only consider a subset of the dataset that starts with the
   following prefixes as this makes the dataset smaller and the translation
   task easier to learn
*)
let filter_prefix =
  let prefixes =
    [ "i am "
    ; "i m "
    ; "you are "
    ; "you re"
    ; "he is "
    ; "he s "
    ; "she is "
    ; "she s "
    ; "we are "
    ; "we re "
    ; "they are "
    ; "they re "
    ]
  in
  fun str -> List.exists prefixes ~f:(fun prefix -> String.is_prefix str ~prefix)

let length_in_words str =
  String.split str ~on:' '
  |> List.sum (* (module Int) *) ~f:(fun str -> if String.is_empty str then 0 else 1)

let filter_pair lhs rhs ~max_length =
  if length_in_words lhs < max_length
     && length_in_words rhs < max_length
     && (filter_prefix lhs || filter_prefix rhs)
  then Some (lhs, rhs)
  else None

let read_unicode_lines filename =
  let command = Printf.sprintf "iconv -f utf8 -t ascii//TRANSLIT \"%s\"" filename in
  let in_channel = Unix.open_process_in command in
  let lines = Stdio.In_channel.input_lines in_channel in
  let open Unix in (* EDIT: add 'open Unix' *)
  match Unix.close_process_in in_channel with
  | WEXITED 0   -> lines
  | WSIGNALED _ -> Printf.failwithf "command %s stopped with non-zero return code" command ()
  | WSTOPPED _  -> Printf.failwithf "command %s stopped with non-zero return code" command ()
  | WEXITED _   -> Printf.failwithf "command %s stopped with non-zero return code" command ()

let read_pairs ~input_lang ~output_lang ~max_length =
  let filename = Printf.sprintf "data/%s-%s.txt" input_lang output_lang in
  let lines = read_unicode_lines filename in
  List.filter_map lines ~f:(fun line ->
      match String.split line ~on:'\t' with
      | [ lhs; rhs ] -> filter_pair (normalize lhs) (normalize rhs) ~max_length
      | _ -> Printf.failwithf "Line %s is not a tab separated pair" line ())

let create ~input_lang ~output_lang ~max_length =
  let pairs = read_pairs ~input_lang ~output_lang ~max_length in
  let input_lang = Lang.create ~name:input_lang in
  let output_lang = Lang.create ~name:output_lang in
  List.iter pairs ~f:(fun (lhs, rhs) ->
      Lang.add_sentence input_lang lhs;
      Lang.add_sentence output_lang rhs);
  { input_lang; output_lang; pairs }

let input_lang t = t.input_lang
let output_lang t = t.output_lang

let pairs t =
  let to_indexes str lang =
    String.split str ~on:' '
    |> List.filter_map ~f:(Lang.get_index lang)
    |> fun l -> l @ [ Lang.eos_token lang ]
  in
  Array.of_list t.pairs
  |> Array.map ~f:(fun (lhs, rhs) ->
         let lhs = to_indexes lhs t.input_lang in
         let rhs = to_indexes rhs t.output_lang in
         lhs, rhs)

let reverse t =
  { input_lang = t.output_lang
  ; output_lang = t.input_lang
  ; pairs = List.map t.pairs ~f:(fun (s1, s2) -> s2, s1)
  }
