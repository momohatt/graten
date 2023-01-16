val concat : ?sep:string -> list(string) -> string
val concat_map : ~f:(char -> string) -> string -> string
val is_empty : string -> bool
val is_prefix : string -> ~prefix:string -> bool
val lowercase : string -> string
val of_char : char -> string
val of_char_list : list(char) -> string
val split : string -> ~on:char -> list(string)
