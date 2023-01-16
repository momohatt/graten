val print_endline : string -> unit

module In_channel : sig
  val input_lines : in_channel -> list (string)
end

module Out_channel : sig
  val output_string : out_channel -> string -> unit
  val with_file : string -> ~f:(out_channel -> unit) -> unit
  val write_all : string -> ~data:string -> unit
end
