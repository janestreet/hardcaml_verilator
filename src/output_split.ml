open! Core

type t =
  { lines_per_file : int option
  ; lines_per_function : int option
  }
[@@deriving sexp_of]

let create ?lines_per_file ?lines_per_function () =
  let check_gt0 msg t =
    Option.iter t ~f:(fun t ->
      if t <= 0 then raise_s [%message msg "must be >=0" (t : int)])
  in
  check_gt0 "lines per file" lines_per_file;
  check_gt0 "lines per function" lines_per_function;
  (match lines_per_file, lines_per_function with
   | Some lines_per_file', Some lines_per_function' ->
     if lines_per_file' < lines_per_function'
     then
       raise_s
         [%message
           "Number of lines output per file must be greater than the number of lines \
            output per function"
             (lines_per_file' : int)
             (lines_per_function' : int)]
   | Some _, None | None, Some _ | None, None -> ());
  { lines_per_file; lines_per_function }
;;

let to_string t =
  let s t = Option.value ~default:"0" (Option.map t ~f:Int.to_string) in
  String.concat
    ~sep:"-"
    [ "perfile" ^ s t.lines_per_file; "perfunc" ^ s t.lines_per_function ]
;;

let flag =
  [%map_open.Command
    let lines_per_file =
      flag "-lines-per-file" (optional int) ~doc:"Max number of lines per file"
    and lines_per_function =
      flag "-lines-per-function" (optional int) ~doc:"Max number of lines per function"
    in
    { lines_per_file; lines_per_function }]
;;

let split_options t =
  let lines_per_file =
    Option.map t.lines_per_file ~f:(fun v -> [ "--output-split "; Int.to_string v ])
    |> Option.value ~default:[]
  in
  let lines_per_function =
    Option.map t.lines_per_function ~f:(fun v ->
      [ "--output-split-cfuncs "
      ; Int.to_string v
      ; "--output-split-ctrace"
      ; Int.to_string v
      ])
    |> Option.value ~default:[]
  in
  String.concat ~sep:" " (List.concat [ lines_per_file; lines_per_function ])
;;

let v4_compat t = { t with lines_per_function = None }
