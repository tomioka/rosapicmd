open Core
open Async
open Cohttp
open Cohttp_async

let dump_response resp body =
  let open Code in
  let status = Response.status resp in
  match status with
  | #success_status -> Body.to_string body >>| fun s -> printf "%s\n" s
  | _ ->
      Body.to_string body >>| fun s ->
      printf "%s\n%s\n" (string_of_status status) s

type endpoint = [ `Info | `Ping | `Entities | `Morphology_complete | `Tokens ]
[@@deriving sexp]

type get_endpoint = [ `Info | `Ping ] [@@deriving sexp]

let is_get_endpoint endpoint =
  match endpoint with "info" | "ping" -> true | _ -> false

let string_of_endpoint = function
  | `Info -> "info"
  | `Ping -> "ping"
  | `Entities -> "entities"
  | `Morphology_complete -> "morphology/complete"
  | `Tokens -> "tokens"

let endpoint_of_string = function
  | "info" -> `Info
  | "ping" -> `Ping
  | "entities" -> `Entities
  | "morphology/complete" -> `Morphology_complete
  | "tokens" -> `Tokens
  | s -> raise (Invalid_argument ("endpoint_of_string: " ^ s))

let endpoints = [ "info"; "ping"; "entities"; "morphology/complete"; "tokens" ]

let validate_endpoint e =
  match List.mem endpoints e ~equal:String.equal with
  | true -> e
  | false ->
      raise
        (Invalid_argument
           (sprintf "Unknown endpoint %s; expected: %s" e
              (String.concat ~sep:", " endpoints)))

let connect endpoint language local_connection adm_output input devel verbose ()
    =
  let uri =
    match local_connection with
    | true -> "http://localhost:8181"
    | false -> "https://api.rosette.com"
  in
  let uri = sprintf "%s/rest/v1/%s" uri endpoint |> Uri.of_string in
  let uri =
    match adm_output with
    | true -> Uri.with_query uri [ ("output", [ "rosette" ]) ]
    | false -> uri
  in
  let headers = [] in
  let headers =
    match devel with
    | true -> headers @ [ ("X-RosetteAPI-Devel", "true") ]
    | false -> headers
  in
  let headers =
    match Sys.getenv "ROSAPI_KEY" with
    | Some v -> headers @ [ ("X-RosetteAPI-KEY", v) ]
    | _ -> headers
  in
  let headers = headers |> Header.of_list in
  if verbose then printf "TRACE:%s\n" (Uri.to_string uri);
  match is_get_endpoint endpoint with
  | true ->
      Client.get uri ~headers >>= fun (resp, body) -> dump_response resp body
  | false ->
      let content =
        match input with
        | Some "-" -> In_channel.input_all In_channel.stdin
        | Some filename -> In_channel.read_all filename
        | None -> "Boston is warm today."
      in
      let payload = [ ("content", `String content) ] in
      let payload =
        match language with
        | Some s -> payload @ [ ("language", `String s) ]
        | None -> payload
      in
      let payload = Yojson.Basic.to_string (`Assoc payload) in
      if verbose then printf "TRACE:%s\n" payload;
      Client.post uri ~body:(Body.of_string payload) ~headers
      >>= fun (resp, body) -> dump_response resp body

let () =
  let open Async_command in
  async_spec ~summary:"simple Rosette API client"
    Spec.(
      empty
      +> anon (map_anons ~f:validate_endpoint ("endpoint" %: string))
      +> flag "--language" ~aliases:[ "-l" ] (optional string)
           ~doc:"language of input"
      +> flag "--localhost" no_arg ~doc:"connect to localhost"
      +> flag "--adm" ~aliases:[ "-a" ] no_arg
           ~doc:"adm output instead of simple response"
      +> flag "--input" ~aliases:[ "-i" ] (optional string) ~doc:"input"
      +> flag "--devel" no_arg ~doc:"send X-RosetteAPI-Devel=true"
      +> flag "--verbose" ~aliases:[ "-v" ] no_arg ~doc:"verbose output")
    connect
  |> run
