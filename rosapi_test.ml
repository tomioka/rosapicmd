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
      printf "!!%s\n%s\n" (string_of_status status) s

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

let connect endpoint url language proto host port local_connection adm_output
    input options devel verbose () =
  let uri =
    match url with
    | Some s -> s
    | None ->
        let proto =
          match proto with
          | Some s -> s
          | None -> if local_connection then "http" else "https"
        in
        let host =
          match host with
          | Some s -> s
          | None -> if local_connection then "localhost" else "api.rosette.com"
        in
        let port =
          match port with
          | Some s -> ":" ^ s
          | None -> if local_connection then ":8181" else ""
        in
        sprintf "%s://%s%s" proto host port
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
      (* POST methods *)
      let content =
        match input with
        | Some "-" -> In_channel.input_all In_channel.stdin
        | Some filename -> In_channel.read_all filename
        | None -> "Boston is warm today."
      in
      let payload = [ ("content", `String content) ] in
      let payload =
        match options with
        | [] -> payload
        | l ->
            payload
            @ [
                ( "options",
                  `Assoc
                    (List.map l ~f:(fun s ->
                         match String.split s ~on:'=' with
                         | [ k; v ] -> (k, `String v)
                         | _ ->
                             raise
                               (Invalid_argument ("invalid option spec" ^ s))))
                );
              ]
      in
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
      +> flag "--url" (optional string)
           ~doc:"url override the url (before \"/rest/v1..\")"
      +> flag "--language" ~aliases:[ "-l" ] (optional string)
           ~doc:"language of input"
      +> flag "--protocol" (optional string) ~doc:"https or http"
      +> flag "--host" (optional string) ~doc:"host hostname to connect"
      +> flag "--port" (optional string) ~doc:"port port number"
      +> flag "--localhost" no_arg ~doc:"connect to localhost"
      +> flag "--adm" ~aliases:[ "-a" ] no_arg
           ~doc:"adm output instead of simple response"
      +> flag "--input" ~aliases:[ "-i" ] (optional string)
           ~doc:"input filename of input. '-' for stdin."
      +> flag "--option" ~aliases:[ "-o" ] (listed string)
           ~doc:"option request option key=value pair"
      +> flag "--devel" no_arg ~doc:"send X-RosetteAPI-Devel=true"
      +> flag "--verbose" ~aliases:[ "-v" ] no_arg ~doc:"verbose output")
    connect
  |> run
