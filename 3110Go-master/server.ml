(** This module contains our server implementation.*)
open Lwt

(** [listen_address], [port]: Varibles that hold the host server ip address and port *)
let listen_address = Unix.inet_addr_loopback
let port = 3110
let backlog = 5

let moves_arr = Array.make 40 ""
let i = ref 0

let rec helper lst acc : string =
  match lst with
  | [] -> acc
  | h::t -> helper t (acc ^ " | " ^ h ^ " ")

(** [handle_message] takes as parameters: [string] that repsents a message.
    Intrustions:
      To display the past move type: show
      To add a move type: add (x,y). Ex. add(2,2)
    @return a [string] which varies on the input message .*)
let handle_message msg =
  begin
    match msg with
    | "show"  -> helper (Array.to_list moves_arr) ""
    |  x  ->  let sub = String.sub x 0 3 in
      begin
        match sub with 
        | "add" -> let len = String.length x in 
          moves_arr.(!i) <- String.sub x 4 (len-4); 
          i := !i + 1; "Move Recorded"
        | _ -> "Unknown move"
      end
  end


(** SOURCE: The functions below, are from the following source: 
    https://ocsigen.org/lwt/5.3.0/manual/manual
    These 4 functions implment a server that can be access 
    from mutiple computer. *)

(** [handle_connection] enables the server to handle connections.*)
let rec handle_connection ic oc () =
  Lwt_io.read_line_opt ic >>=
  (fun msg -> 
     match msg with
     | Some msg -> 
       let reply = handle_message msg in 
       Lwt_io.write_line oc reply >>= handle_connection ic oc
     | None -> Logs_lwt.info (fun m -> m "Connection closed") >>= return)

(** [accept_connection] enables the server to accept connections.*)
let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) 
    (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
  Logs_lwt.info (fun m -> m "New connection") >>= return

(** [create_socket] enables the server to handle connections.*)
let create_socket () =
  let open Lwt_unix in
  let socket = socket PF_INET SOCK_STREAM 0 in
  (bind socket @@ ADDR_INET(listen_address, port);
   listen socket backlog;
   socket)

(** [create_server] creates the server.*)
let create_server socket =
  let rec serve () =
    Lwt_unix.accept socket >>= accept_connection >>= serve
  in serve

(** Call all the above functions *)
let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let socket = create_socket () in
  let srv = create_server socket in
  Lwt_main.run @@ srv ()