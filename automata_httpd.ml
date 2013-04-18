
(** *)

(** Code taken from Easy_engine example of
  {{:https://godirepo.camlcity.org/wwwsvn/trunk/code/?root=lib-ocamlnet2}OCamlnet distribution}
  and adapted to our own purpose. *)

open Automata;;

(*c==v=[String.replace_in_string]=1.0====*)
let replace_in_string ~pat ~subs ~s =
  let len_pat = String.length pat in
  let len = String.length s in
  let b = Buffer.create len in
  let rec iter pos =
    if pos >= len then
      ()
    else
      if pos + len_pat > len then
        Buffer.add_string b (String.sub s pos (len - pos))
      else
        if String.sub s pos len_pat = pat then
          (
           Buffer.add_string b subs;
           iter (pos+len_pat)
          )
        else
          (
           Buffer.add_char b s.[pos];
           iter (pos+1);
          )
  in
  iter 0;
  Buffer.contents b
(*/c==v=[String.replace_in_string]=1.0====*)


exception Http_error of string * Nethttp.http_status

let compute spec_str =
  let spec = Automata.read_spec spec_str in
  let tree = Automata.context_tree_of_spec spec in

  let tree_dot = Automata.dot_of_context_tree
    spec.spec_sym (fun _ _ _ -> ["shape","triangle"; "label",""]) tree
  in
  let act = Automata.automata_context_tree tree in

  let cct = Automata.complemented_context_tree act in
  let auto_dot = Automata.dot_of_cct spec.spec_sym cct in

  let compl_dot = Automata.dot_of_tree_diff spec.spec_sym tree cct in
  (tree_dot, compl_dot, auto_dot)
;;

let svg_width = 700;;
let svg_height = 700;;
let svg_dpi = 96. ;;

let dot_to_svg ?(program="dot") ?(options="") ?size dot =
  let temp_file = Filename.temp_file "automata" "svg" in
  let com = Printf.sprintf "echo %s |%s %s -Tsvg | tail --lines=+%d > %s"
    (Filename.quote dot) (Filename.quote program) options
    (match size with None -> 7 | Some _ -> 9)
    (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let svg = Automata.string_of_file temp_file in
      (* remove bad xlink:title="&lt;TABLE&gt;" in svg code *)
      let svg = replace_in_string ~pat: "xlink:title=\"&lt;TABLE&gt;\"" ~subs: "" ~s: svg in
      Sys.remove temp_file;
      let svg =
        match size with
          None -> svg
        | Some (w,h) ->
            Printf.sprintf "<svg width=\"%d\" height=\"%d\" viewBox=\"0.0 0.0 %d.00 %d.00\"\n
          xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n%s"
              w h w h svg
      in
      svg
  | n ->
      let msg = Printf.sprintf "Execution failed (%d): %s" n com in
      failwith msg
;;


let dot_to_svg ?(svg_w=svg_width) ?(svg_h=svg_height) dot =
  let w = (float svg_w) /. svg_dpi in
  let h = (float svg_h) /. svg_dpi in
  let options = Printf.sprintf "-Gfontsize=8. -Gdpi=%.2f -Gsize=\"%.0f,%.0f!\"" svg_dpi w h in
  dot_to_svg ~options ~size: (svg_w,svg_h) dot
;;

let form (cgi : Netcgi.cgi_activation) =
  Printf.sprintf "
    <p>Type the contexts in the following form.</p>
    <p>The first line is the <strong>list of symbols</strong>, for example <code>01</code></p>
    <p>On each following line, just type a <strong>context path</strong>, for example <code>0110</code></p>
    <form action=%S>
    <textarea cols=\"80\" rows=\"40\" name=\"spec\">01
1
01
000
0010
0011
</textarea>
    <input type=\"submit\"/>
    </form>"
      (cgi#url())
;;

let mk_page contents =
  "<html><body><h1>Complementing Context Trees</h1>"^contents^"</body></html>"
;;


let handle_http_query (cgi : Netcgi.cgi_activation) =
  try
    let spec = cgi#argument_value "spec" in
    let contents =
      match spec with
        "" -> form cgi
      | spec_str ->
          let (tree_dot, compl_dot, auto_dot) = compute spec_str in
          let tree_svg = dot_to_svg tree_dot in
          let compl_svg = dot_to_svg compl_dot in
          let auto_svg = dot_to_svg ~svg_w: 1000 auto_dot in
          "<h2>Input context tree</h2>\n"^tree_svg^"\n"^
          "<h2>Complemented context tree</h2>\n"^compl_svg^"\n"^
          "<h2>Automata details</h2>\n"^auto_svg
    in
    let page = mk_page contents in
    cgi#set_header
      ~cache:`No_cache
      ~content_type:("text/html; charset=\"UTF-8\"")
      ();
    cgi#output#output_string page;
    cgi#output#commit_work ()
  with
    e ->
      let (msg, status) =
        match e with
          Failure msg -> (msg, `Bad_request)
        | Http_error (msg, status) -> (msg, status)
        | _ -> (Printexc.to_string e, `Internal_server_error)
      in
      (* A Netcgi-based content provider *)
      cgi#set_header
        ~status
        ~cache:`No_cache
        ~content_type:"text; charset=\"UTF-8\""
        ();
      cgi#output#output_string msg;
      cgi#output#commit_work ()
;;

let on_request notification =
  (* This function is called when the full HTTP request has been received. For
   * simplicity, we create a [std_activation] to serve the request.
   *
   * An advanced implementation could set up further notifications to get informed
   * whenever there is space in the response buffer for additional output.
   * Currently, data is fully buffered (first
   * in the transactional buffer, then in the response buffer), and only when
   * the message is complete, the transmission to the client starts.
   * By generating only the next part of the response when there is space in
   * the response buffer, the advanced implementation can prevent that the
   * buffers become large.
   *)
  print_endline "Received HTTP request";
  ( try
      let env = notification#environment in
      let cgi =
       Netcgi_common.cgi_with_args
         (new Netcgi_common.cgi)
         (env :> Netcgi.cgi_environment)
         Netcgi.buffered_transactional_outtype
         env#input_channel
         (fun _ _ _ -> `Automatic)
     in
     handle_http_query cgi;
    with
     e ->
       print_endline ("Uncaught exception: " ^ (Printexc.to_string e))
  );
  notification#schedule_finish ()
;;

let on_request_header (notification : Nethttpd_engine.http_request_header_notification) =
  (* After receiving the HTTP header: We always decide to accept the HTTP body, if any
   * is following. We do not set up special processing of this body, it is just
   * buffered until complete. Then [on_request] will be called.
   *
   * An advanced server could set up a further notification for the HTTP body. This
   * additional function would be called whenever new body data arrives. (Do so by
   * calling [notification # environment # input_ch_async # request_notification].)
   *)
  print_endline "Received HTTP header";
  notification#schedule_accept_body ~on_request ()
;;

let serve_connection ues fd =
  (* Creates the http engine for the connection [fd]. When a HTTP header is received
   * the function [on_request_header] is called.
   *)
  let config = Nethttpd_engine.default_http_engine_config in
  Unix.set_nonblock fd;
  let _http_engine =
    new Nethttpd_engine.http_engine ~on_request_header () config fd ues in
  ()
;;
let rec accept ues srv_sock_acc =
  (* This function accepts the next connection using the [acc_engine]. After the
   * connection has been accepted, it is served by [serve_connection], and the
   * next connection will be waited for (recursive call of [accept]). Because
   * [server_connection] returns immediately (it only sets the callbacks needed
   * for serving), the recursive call is also done immediately.
   *)
  let acc_engine = srv_sock_acc#accept () in

  Uq_engines.when_state
    ~is_done:(fun (fd, fd_spec) ->
       if srv_sock_acc#multiple_connections then
         (
          serve_connection ues fd;
          accept ues srv_sock_acc
         )
       else
         srv_sock_acc#shut_down ()
    )
    ~is_error:(fun _ -> srv_sock_acc#shut_down())
    acc_engine
;;

let start_server ?(host=Unix.inet_addr_any) ~pending ~port =
  (* We set up [lstn_engine] whose only purpose is to create a server socket listening
   * on the specified port. When the socket is set up, [accept] is called.
   *)
  let ues = Unixqueue.create_unix_event_system () in
  (* Unixqueue.set_debug_mode true; *)
  let opts =
    { (*Uq_engines.default_listen_options with*)
      Uq_engines.lstn_backlog = pending ;
      Uq_engines.lstn_reuseaddr = true ;
    }
  in
  let lstn_engine =
    Uq_engines.listener
      (`Socket(`Sock_inet(Unix.SOCK_STREAM, host, port), opts)) ues
  in
  Uq_engines.when_state ~is_done:(accept ues) lstn_engine;
  (* Start the main event loop. *)
  Unixqueue.run ues
;;

let inet_addr_of_name host =
  try
    (Unix.gethostbyname host).Unix.h_addr_list.(0)
  with _ ->
      try
        Unix.inet_addr_of_string host
      with _ ->
          let message =
            Printf.sprintf "inet_addr_of_name %s : unknown host" host
          in
          raise (Failure message)
;;

let main () =
  let port = ref 8915 in
  let host = ref None in
  let pending = ref 20 in
  let options =
    [
      "-p", Arg.Set_int port,
      "<n> Listen to port number <n>; default is "^(string_of_int !port) ;

      "-h", Arg.String (fun s -> host := Some s),
      "<name> Reply to connections on host <name>; default is to reply\n\t\tto any query; (\"localhost\" can be used as <name>)" ;

      "-q", Arg.Set_int pending,
      "<n> Set maximum number of pending connections; default is "^ (string_of_int !pending) ;

    ]
  in
  let options = Arg.align options in
  Arg.parse options (fun _ -> ())
     (Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0));

  Netsys_signal.init();
  let host =
    match !host with
      None -> None
    | Some "localhost" -> Some Unix.inet_addr_loopback
    | Some h -> Some (inet_addr_of_name h)
  in
  start_server ?host ~pending: !pending ~port: !port
;;

try main ()
with
  Sys_error msg
| Failure msg ->
    prerr_endline msg; exit 1
| e ->
    prerr_endline (Printexc.to_string e);
    exit 1
;;
