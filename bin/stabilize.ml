open Why3
open Cmdliner
open Why3_tools.Api
open Why3_tools.Bisect

(*
  Clean up and save sessio
*)
let finalize cont _ =
  C.clean cont ~removed:(fun _ -> ()) None;

  (* let unproved =
       List.filter (fun id -> not (Session_itp.pn_proved cont.controller_session id)) roots
     in *)
  Session_itp.save_session cont.controller_session;
  begin
    Session_itp.save_session cont.controller_session;
    Format.printf "Successfully updated session"
  end

let init_env_conf opts =
  let open Whyconf in
  let config = init_config None in
  let main = get_main config in
  let lp = List.rev_append opts (loadpath main) in
  let config = set_main config (set_loadpath main lp) in
  Whyconf.load_plugins main;
  let _ =
    Getopt.parse_many [ Loc.Args.desc_no_warn ]
      [|
        "--warn-off=unused_variable"; "--warn-off=clone_not_abstract"; "--warn-off=axiom_abstract";
      |]
      0
  in
  Loc.Args.set_flags_selected ();
  (config, Env.create_env lp)

let stabilize why3_opts path =
  let config, env = init_env_conf why3_opts in
  let files = Queue.create () in
  if not (Sys.file_exists path) then begin
    Format.printf "Invalid file";
    exit 1
  end;
  Queue.push path files;
  let dir = Server_utils.get_session_dir ~allow_mkdir:false files in
  let ses = Session_itp.load_session dir in

  let cont = Controller_itp.create_controller config env ses in
  Controller_itp.set_session_max_tasks (Whyconf.running_provers_max (Whyconf.get_main config));

  let _, _ =
    try Controller_itp.reload_files ~ignore_shapes:true cont
    with Controller_itp.Errors_list l ->
      List.iter (fun e -> Format.printf "%a@." Exn_printer.exn_printer e) l;
      exit 1
  in

  let need_upgrade =
    Session_itp.fold_all_session cont.controller_session
      (fun acc any ->
        match any with
        | APa pa ->
            let node = Session_itp.get_proof_attempt_node cont.controller_session pa in
            begin
              match node.proof_state with
              | Some { Call_provers.pr_answer = Call_provers.Valid; pr_time; _ } ->
                  if pr_time >= node.limits.limit_time *. 0.5 || pr_time > 2.5 then pa :: acc
                  else acc
              | _ -> acc
            end
        | _ -> acc)
      []
  in

  if List.length need_upgrade = 0 then begin
    Format.printf "No tasks need stabilization, exiting\n";
    finalize cont need_upgrade;
    exit 0
  end;

  Format.printf "Need to stabilize %d goals \n" (List.length need_upgrade);

  let num_strats = ref (List.length need_upgrade) in
  List.iter
    (fun id ->
      Format.printf "Stabilizing goal '%s'\n"
        (Session_itp.get_proof_name cont.controller_session
           (Session_itp.get_proof_attempt_parent cont.controller_session id))
          .id_string;
      bisect_proof_attempt
      (* ~callback_tr:(fun _ _ _ -> ())
         ~callback_pa:(fun _ _ -> ())
      *)
        ~notification:(fun _ -> ())
        ~removed:(fun _ -> ())
        ~finalize:(fun _ ->
          num_strats := !num_strats - 1;

          if !num_strats = 0 then begin
            finalize cont need_upgrade;
            exit 0
          end)
        cont id)
    need_upgrade;

  let update_monitor w s r =
    Format.printf "Progress: %d/%d/%d      \r%!" w s r;
    Format.print_flush ()
  in
  C.register_observer update_monitor;
  Unix_scheduler.Unix_scheduler.main_loop ~prompt:"" (fun _ -> ())

let path =
  let docv = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv)

let load_path =
  let docv = "LIBRARY_PATH" in
  Arg.(value & opt_all string [] & info [ "L" ] ~docv)

let stabilize_cmd =
  Cmd.v
    (Cmd.info ~sdocs:"bisect unstable goals" "stabilize")
    Term.(const stabilize $ load_path $ path)
