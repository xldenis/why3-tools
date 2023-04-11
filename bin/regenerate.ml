open Why3
open Wstdlib
open Cmdliner
open Why3_tools.Api

(*
  Clean up and save sessio
*)
let finalize cont roots =
  C.clean cont ~removed:(fun _ -> ()) None;

  let unproved =
    List.filter (fun id -> not (Session_itp.pn_proved cont.controller_session id)) roots
  in

  (* Session_itp.save_session cont.controller_session; *)
  match unproved with
  | [] -> begin
      Session_itp.save_session cont.controller_session;
      Format.printf "Successfully updated session"
    end
  | _ ->
      List.iter
        (fun un_id ->
          Format.printf "Failed to prove %s"
            (Session_itp.get_proof_name cont.controller_session un_id).id_string)
        unproved

let regenerate_unproved cont strategy =
  let _, _, _, strat =
    Server_utils.load_strategies cont;
    try Hstr.find cont.controller_strategies strategy
    with Not_found ->
      Format.eprintf "Could not find the strategy %s" strategy;
      exit 1
  in

  (* remove detached *)
  C.clean cont ~removed:(fun _ -> ()) None;

  let root_tasks =
    let open Session_itp in
    let session = cont.controller_session in
    let is_root id = match get_proof_parent session id with Theory _ -> true | _ -> false in
    fold_all_session session
      (fun acc any ->
        match any with
        | APn id when is_root id && not (pn_proved session id) -> id :: acc
        | _ -> acc)
      []
  in

  if List.length root_tasks = 0 then begin
    Format.printf "No unproved tasks, exiting";
    finalize cont root_tasks;
    exit 0
  end;

  Format.printf "Found %d unproved root tasks, applying %s to each\n" (List.length root_tasks)
    strategy;
  Format.print_flush ();

  (* A reference which stores how many strategies we are launching *)
  let num_strats = ref (List.length root_tasks) in

  List.iter
    (fun id ->
      C.reset_proofs cont ~removed:(fun _ -> ()) ~notification:(fun _ -> ()) (Some (APn id));
      run_strategy_on_goal cont id strat
        ~finalize:(fun _ ->
          num_strats := !num_strats - 1;
          if !num_strats = 0 then begin
            finalize cont root_tasks;
            exit 0
          end)
        ~notification:(fun _ -> ()))
    root_tasks

let init_env_conf opts =
  let open Whyconf in
  let config = init_config None in
  let main = get_main config in
  let lp = List.rev_append opts (loadpath main) in
  let config = set_main config (set_loadpath main lp) in
  Whyconf.load_plugins main;
  (config, Env.create_env lp)

let regenerate why3_opts path strategy =
  let config, env = init_env_conf why3_opts in
  let files = Queue.create () in
  Queue.push path files;
  let dir = Server_utils.get_session_dir ~allow_mkdir:false files in
  let ses = Session_itp.load_session dir in

  let cont = Controller_itp.create_controller config env ses in
  Controller_itp.set_session_max_tasks (Whyconf.running_provers_max (Whyconf.get_main config));

  let found_obs, _ =
    try Controller_itp.reload_files cont
    with Controller_itp.Errors_list l ->
      List.iter (fun e -> Format.printf "%a@." Exn_printer.exn_printer e) l;
      exit 1
  in
  if found_obs then begin
    Format.printf "Found obsolete goals, replaying..\n";
    Format.print_flush ();
    C.replay ~valid_only:true ~obsolete_only:true cont
      ~callback:(fun _ _ -> ())
      ~notification:(fun _ -> ())
      ~final_callback:(fun _ _ -> regenerate_unproved cont strategy)
      ~any:None
  end
  else regenerate_unproved cont strategy;

  let update_monitor w s r =
    Format.printf "Progress: %d/%d/%d      \r%!" w s r;
    Format.print_flush ()
  in
  C.register_observer update_monitor;
  Unix_scheduler.Unix_scheduler.main_loop ~prompt:"" (fun _ -> ())

let path =
  let docv = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv)

let strategy =
  let docv = "STRATEGY" in
  Arg.(required & pos 1 (some string) None & info [] ~docv)

let load_path =
  let docv = "LIBRARY_PATH" in
  Arg.(value & opt_all string [] & info [ "L" ] ~docv)

let regenerate_cmd =
  Cmd.v
    (Cmd.info ~sdocs:"apply a strategy to a proof session" "regenerate")
    Term.(const regenerate $ load_path $ path $ strategy)
