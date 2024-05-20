open Why3
open Cmdliner
open Why3_tools.Api

let pa_name (cont : Controller_itp.controller) pa =
  let pn = Session_itp.get_proof_attempt_parent cont.controller_session pa in
  (Session_itp.get_proof_name cont.controller_session pn).id_string

(*
  Clean up and save session
*)
let finalize allow_partial (cont : Controller_itp.controller) failed =
  (* Session_itp.save_session cont.controller_session; *)
  match failed with
  | [] -> begin
      Session_itp.save_session cont.controller_session;
      Format.printf "Successfully updated session\n";
      exit 0
    end
  | _ ->
    if allow_partial then Session_itp.save_session cont.controller_session;
      Format.printf "Failed to update session\n";
      exit 1

open Whyconf

let upgrade_prover allow_partial (cont : Controller_itp.controller) (upgrade : prover Hprover.t) =
  let open Session_itp in
  let session = cont.controller_session in
  let need_upgrade =
    fold_all_session session
      (fun acc any ->
        match any with
        | APa pa ->
            let node = get_proof_attempt_node session pa in
            if Hprover.mem upgrade node.prover then pa :: acc else acc
        | _ -> acc)
      []
  in

  let remaining = ref (List.length need_upgrade) in
  let failed = ref [] in

  Format.printf "Upgrading %d proof attempts\n" !remaining;
  Format.print_flush ();
  let finalize () =
    if !remaining = 0 then
      finalize allow_partial cont !failed
  in

  List.iter
    (fun pa ->
      let pn = get_proof_attempt_parent session pa in
      let node = get_proof_attempt_node session pa in
      let new_prover = Hprover.find upgrade node.prover in
      C.schedule_proof_attempt cont pn new_prover ~limit:node.limit
        ~callback:(fun id status ->
          match status with
          | Scheduled -> ()
          | Done res_status -> begin
              remaining := !remaining - 1;
              let old_status = Option.get node.proof_state in

              if old_status.pr_answer <> res_status.pr_answer then begin
                Format.eprintf "Failed to upgrade %s\n" (pa_name cont pa);
                Format.print_flush ();
                failed := id :: !failed
              end;
              remove_proof_attempt session pn node.prover;
              finalize ()
            end
          | Running -> ()
          | _ -> begin
              Format.eprintf "Prover failed to run on task %s\n" (pa_name cont pa);
              Format.print_flush ();
              remaining := !remaining - 1;
              failed := id :: !failed;
              finalize ()
            end)
        ~notification:(fun _ -> ()))
    need_upgrade;
  finalize ()

let init_env_conf opts =
  let open Whyconf in
  let config = init_config None in
  let main = get_main config in
  let lp = List.rev_append opts (loadpath main) in
  let config = set_main config (set_loadpath main lp) in
  Whyconf.load_plugins main;
  (config, Env.create_env lp)

let load_session why3_opts dir =
  let config, env = init_env_conf why3_opts in
  Loc.set_warning_hook (fun ?loc:_ _ -> ());
  let files = Queue.create () in
  Queue.push dir files;
  let dir = Server_utils.get_session_dir ~allow_mkdir:false files in
  let ses = Session_itp.load_session dir in

  let cont = Controller_itp.create_controller config env ses in
  Controller_itp.set_session_max_tasks (Whyconf.running_provers_max (Whyconf.get_main config));
  cont

let validate_upgrade (cont : Controller_itp.controller) upgrade =
  let config = cont.controller_config in

  Hprover.iter
    (fun _ tgt ->
      if not (Whyconf.is_prover_known config tgt) then begin
        Format.printf "Prover %s not found, known choices are: \n" (prover_parseable_format tgt);
        Mprover.iter
          (fun prover _ -> Format.printf "%s\n" (prover_parseable_format prover))
          (Whyconf.get_provers config);
        exit 1
      end)
    upgrade

let upgrade allow_partial why3_opts path upgrade_specs =
  let cont = load_session why3_opts path in
  let found_obs, _ =
    try Controller_itp.reload_files ~ignore_shapes:true cont
    with Controller_itp.Errors_list l ->
      List.iter (fun e -> Format.printf "%a@." Exn_printer.exn_printer e) l;
      exit 1
  in
  let upgrades = Hprover.create 5 in
  List.iter (fun (from, to_) -> Hprover.add upgrades from to_) upgrade_specs;

  validate_upgrade cont upgrades;
  if found_obs then begin
    Format.printf "Found obsolete goals, replaying..\n";
    Format.print_flush ();
    C.replay ~valid_only:true ~obsolete_only:true cont
      ~callback:(fun _ _ -> ())
      ~notification:(fun _ -> ())
      ~final_callback:(fun _ _ -> upgrade_prover allow_partial cont upgrades)
      ~any:None
  end
  else upgrade_prover allow_partial cont upgrades;

  let update_monitor w s r =
    Format.printf "Progress: %d/%d/%d      \r%!" w s r;
    Format.print_flush ()
  in
  C.register_observer update_monitor;
  Unix_scheduler.Unix_scheduler.main_loop ~prompt:"" (fun _ -> ())

let path =
  let docv = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv)

let parse_prover str : (Whyconf.prover, string) result =
  match String.split_on_char ',' str with
  | [ nm; version ] -> Ok { prover_name = nm; prover_version = version; prover_altern = "" }
  | [ nm; version; special ] ->
      Ok { prover_name = nm; prover_version = version; prover_altern = special }
  | _ -> Error "invalid prover"

let print_prover = Whyconf.print_prover
let prover_conv : Whyconf.prover Arg.conv = Arg.(conv' (parse_prover, print_prover))

let prover_upgrade : (prover * prover) list Cmdliner.Term.t =
  let docv = "UPGRADE" in
  Arg.(non_empty & opt_all (pair ~sep:'=' prover_conv prover_conv) [] & info [ "upgrade" ] ~docv)

let load_path =
  let docv = "LIBRARY_PATH" in
  Arg.(value & opt_all string [] & info [ "L" ] ~docv)

let allow_partial =
  let doc = "Save a partially upgraded session even if there are still failing VCs" in
  Arg.(value & flag & info ["allow-partial"] ~doc)

let upgrade_cmd =
  Cmd.v
    (Cmd.info ~sdocs:"upgrade a prover in a session" "upgrade")
    Term.(const upgrade $ allow_partial $ load_path $ path $ prover_upgrade)
