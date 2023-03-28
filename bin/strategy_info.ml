open Why3
open Wstdlib
open Cmdliner
module S = Unix_scheduler.Unix_scheduler
module C = Controller_itp.Make (Unix_scheduler.Unix_scheduler)

let fmt_strat_step fmt s =
  let open Strategy in
  match s with
  | Icall_prover (pr, _, _, _) -> Format.fprintf fmt "prover %a" Whyconf.print_prover pr
  | Itransform _ -> Format.fprintf fmt "transform"
  | Igoto n -> Format.fprintf fmt "goto %d" n

type id = int
type kind = Prover | Transformation
type step = Start of id * string | End of id

let run_strategy_on_goal c id strat ~notification ~finalize ~callback =
  let open Strategy in
  let open Controller_itp in
  let cnt = ref 0 in
  let rec exec_strategy id pc (mem : int ref list) (strat : Strategy.instruction array) g =
    let rec halt mem =
      match mem with
      | m :: tl ->
          m := !m - 1;
          if !m <= 0 then halt tl
      | [] -> finalize ()
    in
    if pc < 0 || pc >= Array.length strat then halt mem
    else begin
      let cur_id = !id in
      id := !id + 1;
      match Array.get strat pc with
      | Icall_prover (p, timelimit, memlimit, steplimit) ->
          let main = Whyconf.get_main c.controller_config in
          let timelimit = Opt.get_def (Whyconf.timelimit main) timelimit in
          let memlimit = Opt.get_def (Whyconf.memlimit main) memlimit in
          let steplimit = Opt.get_def 0 steplimit in
          let callback _panid res =
            match res with
            | Running -> callback (Start (cur_id, "prover"))
            | UpgradeProver _ | Scheduled -> (* nothing to do yet *) ()
            | Done { Call_provers.pr_answer = Call_provers.Valid; _ } ->
                (* proof succeeded, nothing more to do *)
                callback (End cur_id);

                halt mem
            | Interrupted ->
                callback (End cur_id);

                halt mem
            | Done _ | InternalFailure _ ->
                callback (End cur_id);
                (* proof did not succeed, goto to next step *)
                exec_strategy id (pc + 1) mem strat g
            | Undone | Detached | Uninstalled _ | Removed _ ->
                (* should not happen *)
                assert false
          in
          let limit =
            { Call_provers.limit_time = timelimit; limit_mem = memlimit; limit_steps = steplimit }
          in
          C.schedule_proof_attempt c g p ~limit ~callback ~notification
      | Itransform (trname, pcsuccess) ->
          let callback ntr =
            match ntr with
            | TSfatal (_, _) -> callback (End cur_id); halt mem
            | TSfailed _e ->
                callback (End cur_id);
                (* transformation failed *)
                exec_strategy id (pc + 1) mem strat g
            | TSscheduled -> callback (Start (cur_id, "transform"));
            | TSdone tid ->
                callback (End cur_id);
                let sub_tasks = Session_itp.get_sub_tasks c.controller_session tid in
                let children = ref (List.length sub_tasks) in
                List.iter (fun g -> exec_strategy id pcsuccess (children :: mem) strat g) sub_tasks
          in

          begin
            match Session_itp.get_transformation c.controller_session g trname [] with
            | tid -> callback (TSdone tid)
            | exception Not_found -> C.schedule_transformation c g trname [] ~callback ~notification
          end
      | Igoto pc -> exec_strategy id pc mem strat g
    end
  in
  exec_strategy cnt 0 [] strat id

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
  | [] -> begin Format.eprintf "Successfully proved session" end
  | _ ->
      List.iter
        (fun un_id ->
          Format.eprintf "Failed to prove %s"
            (Session_itp.get_proof_name cont.controller_session un_id).id_string)
        unproved

let init_env_conf opts =
  let open Whyconf in
  let config = init_config None in
  let main = get_main config in
  let lp = List.rev_append opts (loadpath main) in
  let config = set_main config (set_loadpath main lp) in
  Whyconf.load_plugins main;
  (config, Env.create_env lp)

let strategy_info why3_opts path strategy =
  let config, env = init_env_conf why3_opts in
  let files = Queue.create () in
  Queue.push path files;
  let dir = Server_utils.get_session_dir ~allow_mkdir:false files in
  let ses = Session_itp.load_session dir in

  let cont = Controller_itp.create_controller config env ses in
  Server_utils.load_strategies cont;
  Controller_itp.set_session_max_tasks (Whyconf.running_provers_max (Whyconf.get_main config));

  let found_obs, _ =
    try Controller_itp.reload_files cont
    with Controller_itp.Errors_list l ->
      List.iter (fun e -> Format.printf "%a@." Exn_printer.exn_printer e) l;
      exit 1
  in
  if found_obs then Format.eprintf "Found obsolete goals..\n";

  C.reset_proofs cont ~removed:(fun _ -> ()) ~notification:(fun _ -> ()) None;

  let _, _, _, strat =
    try Hstr.find cont.controller_strategies strategy
    with Not_found ->
      Format.eprintf "Could not find the strategy %s" strategy;
      exit 1
  in

  let root_tasks =
    Session_itp.fold_all_session cont.controller_session
      (fun acc any -> match any with APn id -> id :: acc | _ -> acc)
      []
  in

  Format.eprintf "Found %d root tasks, applying %s to each\n" (List.length root_tasks) strategy;
  Format.print_flush ();

  (* A reference which stores how many strategies we are launching *)
  let num_strats = ref (List.length root_tasks) in

  Format.printf "run_id,step_id,step_kind,step_start,step_end\n";
  List.iter
    (fun id ->
      let times : (int, string * float * float option) Hashtbl.t = Hashtbl.create 32 in

      run_strategy_on_goal cont id strat
        ~finalize:(fun _ ->
          num_strats := !num_strats - 1;

          Hashtbl.iter
            (fun (step : int) (k, strt, Some e) ->
              Format.printf "%d, %d, %s, %d, %d\n" (Obj.magic id) step k
                (int_of_float (strt *. 1000.))
                (int_of_float (e *. 1000.)))
            times;

          if !num_strats = 0 then begin
            finalize cont root_tasks;
            exit 0
          end)
        ~notification:(fun _ -> ())
        ~callback:(fun s ->
          match s with
          | Start (id, k) -> Hashtbl.replace times id (k, Unix.gettimeofday (), None)
          | End id ->
              let k, s, _ = Hashtbl.find times id in
              Hashtbl.replace times id (k, s, Some (Unix.gettimeofday ()))))
    root_tasks;

  let update_monitor w s r =
    Format.eprintf "Progress: %d/%d/%d\r%!" w s r;
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

let cmd =
  Cmd.v
    (Cmd.info ~sdocs:"build a timeline of strategy execution\n      " "strategy_info")
    Term.(const strategy_info $ load_path $ path $ strategy)
