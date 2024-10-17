open Why3
module S = Unix_scheduler.Unix_scheduler
module C = Controller_itp.Make (Unix_scheduler.Unix_scheduler)

let fmt_strat_step fmt s =
  let open Strategy in
  match s with
  | Icall_prover prs ->
      Format.fprintf fmt "prover %a"
        (Format.pp_print_list (fun fmt (pr, _, _, _) -> Whyconf.print_prover fmt pr))
        prs
  | Itransform _ -> Format.fprintf fmt "transform"
  | Igoto n -> Format.fprintf fmt "goto %d" n

let call_one_prover (c : Controller_itp.controller) (p, timelimit, memlimit, steplimit) ~callback
    ~notification g =
  let main = Whyconf.get_main c.controller_config in
  let timelimit = Option.value ~default:(Whyconf.timelimit main) timelimit in
  let memlimit = Option.value ~default:(Whyconf.memlimit main) memlimit in
  let steplimit = Option.value ~default:0 steplimit in

  let limits =
    { Call_provers.limit_time = timelimit; limit_mem = memlimit; limit_steps = steplimit }
  in

  C.schedule_proof_attempt c g p ~limits ~callback ~notification

let run_strategy_on_goal c id strat ~notification ~finalize =
  let open Strategy in
  let open Controller_itp in
  let rec exec_strategy pc (mem : int ref list) (strat : Strategy.instruction array) g =
    let rec halt mem =
      match mem with
      | m :: tl ->
          m := !m - 1;
          if !m <= 0 then halt tl
      | [] -> finalize ()
    in
    if pc < 0 || pc >= Array.length strat then halt mem
    else begin
      match Array.get strat pc with
      | Icall_prover is ->
          let already_done = ref (List.length is) in
          let callback _panid res =
            match res with
            | UpgradeProver _ | Scheduled | Running -> (* nothing to do yet *) ()
            | Done { Call_provers.pr_answer = Call_provers.Valid; _ } ->
                (* proof succeeded, nothing more to do *)
                C.interrupt_proof_attempts_for_goal c g;
                already_done := 0;
                halt mem
            | Interrupted -> ()
                (* halt mem *)
            | Done _ ->
                already_done := !already_done - 1;
                if !already_done = 0 then begin

                  (* proof did not succeed, goto to next step *)
                  exec_strategy (pc + 1) mem strat g
                end
            | Undone | Detached | Uninstalled _ | Removed _  | InternalFailure _ ->
                (* should not happen *)
                assert false
          in
          List.iter (fun i -> call_one_prover c i ~callback ~notification g) is
      | Itransform (trname, pcsuccess) ->
          let callback ntr =
            match ntr with
            | TSfatal (_, _) -> halt mem
            | TSfailed _e ->
                (* transformation failed *)
                exec_strategy (pc + 1) mem strat g
            | TSscheduled -> ()
            | TSdone tid ->
                let sub_tasks = Session_itp.get_sub_tasks c.controller_session tid in
                let children = ref (List.length sub_tasks) in
                List.iter (fun g -> exec_strategy pcsuccess (children :: mem) strat g) sub_tasks
          in

          C.schedule_transformation c g trname [] ~callback ~notification
          
      | Igoto pc -> exec_strategy pc mem strat g
    end
  in
  exec_strategy 0 [] strat id
