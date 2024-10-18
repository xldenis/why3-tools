open Why3
open Api
open Session_itp
open Controller_itp

type ('a, 'r) cont = Cont of (('a -> 'r) -> 'r)

let ( >>= ) (Cont a : ('a, 'r) cont) (fb : 'a -> ('b, 'r) cont) : ('b, 'r) cont =
  Cont
    (fun b ->
      a (fun a ->
          let (Cont c) = fb a in
          c b))

let return (a : 'a) : ('a, 'r) cont = Cont (fun k -> k a)
let ( let* ) a fb = a >>= fb
let debug = Debug.register_flag ~desc:"Task bisection" "bisect"

let proof_is_complete pa =
  match pa.Session_itp.proof_state with
  | None -> false
  | Some pr -> (not pa.Session_itp.proof_obsolete) && Call_provers.(pr.pr_answer = Valid)

let create_rem_list =
  let b = Buffer.create 17 in
  fun rem ->
    Buffer.clear b;
    let add pr id =
      if Buffer.length b > 0 then Buffer.add_char b ',';
      Buffer.add_string b (Pp.string_of pr id)
    in
    let module P =
      (val Pretty.create ~do_forget_all:false rem.Eliminate_definition.rem_nt.Trans.printer
             rem.Eliminate_definition.rem_nt.Trans.aprinter
             rem.Eliminate_definition.rem_nt.Trans.printer
             rem.Eliminate_definition.rem_nt.Trans.printer)
    in
    let remove_ts ts = add P.print_ts ts in
    let remove_ls ls = add P.print_ls ls in
    let remove_pr pr = add P.print_pr pr in
    Ty.Sts.iter remove_ts rem.Eliminate_definition.rem_ts;
    Term.Sls.iter remove_ls rem.Eliminate_definition.rem_ls;
    Decl.Spr.iter remove_pr rem.Eliminate_definition.rem_pr;
    Buffer.contents b

exception CannotRunBisectionOn of proofAttemptID

let apply_remove c goal_id rem ~notification : (_ option, 'r) cont =
  Cont
    (fun k ->
      let rem = create_rem_list rem in
      let callback = function
        | TSscheduled -> ()
        | TSfatal (_, exn) | TSfailed (_, exn) ->
            Debug.dprintf debug "[Bisect] transformation failed %a@." Exn_printer.exn_printer exn;
            k None
        | TSdone trid -> k (Some trid)
      in
      C.schedule_transformation c goal_id "remove" [ rem ] ~callback ~notification)

let apply_prover c pn prover limit ~notification : (Call_provers.prover_result, 'r) cont =
  Cont
    (fun k ->
      let callback _ = function
        | UpgradeProver _ | Removed _ -> ()
        | Scheduled -> Debug.dprintf debug "[Bisect] prover on subtask is scheduled@."
        | Running -> Debug.dprintf debug "[Bisect] prover on subtask is running@."
        | Detached | Uninstalled _ -> assert false
        | Undone | Interrupted -> Debug.dprintf debug "Bisecting interrupted.@."
        | InternalFailure exn ->
            (* Perhaps the test can be considered false in this case? *)
            Debug.dprintf debug "[Bisect] prover interrupted by an error: %a.@."
              Exn_printer.exn_printer exn
        | Done res -> k res
      in
      C.schedule_proof_attempt c pn prover ~limit ~callback ~notification)

let bisect_step c goal_id prover limit rem ~notification ~removed : (bool, 'r) cont =
  let ses = c.controller_session in
  let* id = apply_remove c goal_id rem ~notification in
  match id with
  | None -> return false
  | Some id ->
      let pn = List.nth (get_sub_tasks ses id) 0 in
      let* res = apply_prover c pn prover limit ~notification in
      Debug.dprintf debug "[Bisect] prover on subtask returns %a@." Call_provers.print_prover_answer
        res.Call_provers.pr_answer;

      Session_itp.remove_subtree ~notification ~removed ses (Session_itp.ATn id);

      let b = res.Call_provers.pr_answer = Call_provers.Valid in

      return b

let bisect_done c rem ~notification goal_id prover limit : (unit, 'r) cont =
  let ses = c.controller_session in
  if
    Decl.Spr.is_empty rem.Eliminate_definition.rem_pr
    && Term.Sls.is_empty rem.Eliminate_definition.rem_ls
    && Ty.Sts.is_empty rem.Eliminate_definition.rem_ts
  then begin
    Debug.dprintf debug "Bisecting didn't reduce the task.@.";
    return ()
  end
  else begin
    Debug.dprintf debug "Bisecting done.@.";
    let* id = apply_remove c goal_id rem ~notification in
    let id = Option.get id in
    let pn = List.nth (get_sub_tasks ses id) 0 in
    let* res = apply_prover c pn prover limit ~notification in
    assert (res.Call_provers.pr_answer = Call_provers.Valid);
    return ()
  end

let rec decide_step ~notification ~removed c goal_id prover limit rem limit_k kont :
    (unit, unit) cont =
  let* b = bisect_step c goal_id prover limit rem ~notification ~removed in

  if limit_k limit then bisect_done c rem ~notification goal_id prover limit
  else
    match kont b with
    | Eliminate_definition.BSstep (rem, kont) ->
        decide_step ~notification ~removed c goal_id prover limit rem limit_k kont
    | Eliminate_definition.BSdone rem -> bisect_done c rem ~notification goal_id prover limit

let bisect_proof_attempt ~notification ~removed ~finalize (c : controller) pa_id =
  let ses = c.controller_session in
  let pa = get_proof_attempt_node ses pa_id in
  if not (proof_is_complete pa) then raise (CannotRunBisectionOn pa_id);
  (* proof attempt should be valid *)
  let goal_id = pa.parent in
  let prover = pa.prover in
  let limits =
    { pa.limit with Call_provers.limit_steps = Call_provers.empty_limit.Call_provers.limit_steps }
  in

  let l (l : Call_provers.resource_limit) = l.limit_time /. pa.limit.limit_time < 0.5 in
  Debug.dprintf debug "Bisecting with %a started.@." Whyconf.print_prover prover;
  let t = get_task ses goal_id in
  match Eliminate_definition.bisect_step t with
  | Eliminate_definition.BSdone _ -> assert false
  | Eliminate_definition.BSstep (rem, kont) ->
      let (Cont c) = decide_step ~notification ~removed c goal_id prover limits rem l kont in
      c finalize
