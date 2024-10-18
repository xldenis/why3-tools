open Why3
open Session_itp
open Cmdliner
open Regenerate
open Upgrade
open Stabilize


let stats path =
  (* Required to be able to load a session *)
  let _ = Whyconf.Args.initialize [] (fun _ -> ()) "" in
  let sess = Session_itp.load_session path in

  let stats = ref [] in
  session_iter_proof_attempt
    (fun _ attempt ->
      begin
        match attempt.proof_state with
        | Some result -> begin
            match result.pr_answer with
            | Valid ->
                let theory = find_th sess attempt.parent in
                stats := (theory, result.pr_time, attempt.prover) :: !stats
            | _ -> ()
          end
        | _ -> ()
      end)
    sess;

  let sess_path = get_dir sess in
  List.iter
    (fun (th, t, p) ->
      Format.printf "\"%s\", \"%a\", %.2f, \"%s\"\n" (theory_name th).id_string Whyconf.print_prover
        p t sess_path)
    !stats

let path =
  let docv = "FILE" in
  Arg.(required & pos 0 (some string) None & info [] ~docv)

let stats_command =
  Cmd.v
    (Cmd.info ~doc:"export a CSV of prover times for a session" "csv-export")
    Term.(const stats $ path)

let cmd = Cmd.group (Cmd.info "why3-tool") [ stats_command; regenerate_cmd; upgrade_cmd; stabilize_cmd; z3_cmd ]
let () = exit (Cmd.eval cmd)
