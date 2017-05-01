open Core.Std
open Command.Spec

let run() =
  let trace1 = step (fun m x -> m ~trace1:x)
    +> flag "trace1" no_arg
      ~doc:" count execution of Z-machine instructions"
  in
  let trace2 = step (fun m x -> m ~trace2:x)
    +> flag "trace2" no_arg
      ~doc:" trace program counters during Z-machine execution"
  in
  let trace9 = step (fun m x -> m ~trace9:x)
    +> flag "trace" no_arg
      ~doc:" trace execution of Z-machine instructions"
  in
  let tandy =
    step (fun m x -> m ~tandy:x)
    +> flag "tandy" no_arg
      ~doc:" set the tandy bit"
  in
  let cheat =
    step (fun m x -> m ~cheat:x)
    +> flag "cheat" no_arg
      ~doc:" get hints on words which change the score"
  in
  let story_file =
    step (fun m x -> m ~story_file:x)
    +> anon ("story-file" %: string)
  in
  Command.basic ~summary:"Nick's Z-machine interpreter"
    (
      empty
      ++ trace1
      ++ trace2
      ++ trace9
      ++ tandy
      ++ cheat
      ++ story_file
    )
    (fun ~trace1 ~trace2 ~trace9 ~tandy ~cheat ~story_file () ->
      let trace = 
	if trace9 then 9 
	else if trace2 then 2 
	else if trace1 then 1 
	else 0
      in
      let options = {Options. trace; tandy; cheat;} in
      Interactive.run options ~story_file ())
    
  |> Command.run
