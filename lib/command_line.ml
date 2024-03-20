open Core
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
  let no_buffer =
    step (fun m x -> m ~no_buffer:x)
    +> flag "no-buffer" no_arg
      ~doc:" print game output without any buffering."
  in
  let no_line_wrap =
    step (fun m x -> m ~no_line_wrap:x)
    +> flag "no-line-wrap" no_arg
      ~doc:" print game output without any line-wrap."
  in
  let hide_unimplemented =
    step (fun m x -> m ~hide_unimplemented:x)
    +> flag "hide-unimplemented" no_arg
      ~doc:" don't show unimplemented ops (i.e. screen/font control)."
  in
  let story_file =
    step (fun m x -> m ~story_file:x)
    +> anon ("story-file" %: string)
  in
  Command.basic_spec ~summary:"Nick's Z-machine interpreter"
    (
      empty
      ++ trace1
      ++ trace2
      ++ trace9
      ++ tandy
      ++ cheat
      ++ story_file
      ++ no_buffer
      ++ no_line_wrap
      ++ hide_unimplemented
    )
    (fun ~trace1 ~trace2 ~trace9 ~tandy ~cheat ~story_file 
      ~no_buffer 
      ~no_line_wrap
      ~hide_unimplemented
      () ->
      let trace = 
	if trace9 then 9 
	else if trace2 then 2 
	else if trace1 then 1 
	else 0
      in
      let options = {
	Options. 
	trace; tandy; cheat; 
	no_buffer; 
	no_line_wrap;
	hide_unimplemented;
      } in
      let 
	module Interactive = 
	  Interactive.F(struct 
	    let story_file = story_file 
	    let options = options
	  end) 
      in
      Interactive.run ())
  |> Command_unix.run
