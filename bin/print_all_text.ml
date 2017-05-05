module Z = Z_machine
let story_file = Sys.argv.(1)
let the_mem = Z.Mem.create ~story_file
module Text = Z.Text.F(struct let the_mem = the_mem end)
let () = Text.print_all()
