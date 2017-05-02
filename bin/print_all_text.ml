module Z = Z_machine
let file = Sys.argv.(1)
let the_mem = Z.Mem.create ~file
module Text = Z.Text.F(struct let the_mem = the_mem end)
let () = Text.print_all()
