module Z = Z_machine
let story_file = Sys.argv.(1)
let the_mem = Z.Mem.create ~story_file
module I_decoder = Z.I_decoder.F(struct let the_mem = the_mem end)
let () = I_decoder.disassemble_all()
