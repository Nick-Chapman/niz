module Z = Z_machine
let file = Sys.argv.(1)
let the_mem = Z.Mem.create ~file
module I_decoder = Z.I_decoder.F(struct let the_mem = the_mem end)
let () = I_decoder.disassemble_all()
