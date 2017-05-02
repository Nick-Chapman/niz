module Z = Z_machine
let file = Sys.argv.(1)
let () = Z.Dictionary.dump (Z.Mem.create ~file)
