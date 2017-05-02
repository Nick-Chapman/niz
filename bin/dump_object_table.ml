module Z = Z_machine
let file = Sys.argv.(1)
let () = Z.Object_table.dump (Z.Mem.create ~file)
