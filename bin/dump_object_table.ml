module Z = Z_machine
let story_file = Sys.argv.(1)
let () = Z.Object_table.dump (Z.Mem.create ~story_file)
