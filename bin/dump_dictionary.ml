module Z = Z_machine
let story_file = Sys.argv.(1)
let () = Z.Dictionary.dump (Z.Mem.create ~story_file)
