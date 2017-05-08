open Z_machine
let story_file = Sys.argv.(1)
let mem = Mem.create ~story_file
let zversion = Mem.zversion mem
module Object_table = Object_table.F(struct let zversion = zversion end)
let () = Object_table.dump mem
