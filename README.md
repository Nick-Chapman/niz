
This Ocaml implementation of the z-machine has been superseded by a [new implementation](https://github.com/Nick-Chapman/zagain) (written in Haskell) which additionally suports static recompilation.

# niz : Nick's Z-machine interpreter (in Ocaml)

At the recent F-sharp conference (https://skillsmatter.com/conferences/8053-f-sharp-exchange-2017), Ross McKinlay presented a talk "YOU WERE EATEN BY A GRUE" where he described his experience of writing an implementation of Infocom's virtual machine, the Z-machine, in F#. This sounded like a fun project for the Easter holiday, and so I decided to write my own Z-machine implementation. But for me the language of choice is Ocaml.

So after way too many hours, I can confirm it was/is a fun project... I followed the Z-machine standards spec 1.1 (http://inform-fiction.org/zmachine/standards) limiting myself (for the moment anyhow) to just supporting version-3 of the Z-machine. Zork was of course the game to motivate and guide my development. You can find zork (you want release 88) and various other .z3 story files here: http://www.resonant.org/games/infocom/games/

UPDATE: now with support for versions: .z1 .z2 .z3 .z4 and .z5

The coding approach I took included:
- functional state (as suggested by the F# guy)
- using types wherever possible to distinguish the different numeric types
- clean separation of the z-code instruction decoder, the z-code evaluator and the interactive terminal

What I ended up with is a basic but working Z-machine interpreter for version-3 (.z3)
- Simple IO. There is currently no readline support (no problem for me: I run my shells under emacs)
- Save and restore works (including default save on C-D and default restore on startup)
- Tools for dumping game text, game dictionary, game object model, disassembling code etc
- Flags to switch on tracing of Z-code execution
- An experimental "-cheat" flag, which explores all single-word commands looking for score-changers
- The code is pretty in some places, but more polish would be nice.

The code for niz is written in Ocaml, making use of Janestreet's core library and standard (ppx) syntax extensions.  Build using `opam` and `dune`. Have fun!
