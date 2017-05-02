
# niz : Nick's Z-machine interpreter


At the recent F-sharp conference (https://skillsmatter.com/conferences/8053-f-sharp-exchange-2017), Ross McKinlay presented a talk "YOU WERE EATEN BY A GRUE" where he described his experience of writing an implementation of Infocom's virtual machine, the Z-machine, in F#. This sounded like a fun project for the Easter holiday, and so I decided to write my own Z-machine implementation. But for me the language of choice is Ocaml.


So after way too many hours, I can confirm it was/is a fun project... I followed the Z-machine standards spec 1.1 (http://inform-fiction.org/zmachine/standards) limiting myself (for the moment anyhow) to just supporting version-3 of the Z-machine. Zork was of course the game to motivate and guide my development. You can find zork (you want release 88) and various other .z3 story files here: http://www.resonant.org/games/infocom/games/


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
- The code is pretty in some places, but more polish would xbe nice.


The code for niz is written in Ocaml, making use of Janestreet's core library and standard (ppx) syntax extensions. The easiest way to access these libraries, and the ocaml compiler itself, is via "opam" (https://opam.ocaml.org) the Ocaml Package Manager. You can use Janestreet's "jbuilder" to build niz; jbuilder is available via opam, but it's still in beta, and I needed to use the Janestreet bleeding edge library versions to get a successful build. (see the "opam repo add" below).


The following sequence of commands should get you a working niz.exe


$ wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
(The warning "Recommended external solver aspcud not found." seems not to matter)


Download/install ocaml compiler & libraries, and jbuilder...

$ opam switch 4.03.0
$ eval `opam config env`
$ opam repo add janestreet-bleeding https://ocaml.janestreet.com/opam-repository
$ opam update
$ opam install core jbuilder ocaml-migrate-parsetree
$ opam list 


Download & build niz...

$ git clone https://github.com/Nick-Chapman/niz.git
$ cd niz
$ make


Get a story file and play...

$ wget http://www.resonant.org/games/infocom/games/zork1.88-840726
$ _build/default/bin/niz.exe zork1.88-840726 


Have fun!
