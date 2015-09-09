# What is it? #

`rox6read` is a command line tool to read rides (and also settings, totals, etc.) from Sigma ROX6 cycling computer and save them as TCX files.

It may or may not work, it may brick your ROX6 (but it seems unlikely, as it doesn't try to write/save/change anything). The code may be rough and of draft quality. The protocol was recreated by reverse engineering Sigma Data Center.

It wouldn't be developed any further, as the author switched to Garmin Edge, which is much less pain in the ass to get data from on non-Windows.

# How to build it? #

It depends on the following OCaml packages:

* [batteries](http://batteries.forge.ocamlcore.org/) (≥ 2.2.0)
* [tcx](https://bitbucket.org/a12n/ocaml-tcx/) (≥ 0.25.0)

The easiest way to install the dependencies is by means of [OPAM](https://opam.ocaml.org/):

```
#!sh
$ opam init
$ eval $(opam config env)
$ opam install batteries tcx
```

Then run usual `make`. The program will be `rox6read.native`.