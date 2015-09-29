ghcjs-debian
============

This repository contains debian packaging of the bootstrapped ghcjs
compiler.  Installing the resulting ghcjs debian package produces a
fully functioning ghcjs system.

Three other packages are build dependencies of ghcjs and part of the
ghcjs project.  Debianizations are provided here.

  * Cabal (modified for ghcjs)
  * cabal-install (modified for ghcjs)
  * ghcjs-prim

Depending on your OS version, it may be necessary to build other
packages such as nodejs from sid.
