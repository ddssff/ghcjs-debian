ghcjs-debian
============

This repository contains debian packaging of the bootstrapped ghcjs
compiler.  It uses ghcjs-tools
(https://github.com/ddssff/ghcjs-tools-debian) as a build dependency
and packages all the compiled standard libraries.  Installing the
resulting ghcjs debian package produces in a fully functioning ghcjs
system.

You will also need to build a new version of the Cabal library using
the debianization in cabal-ghcjs-debin, and a new version of cabal-install
using the debianization in cabal-install-ghcjs-debian.
