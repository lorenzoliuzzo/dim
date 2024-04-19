# [Package]
version       = "0.1.0"
author        = "lorenzoliuzzo"
description   = "Dimensional Analysis in Nim"
license       = "GPL-3.0-or-later"
installDirs   = @["dim"]

# Dependencies
requires "nim >= 2.0.2"

task examples, "Build examples":
    withDir "examples":
        exec "nim c -d:release -p:~/.nimble/pkgs2 main.nim"