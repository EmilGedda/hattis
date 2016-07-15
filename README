# ![Screenshot of a failed submission using Hattis](https://emilgedda.se/hattis/failed-submission.png)

# Hattis
Hattis is a fully integrated command line interface to the
[Kattis](https://www.kattis.com/) online coding judge, supporting everything
from submitting solutions to real time progress reporting.  

Hattis makes use of features which only seems to be available to the
KTH-bound version of [Kattis](https://kth.kattis.com/), but anyone can signup
over there nonetheless.

## Features

* Automatic passwordless authentication
* Submission of solutions to Kattis
* Detect used programming language automatically
* Real time progress reporting of a running submission
* Error information from Kattis (if any)
* Compiler output from Kattis (if any)
* Hints about test cases from Kattis (if any)
* Robust error handling
* Fancy UTF-8 glyphs and colored output
* Bash completion support

Hattis should work on all major operating systems. Features such as colors and
glyphs may or may not work on some platforms and their default environments due
to the lack of full UTF-8 and ANSI support in the default terminal emulator,
such as cmd.exe on Windows. All relatively new terminal emulator should have no
trouble with this.

### Future possible features

* man page
* Watch files, and resubmit everytime a file change is detected.
* Verification of Problem ID's
* ZSH completion

## Using 

All options and arguments can be seen using `hattis --help`.
Hattis expects its first parameter to be the kattis problem id, all
parameters after the first one should be the files one wish to submit.

All command line options, for example `--force` (shorthand `-f`) or
`--no-glyphs`, may be positioned anywhere, even before the problem id.

Example: Submitting a file name `hello.c` which resides in the directory `src`
to the kattis problem `hello`.

```
$ hattis hello src/hello.c
```

Example: Forcing submission of a couple of prolog files with a custom location
of kattisrc, to the Kattis problem `kth.progp.s3`.

```
$ hattis -f --conf some/dir/custom-kattisrc.txt kth.progp.s3 src/dfa.pl src/main.pl src/kattio.pl 
```

## Building and installing

I do not ship Hattis binaries as of yet, and the only way to run it on your
system is to compile and install it yourself. Compilation and installation is
handled by [Stack](https://github.com/commercialhaskell/stack) which is a
common buildsystem for Haskell applications. Using Stack is the recommended way
for compiling and install Hattis. It is however possible to build hattis using
`cabal-install` aswell.

### Using Stack

#### Prerequisites

* (Optional) git
* Stack 

Information on how to install Stack can be found [here](http://www.haskellstack.org/).

Do not forget to add `~/.local/bin/` to your `$PATH`, which is where Stack will
install Hattis, and to run `stack setup` after installation of Stack.

First we need to download the source for Hattis, using either git or the zip distributed by GitHub.

```
$ git clone https://github.com/EmilGedda/hattis
$ cd hattis
```

After that, we need we build and install Hattis. `stack build` will download all dependencies and compile them separately. Thus, the building process might take a while depending on your computer.

```
$ stack build
$ stack install
```

And now you are done! You can now use hattis.

```
$ hattis -h
```

### Using cabal install

#### Prerequisites

* (Optional) git
* GHC (>=7.10) 
* cabal-install

Haven't gotten around to write this yet, but Hattis does not need any fancy
compilation steps and could be built and installed as every other cabal
package.
