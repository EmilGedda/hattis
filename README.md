# ![Screenshot of Hattis](https://emilgedda.se/hattis/failed-submission.png)

# Hattis

Hattis is a fully integrated command line interface to the
[Kattis](https://www.kattis.com/) online coding judge. 
Kattis hosts a large amount of programming challenges, which are free for everyone to try.
Hattis takes kattis to the command line, while still supporting everything from 
submitting solutions to real time progress reporting. 

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
problem with this.

### Possible future features

* man page
* Watch files, and resubmit everytime a file change is detected.
* Verification of Problem ID's
* ZSH completion

## Getting started

Hattis makes use of token based authentication to be able to provide a password-less login to kattis.
This means that in order to use Hattis you need to download your own custom configuration file called _kattisrc_.
A _kattisrc_ is a **personal** configuration file, and can be downloaded from [kattis](https://kth.kattis.com/download/kattisrc),
you need to be logged in into kattis to download the file.
By default _kattisrc_ contains all the information needed by Hattis to fully function.

To be able to find _kattisrc_, Hattis makes use of the XDG specification and some environment variables. 
Hattis will check these two places for the _kattisrc_, in the given order.

* `$XDG_CONFIG_HOME/hattis/kattisrc`  (where _kattisrc_ is the file, not a directory).
* `$HOME/.config/hattis/kattisrc`

If the file wasn't found, Hattis will say that it was unable to locate _kattisrc_, and then exit.
A custom _kattisrc_ location can be specified using the `--conf` (shorthand `-c`) option.

## Usage and examples  

Hattis expects its first parameter to be the kattis problem id, all
parameters after the first one should be the files one wish to submit.

All command line options, for example `--force` (shorthand `-f`) or
`--no-glyphs`, may be positioned anywhere, even before the problem id.

All options and arguments can be seen using `hattis --help`.

Example: Submitting a file name `hello.c` which resides in the directory `src`
to the kattis problem `hello`.

```
$ hattis hello src/hello.c
```

Example: Submittion a solution from an older terminal emulator, disabling colors and fancy glyphs.

```
$ hattis hello src/hello.c --no-colors --no-glyphs
```

Example: Forcing submission of a couple of prolog files with a custom location
of kattisrc, to the Kattis problem `kth.progp.s3`.

```
$ hattis -f --conf some/dir/custom-kattisrc.txt kth.progp.s3 src/dfa.pl src/main.pl src/kattio.pl 
```

## Bash auto completion 

Hattis has built in Bash completion support.

```
$ source <(hattis --bash-completion-script $(which hattis))
```

This will load the completion script into the current bash session, put this in your
`~/.bashrc` to enable autocompletion automatically whenever bash starts.

If you are using _ZSH_, remember to load and execute bashcompinit before sourcing the completion script.

## Using a proxy

A custom proxy can be specified in the `$https_proxy` environment variable and Hattis will tunnel all its traffic through that proxy.
`$https_proxy` is also used by many other applications, for example _wget_ and _cURL_.
Hattis enforces the use of https, and thus will only listen to `$https_proxy` and **not** to `$http_proxy`.

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

Do not forget to add `~/.local/bin/` to your `$PATH` environment variable, which is where Stack will
install Hattis, and to run `stack setup` after installation of Stack.

First we need to download the source for Hattis, using either git or the zip distributed by GitHub.

```
$ git clone https://github.com/EmilGedda/hattis
$ cd hattis
```

After that, we need we build and install Hattis. `stack build` will download all dependencies and compile them separately. 
Thus, the building process might take a while depending on your computer.

```
$ stack build
$ stack install
```

And now you are done! You can now use hattis.

```
$ hattis --help
```

### Using cabal-install

#### Prerequisites

* (Optional) git
* GHC (>=7.10) 
* cabal-install

First we need to download the source for Hattis, using either git or the zip distributed by GitHub.

```
$ git clone https://github.com/EmilGedda/hattis
$ cd hattis
```

After that, we need to initialize a new cabal sandbox, and the dependencies in that sandbox.
The sandbox prevents breakage of existing packages.

```
$ cabal sandbox init 
$ cabal install --only-dependencies
```


After that, we build Hattis.

```
$ cabal configure
$ cabal build 
$ cabal copy
```

Hattis will now be found in `./.cabal-sandbox/bin/` and may be copied to your favorite directory on your `$PATH`.

### License

Hattis is licensed under the BSD 3 license, see LICENSE.
Copyright Emil Gedda, 2016.
