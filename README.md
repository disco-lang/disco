[![Build Status](https://travis-ci.org/disco-lang/disco.svg?branch=master)](https://travis-ci.org/disco-lang/disco)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)

Disco is a programming language intended to teach basic functional
programming principles in the context of a discrete mathematics
course.

Installation
------------

If you just want to *use* disco (*i.e.* if you are a student), follow
these instructions.  If you want to *contribute* to disco development,
you should skip to the instructions below about building with stack.

- Follow the instructions to [install
  ghcup](https://www.haskell.org/ghcup/) by opening a terminal or
  command prompt and copy-pasting the given installation command.  You
  can just accept all the defaults.  If you don't have [Windows
  Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/)
  (if you don't know what that is, then you don't have it), see the
  [instructions here](https://www.haskell.org/ghcup/install/) for a
  PowerShell command to run.
- Now run `cabal install disco` at a command prompt.

If you encounter any difficulties, please let me know --- either come
talk to me or [open a GitHub
issue](https://github.com/disco-lang/disco/issues/new).  These
instructions will be kept up-to-date with whatever helpful tips or
workarounds I learn. So even if you encounter a difficulty but figure
out the solution youself, let me know --- that way I can include the
problem and solution here so others can benefit!

Design principles
-----------------

* Includes those features, and *only* those features, useful in the
  context of a discrete math course. This is *not* intended to be a
  general-purpose language.
* Syntax is as close to standard *mathematical* practice as possible,
  to make it easier for mathematicians to pick up, and to reduce as
  much as possible the incongruity between the language and the
  mathematics being explored and modeled.
* Tooling, error messages, etc. are very important---the language
  needs to be accessible to undergrads with no prior programming
  experience. (However, this principle is, as of yet, only
  that---there is no tooling or nice error messages to speak of.)

Feel free to look around, ask questions, etc.  You can even contribute
some code if you like---collaborators are most welcome.

Community
---------

Check out the disco IRC channel, `#disco-lang` on Libera.Chat.  If
you're not familiar with IRC, you can connect via [this web client](https://kiwiirc.com/nextclient/irc.libera.chat/?nick=Guest?#disco-lang).

Documentation
-------------

Documentation is [hosted on
readthedocs.io](http://disco-lang.readthedocs.io/en/latest/).

Building with stack
-------------------

First, make sure you have
[the `stack` tool](https://docs.haskellstack.org/en/stable/README/)
(the easiest way to install it is via [ghcup](https://www.haskell.org/ghcup/)).
Then open a command prompt, navigate to the root directory of this
repository, and execute

```
stack build
```

After this completes, you should be able to

```
stack exec disco
```

to run the Disco command-line REPL.

While developing, you may want to use a command like

```
stack test --fast --file-watch --ghc-options='-Wall'
```

which will turn on warnings, turn off optimizations for a faster
edit-compile-test cycle, and automatically recompile and run the test
suite every time a source file changes.

