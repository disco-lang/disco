[![Build Status](https://travis-ci.org/disco-lang/disco.svg?branch=master)](https://travis-ci.org/disco-lang/disco)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)

Disco is a programming language intended to teach basic functional
programming principles in the context of a discrete mathematics
course.

Using Disco on replit.com
---------------------------

If you just want to *use* disco (*i.e.* if you are a student, or just
checking out the language), the recommended way is to use it via
`replit.com`.  Simply [visit this
REPL](https://replit.com/@BrentYorgey/Disco#README.md) and follow the
instructions there to fork your own copy, where you will be able to
evaluate Disco expressions, and edit and run your own `.disco` files,
all via your web browser, without installing anything on your computer.

Installation
------------

If for some reasons you want to actually install `disco` on your
computer, follow the below instructions.  (If you want to *contribute*
to disco development, you should skip to the instructions about
building with stack.)

**Note**, if you are a student, you should **not** need to do this!
The above instructions about using `disco` on `replit.com` should be
all you need.  The below instructions are kept here for completeness.

- Follow the instructions to [install
  ghcup](https://www.haskell.org/ghcup/) by opening a terminal or
  command prompt and copy-pasting the given installation command.  You
  can just accept all the defaults.  If you don't have [Windows
  Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/)
  (if you don't know what that is, then you don't have it), see the
  [instructions here](https://www.haskell.org/ghcup/install/) for a
  PowerShell command to run.
    - If you use PowerShell, note that after running the magic
      PowerShell command to set up `ghcup`, you need to close and
      reopen PowerShell in order for it to recognize the `cabal`
      command.

- Run `cabal update`, which will download the latest information about
  Haskell packages.

- Now run `cabal install disco` at a command prompt.

    - Note that this may take a very long time, on the order of an
      hour or so.
    - The good news is that most of this work only needs to be done
      once, even if you later install an updated version of disco.
      Even if installation fails partway through, the work already
      completed up to that point need not be redone.
    - On OSX, if building fails with an error like `ghc: could not
      execute: opt`, it means you need to install LLVM.  The easiest
      way to do this is to first [follow the instructions to install
      Homebrew](https://brew.sh/) (if you don't already have it), and
      then type

          brew install llvm

      at a terminal prompt.

        - If this fails with an error like `Could not resolve HEAD to
          a revision`, then try running these two commands at a
          terminal prompt:

              rm -rf $(brew --repo homebrew/core)
              brew tap homebrew/core

          Then re-run the `brew install llvm` command.

        - After installing `llvm`, you may need to close and re-open
          the terminal before running `cabal install disco` again.

- If it works, you should be able to now type `disco` at a command
  prompt, which should display a message like this:

    ```
    Welcome to Disco!

    A language for programming discrete mathematics.

    Disco>
    ```

- If installation seems like it succeeded but the `disco` command is
  not recognized, it may be an issue with your path environment
  variable settings.  Try running `disco` using an explicit path:
    - `~/.cabal/bin/disco` on Linux or OSX
    - `C:\cabal\bin\disco` on Windows
    - If those don't work, poke around and see if you can figure
      out where the `cabal/bin` folder is on your computer, and
      run `disco` from there.
    - If you wish, you may add the `cabal/bin` folder (wherever it is
      located) to your `Path` (Windows) or `PATH` (Linux/OSX)
      environment variable, so that you can run disco simply by typing
      `disco`.  However, this step is optional.

- On Windows, if disco crashes with an error about `foldr` after you
  try to type anything (or if it simply closes the entire window when
  you type anything), the problem is probably that you need to [enable
  UTF-8 mode](https://github.com/disco-lang/disco/issues/253).

    - Open a command prompt, and type

          chcp 65001

    - Now start `disco` as before (by typing `disco` or
      `C:\cabal\bin\disco` or whatever worked).

    - You will have to do this every time you run disco.
      Alternatively, you can create a file called `disco.cmd`
      containing those two commands, for example:

          chcp 65001
          C:\cabal\bin\disco

      Now you can simply double-click on `disco.cmd` to run disco.

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

Feel free to look around, ask questions, etc.  You can also
[contribute](CONTRIBUTING.md)---collaborators are most welcome.

Community
---------

Check out the disco IRC channel, `#disco-lang` on Libera.Chat.  If
you're not familiar with IRC, you can connect via [this web client](https://kiwiirc.com/nextclient/irc.libera.chat/?nick=Guest?#disco-lang).

Documentation
-------------

Documentation is [hosted on
readthedocs.io](http://disco-lang.readthedocs.io/en/latest/).

Contributing
------------

If you'd like to contribute to disco development, check out
[CONTRIBUTING.md](CONTRIBUTING.md).

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
