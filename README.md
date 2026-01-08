[![Build Status](https://github.com/disco-lang/disco/actions/workflows/haskell-ci.yml/badge.svg](https://github.com/disco-lang/disco/actions)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](CODE_OF_CONDUCT.md)

Disco is a programming language intended to teach basic functional
programming principles in the context of a discrete mathematics
course.

**Visit [Disco Live](https://disco-lang.github.io/disco-live/) to use
Disco directly in your browser without installing anything!**

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

Building disco
--------------

Scroll down to find the instructions appropriate to your operating system.  **Note,
if you are a student using Disco for a class, you should not need
these instructions!**  You should be able to use [Disco
Live](https://disco-lang.github.io/disco-live/) in your browser.

Building disco on Windows
-------------------------

- Open a PowerShell terminal.

- Run the command `wsl --install`.

- Now type `wsl` to open a Windows Subsystem for Linux prompt.

- Paste this command:

  ```
  sudo apt install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config
  ```

- Paste the following command:

  ```
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  ```

- Keep hitting Enter to accept the defaults.

- Once it finishes, type the following commands:

  ```
  sudo apt install zlib1g-dev
  cabal update
  cabal install disco
  ```

- If it works, you should be able to now type `disco` at a command
  prompt, which should display a message like this:

    ```
    Welcome to Disco!

    A language for programming discrete mathematics.

    Disco>
    ```

- See the [Troubleshooting](#troubleshooting) section below if you
  have any issues.

Building disco on Mac OS / Linux
--------------------------------

- Open a terminal.

- Paste the following command:

  ```
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  ```

- Keep hitting Enter to accept the defaults.

- Now run the following:

  ```
  cabal update
  cabal install disco
  ```

- If building fails with an error like `ghc: could not
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

- See the [Troubleshooting](#troubleshooting) section below if you
  have any issues.

Troubleshooting
---------------

- If installation seems like it succeeded but the `disco` command is
  not recognized, it may be an issue with your path environment
  variable settings.  Try running `disco` using an explicit path:
    - `~/.cabal/bin/disco` on Linux, OSX, or WSL2
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
