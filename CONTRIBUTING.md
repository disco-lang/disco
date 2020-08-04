# Contributing to Disco

Thanks so much for taking the time to contribute to Disco!  All
contributions are welcomed and appreciated, all the way from reporting
a bug or fixing a single punctuation mark in the documentation, up to
implementing a complex new feature.

The following is a set of guidelines for contributing to Disco and
related websites and tools, which are hosted in the [disco-lang
organization](https://github.com/disco-lang) on GitHub. These are
mostly guidelines, not rules.  In particular, you don't need to worry
that your contribution will be ignored or rejected in some way if you
don't follow the guidelines. Use your best judgment, and also feel
free to propose changes to this document in a pull request.

## Table Of Contents

[Code of Conduct](#code-of-conduct)

[Quick Links](#quick-links)

[How can I contribute?](#how-can-i-contribute)
  * [Reporting Bugs](#reporting-bugs)
  * [Improving Documentation](#improving-documentation)
  * [Suggesting Enhancements](#suggesting-enhancements)
  * [Your First Code Contribution](#your-first-code-contribution)
  * [Development Workflow](#development-workflow)
  * [Pull Requests](#pull-requests)

[Style guides](#styleguides)

[I have push access to the Disco repository, now what?](#i-have-push-access-to-the-disco-repository-now-what)

## Code of Conduct

The Disco project and everyone participating in it is governed by the
[Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md). By
participating, you are expected to uphold this code. Please report
unacceptable behavior to [byorgey@gmail.com](mailto:byorgey@gmail.com).

## Quick Links

* Bug to report or feature to request? Try the [GitHub issue
  tracker](https://github.com/disco-lang/disco/issues).
* Questions? Join the [`#disco-lang` IRC channel on
  `freenode.net`](https://webchat.freenode.net/#disco-lang).

## How can I contribute?

There are lots of ways to contribute to Disco—regardless of your
background knowledge or level of mathematical or programming skill,
there is probably a way you can help.  For general advice on
contributing to open source projects like Disco, see [How to
Contribute to Open
Source](https://opensource.guide/how-to-contribute/), or [Your first
open source contribution: a step-by-step technical
guide](https://medium.com/@jenweber/your-first-open-source-contribution-a-step-by-step-technical-guide-d3aca55cc5a6).
The rest of this section walks through specific ways you can
contribute and instructions for how to do so.

### Reporting Bugs

Even if you're not at the point where you would feel comfortable
contributing code or documentation, playing around with the language
and reporting any problems that you find is an immensely valuable way to
help make the language better.

Feel free to [submit a bug
report](https://github.com/disco-lang/disco/issues) for anything that
seems like it's not the way it should be, such as a typo or error in
the documentation, a program that yields an error even though you
think it should be accepted, or a program that seems to output the
wrong result.  Even if the error turns out to be in your understanding
rather than in the documentation or code, it's still a valuable
contribution, since it may point out a way that the documentation or
interface could be improved to help reduce similar confusion in the
future.

Bugs are tracked as [GitHub
issues](https://github.com/disco-lang/disco/issues).  Before creating
a new issue, you can [do a quick
search](https://github.com/search?q=+is%3Aissue+user%3Adisco-lang) to
see if the problem has already been reported.  If it has and the issue
is still open, feel free add a comment to the existing issue instead
of opening a new one.

When creating a new issue, explain the problem and include additional
details to help the maintainers reproduce the problem, for example:

* **Use a clear and descriptive title** for the issue to identify the
  problem, if you can.
* **Provide specific examples to demonstrate the problem**. In many
  cases this may be in the form of some specific Disco code which
  causes the problem to happen.  To include some Disco code in the
  issue, you can use [Markdown code
  blocks](https://help.github.com/articles/markdown-basics/#multiple-lines).
* **Describe the behavior you observed after following the steps** and
  point out what exactly is the problem with that behavior.
* **Explain which behavior you expected to see instead and why.**

Not all of these are necessarily required for every single bug report
(for example, to report a typo in the documentation you don't need to
do much beyond point it out); use your judgment, but try to err on the
side of including more information.

### Suggesting Enhancements

If you have an idea for a way Disco could be better, or a super-cool
new feature you'd like to see, you can [submit it on the GitHub issue
tracker](https://github.com/disco-lang/disco/issues), the same as a
bug report.  Just describe your idea in as much detail as you can.

### Improving Documentation

Disco has two main types of documentation:

1. Documentation [hosted on
   `readthedocs.io`](https://disco-lang.readthedocs.io/en/latest/),
   aimed at users of Disco (students, teachers, *etc.*).  The source
   for this documentation can be found in the [`docs/`
   directory](https://github.com/disco-lang/disco/tree/master/docs).
   Right now there is only a tutorial for experienced functional
   programmers, but more documents are planned, such as a language
   reference and a tutorial for beginning programmers; you may have
   ideas for other sorts of helpful documents!  You can contribute to
   an existing document or even start a new one.  For more information
   about the technical details of contributing to the documentation,
   see [docs/README.md](docs/README.md).

2. [Haddock](https://www.haskell.org/haddock/) documentation in the
   code, which is aimed at developers and those trying to understand
   the implementation of Disco.  If you know Haskell, one valuable way
   to contribute is by reading and trying to understand the code (feel
   free to ask questions in the [`#disco-lang` IRC channel on
   `freenode.net`](https://webchat.freenode.net/#disco-lang) along the
   way!), and adding comments to document what you learn, especially
   in places where the comments are inadequate.

Specific documentation that needs fixing, or specific documentation
that needs to be written, can also be found [on the issue tracker under the
"Documentation"
label](https://github.com/disco-lang/disco/labels/Z-Documentation)
(though as explained above, the issues on the issue tracker are by no
means an exhaustive list of documentation improvements that would be
helpful).

### Making a Code Contribution

Disco is written in [Haskell](http://haskell.org).  The level of
Haskell skill needed to understand and contribute to the Disco
codebase varies widely depending on which part of the code you look
at, but generally speaking you will probably need to be comfortable
with standard monads (`Except`, `Reader`, `State`, `Maybe`) and with
standard container types like `Data.Set` and `Data.Map`.  If you'd
like to learn enough Haskell to contribute to Disco, we recommend
starting by [working through these introductory Haskell course
materials](https://www.cis.upenn.edu/~cis194/spring13/).

If you'd like to contribute some code but are unsure where to begin,
you can start by looking through [issues tagged "Low-Hanging
Fruit"](https://github.com/disco-lang/disco/labels/C-Low%20Hanging%20Fruit)
in the issue tracker.  These are bugs and features which should be
appropriate for someone just getting started to tackle.  If you want
help understanding or getting started on a particular issue, feel free
to leave a comment on the issue or ask in the [`#disco-lang` IRC
channel](https://webchat.freenode.net/#disco-lang).

### Development Workflow

Never made an open source contribution before? Wondering how
making a contribution actually works? Here's a quick rundown!

1. Find an issue that you are interested in addressing or a feature
   that you would like to add.
1. *Fork* [the `disco` repository](https://github.com/disco-lang/disco)
   (by clicking the "Fork" button in the upper-right corner).  This
   will make a copy of the repository in your personal GitHub account,
   that is, you will have your own personal copy of the repository
   under `<your-GitHub-username>/disco`.
1. *Clone* the repository to your local machine by opening a terminal,
   navigating to the directory where you would like to store the
   `disco` repository, and typing `git clone
   https://github.com/your-GitHub-username/disco.git`.  (If you don't
   already have the `git` tool, you will have to [install it
   first](https://git-scm.com/downloads).) You should now have a
   subfolder named `disco` containing an up-to-date copy of the
   repository.
1. Create a new *branch* for your fix using `git checkout -b
   BRANCH-NAME` (replace `BRANCH-NAME` by some appropriate name based on
   the feature or fix you plan to make).
1. Make the appropriate changes for the issue you are trying to
   address or the feature that you want to add.
1. Use `git add` to add the file contents of the changed files to the
   "snapshot" git uses to manage the state of the project, also known
   as the index.
1. Use `git commit -m "Insert a short message of the changes made
   here"` to store the contents of the index with a descriptive
   message.
1. Push the changes to your fork on GitHub using `git push origin BRANCH-NAME`.
1. [Submit a pull
   request](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork)
   from your fork to the `disco-lang/disco` repository.
1. Title the pull request with a short description of the changes made
   and the issue or bug number associated with your change (if
   any). For example, you can title an issue like so "Added more log
   output to resolve #4352".
1. In the description of the pull request, explain the changes that
   you made, any issues you think exist with the pull request you
   made, and any questions you have for the maintainer. It's OK if
   your pull request is not perfect (no pull request is), the reviewer
   will be able to help you fix any problems and improve it!
1. Wait for the pull request to be reviewed by a maintainer.
1. Make changes to the pull request if the reviewing maintainer
   recommends them.  You can simply make additional changes, `add` and
   `commit` them, and then `push` to the same branch.  Any additional
   commits added to the same branch will automatically be included in
   the pull request.
1. Celebrate your success after your pull request is merged!
1. After your first pull request is merged, it is very likely that you
   will be granted push access to the `disco-lang/disco` repository.  This means
   that from now on you can create branches directly within the
   `disco` repository rather than working in your own fork.  For more
   information, see [I have push access to the Disco repository, now what?](#i-have-push-access-to-the-disco-repository-now-what).

## I have push access to the Disco repository, now what?

Inspired by [Edward
Kmett](https://www.reddit.com/r/haskell/comments/5n61uh/an_interview_with_haskell_developer_edward_kmett/dc92hcc/?context=8&depth=9),
Disco uses [The Pull Request
Hack](https://felixge.de/2013/03/11/the-pull-request-hack.html):
anyone who submits a pull request to Disco is likely to be granted
push access in short order.

One benefit is that when working on a new feature, you can create a
branch in the `disco` repository itself (instead of in your own fork)
and then open a pull request from the feature branch to `master`.
This is actually *preferred* since it makes several things smoother
(for example, the `restyled.io` bot works much better on pulls from
local branches than from forks).

Although being given push access does not imply any particular
responsibility, you are welcome to do things such as help review and
merge other pull requests, and help triage, label, and update issues
in the issue tracker.

Having push access also means, of course, that you can push directly
to `master`.  You are welcome to do so for typos, small fixes,
documentation improvements, and the like; for larger fixes, new
features, *etc.* opening a pull request from a feature branch is still
preferred, to give a chance for others to offer suggestions for
improvement.

<!-- ### Pull Requests -->

<!-- The process described here has several goals: -->

<!-- - Maintain Atom's quality -->
<!-- - Fix problems that are important to users -->
<!-- - Engage the community in working toward the best possible Atom -->
<!-- - Enable a sustainable system for Atom's maintainers to review contributions -->

<!-- Please follow these steps to have your contribution considered by the maintainers: -->

<!-- 1. Follow all instructions in [the template](PULL_REQUEST_TEMPLATE.md) -->
<!-- 2. Follow the [styleguides](#styleguides) -->
<!-- 3. After you submit your pull request, verify that all [status checks](https://help.github.com/articles/about-status-checks/) are passing <details><summary>What if the status checks are failing?</summary>If a status check is failing, and you believe that the failure is unrelated to your change, please leave a comment on the pull request explaining why you believe the failure is unrelated. A maintainer will re-run the status check for you. If we conclude that the failure was a false positive, then we will open an issue to track that problem with our status check suite.</details> -->

<!-- While the prerequisites above must be satisfied prior to having your pull request reviewed, the reviewer(s) may ask you to complete additional design work, tests, or other changes before your pull request can be ultimately accepted. -->

<!-- ## Style Guides -->

<!-- ### Git Commit Messages -->

<!-- <\!-- * Use the present tense ("Add feature" not "Added feature") -\-> -->
<!-- <\!-- * Use the imperative mood ("Move cursor to..." not "Moves cursor to...") -\-> -->
<!-- <\!-- * Limit the first line to 72 characters or less -\-> -->
<!-- <\!-- * Reference issues and pull requests liberally after the first line -\-> -->
<!-- <\!-- * When only changing documentation, include `[ci skip]` in the commit title. -\-> -->

<!-- ### Documentation -->



<!-- ### Haskell -->


