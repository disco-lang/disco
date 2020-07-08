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

### Reporting Bugs

Even if you're not at the point where you would feel comfortable
contributing code or documentation, playing around with the language
and reporting any problems that you find is a super valuable way to
help make the language better.

Feel free to [submit a bug
report](https://github.com/disco-lang/disco/issues) for anything that
seems like it's not the way it should be, such as a typo or error in
the documentation, a program that yields an error even though you
think it should be accepted, or a program that seems to output the
wrong result.  Even if the error turns out to be in your understanding
rather than in the documentation or code, it's still a valuable
contribution since it may point out a way that the documentation or
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
   the implementation of Disco.  One valuable way to contribute is by
   reading and trying to understand the code, and adding additional
   comments to document your understanding in places where the
   comments are inadequate.

### Your First Contribution

Unsure where to begin contributing to Disco? You can start by looking
through [issues tagged "Low-hanging
fruit"][https://github.com/disco-lang/disco/issues?q=is%3Aissue+is%3Aopen+label%3A%22C-Low+Hanging+Fruit%22]
in the issue tracker.  These are bugs and features which should be
appropriate for someone just getting started to tackle.  If you want
help understanding or getting started on a particular issue, feel free
to leave a comment on the issue, or ask in the [`#disco-lang` IRC
channel](https://webchat.freenode.net/#disco-lang).

### Development Workflow



### Pull Requests

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

## Style Guides

### Git Commit Messages

* Use the present tense ("Add feature" not "Added feature")
* Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
* Limit the first line to 72 characters or less
* Reference issues and pull requests liberally after the first line
* When only changing documentation, include `[ci skip]` in the commit title.

### Documentation



### Haskell

## I have push access to the Disco repository, now what?

