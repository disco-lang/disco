* 0.1.5 (12 March 2022)

  - Syntax errors in function definitions now result in much more
    helpful error messages.  Before, it would just complain about
    something being wrong with the '=' sign.
    ([#346](https://github.com/disco-lang/disco/issues/346))

* 0.1.4 (10 March 2022)

  - New features or syntax
      - `if` and `when` are now synonyms

  - Bug fixes
      - Only allow constructing sets and bags with element types
        that can be compared ([#306](https://github.com/disco-lang/disco/issues/306))

  - UI
      - Test results are now printed in the same order in which
        they were declared

* 0.1.3.1 (5 March 2022)

  - Fix [#340](https://github.com/disco-lang/disco/issues/340): disco
    no longer crashes when encountering an undefined name at runtime,
    but prints a nice error message instead.

* 0.1.3.0 (3 March 2022)

  - New features or syntax
      - `∈` now works as a synonym for `elem`
      - add `→` and `↔` as synonyms for `->`, `<->`

  - Additional documentation for `|~|`, `power`

  - UI
      - `--help`, `--version`, and the welcome message now all report
        the current version number
      - Typechecking errors now report the name of the thing being
        typechecked when the failure occurred
      - The values of top-level expressions are now printed when
        `:load`ing

* 0.1.2.0 (12 February 2022)

  - New features or syntax
      - Add `String` type synonym
      - New `iff` / `<->` operator
      - New Cartesian product operator `><`
      - `|~|` notation now denotes container size in addition to
        absolute value

  - Documentation
      - `:doc` command now works on many more things
      - A lot of additional documentation and some improved error
        messages

  - A few small bug fixes related to parsing

* 0.1.1.0 (21 January 2022)

  - `->`, `/\`, `\/` syntax for implies, and, or
  - `truthtable.disco` example
  - Coinductively check user-defined types for qualifiers (#317)
  - Additional documentation

* 0.1.0.0 (17 January 2022): initial release
