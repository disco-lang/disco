This directory contains a regression test suite for Disco.

Running the tests
-----------------

To run the tests, just do `stack test`.

Adding a new test case
----------------------

Adding a new test case is easy.

1. Create a directory for the new test, with a prefix denoting the
   general category of the test, and a three-digit number sequentially
   after the highest existing number in that category.

2. In that directory, you must create two files:

    - `input` should consist of a sequence of commands or expressions
      to be evaluated by the Disco REPL, one per line.

    - `expected` should consist of the expected output.

    In fact, you don't even have to create `expected` yourself.  If
    you know that Disco currently has the expected behavior for the
    commands and expressions in `input`, simply run the test suite and
    `expected` will be created automatically if it does not exist.

    You may create additional files as well, for example, one or more
    `.disco` files to be `:load`ed by a command in `input`.  (Be aware
    that the test suite runs from the root directory of the
    repository, so you will have to write something like `:load
    test/category099/foo.disco`.)

Dealing with mass test suite breakage
-------------------------------------

In certain cases many test cases may break all at once for a known
reason---for example, if a change in Disco's pretty-printer or
error messages causes the expected output of many tests to change.  In
this case you need not manually paste in the new expected output for
each test case.

1. Verify by inspection that all the failing test cases are in fact
   producing the expected new output (by examining the `output` files).

2. Run `stack test --test-arguments --accept`.  This will overwrite
   the `expected` files with the actual output.
