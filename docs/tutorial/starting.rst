
***************
Getting started
***************

After you have been added as a collaborator to the `disco github
repository`_, get the source code via SSH:

.. _`disco github repository`: https://github.com/disco-lang/disco

::

    git clone git@github.com:disco-lang/disco.git

If you are not a collaborator on the repository you can also get the
source code via HTTPS:

::

    git clone https://github.com/disco-lang/disco.git

Make sure you have `the stack tool`_ installed.  Then navigate to
the root directory of the disco repository, and execute

.. _`the stack tool`: https://docs.haskellstack.org/en/stable/README/

::

    stack setup
    stack build --fast

(This may take quite a while the first time, while ``stack`` downloads
and builds all the dependencies of ``disco``.)

After building disco with ``stack build``, to run the disco REPL
(Read-Eval-Print Loop), type ``stack exec disco`` at a command prompt.
You should see a disco prompt that looks like this:

::

    Disco>

To run the test suite, you can execute

::

   stack test --fast

See the ``build`` script in the root of the repository for an example of
additional arguments you may wish to pass to ``stack build`` for a tight
edit-compile-test loop while working on the code.
