prop
====

The Disco standard :doc:`prop <prop>` library can be :doc:`imported <import>` with

::

   import prop

Currently, it provides only a single function, ``holds : Prop ->
Bool``. The idea of this function is that it will try to tell you *for
sure* whether the given :doc:`proposition <prop>` is true or false.
If it cannot decide it may simply loop forever.  By contrast, using
the ``:test`` :doc:`REPL command <REPL>` is guaranteed to stop but it
may only try some possible inputs and therefore it may not be able to
figure out the correct answer.
