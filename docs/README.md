Documentation is built using
[Sphinx](https://www.sphinx-doc.org/en/master/) and hosted on
[readthedocs.io](htts://readthedocs.io).

Contributing to the documentation can be done in the same way as
contributing to any other part of the project.  Commits pushed to the
`master` branch on GitHub are automatically built and hosted at [https://disco-lang.readthedocs/io](https://disco-lang.readthedocs/io).

To build the documentation locally, first install Sphinx and the
Sphinx theme for readthedocs with

    pip install sphinx sphinx_rtd_theme

Then you should be able to build with `make`:

    cd docs
    make html

Now point your browser at `docs/_build/html/index.html` to view.

When contributing to the documentation, you are highly encouraged to
build it locally to make sure it looks good before pushing/opening a
pull request/etc.  However, if you are unable to, it's not absolutely
necessary.

Documentation is written using
[reStructuredText](https://docutils.sourceforge.io/rst.html). See the
linked documentation for information, including tutorials and
reference material; or just look at some existing Disco documentation
and copy the syntax!
