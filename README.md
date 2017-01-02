Crystallize
===========

Scala implementation of LEIA: Library for <b>L</b>earning <b>E</b>xpectations <b>I</b>ncrementally and <b>A</b>utonomously.

See the [LEIA web site][LEIA].

  [LEIA]: <http://leialearns.org> "LEIA"

The development of Crystallize is an attempt to address the complexity issues of earlier implementations of LEIA (and its
predecessor LExAu).

Where the Java implementation of LEIA is based on mutable objects that needs synchronized updates, Crystallize is
based on immutable objects. When the model needs to be updated, a layer of slightly modified objects is added to the
previous model, which results in a new model. Processes that are still evaluating the old model can continue their
work unimpeded. In this way, many processes that produce updated versions of the model itself, as well as derived
models can operate in parallel.

This library is still in its infancy. If you want to try it, running `src/main/script/file-example.sh` is probably a
good start.