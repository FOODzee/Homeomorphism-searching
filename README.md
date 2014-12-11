Homeomorphism searching
=======================

Determine whether the graph homeomorphism exists and try to build it.


Overview
--------

### Main methods and their returning values.

After checking the necessary conditions by call like

    val (can, msg) = g1.canBeHomeomorphicTo(g2)

`can: Boolean` will contain
* `false` if one of the conditions have been unsatisfied and hence there is no homeomorphism between given graphs;
* `true`  if all necessary conditions satisfied and hence there CAN be some homeomorphism found.


If `can == true` the attempt on finding the homeomorphism can be made by calling

    val (seq, msg) = findHomeomorphism(g1, g2)

`seq: Option[GenSeq[...]]` will contain
* `None` if homeomorphism wasn't found;
* sequence of graphs starting from `G_0_1 == g1`, `G_0_2 == g2` and ended with `G_n_1`, `G_n_2` such, that
  + for each `G_i_k` (0 < i <= n; k = 1, 2) `G_i_k` obtained from `G_(i-1)_k` by subdividing an edge of `G_(i-1)_k`;
  + `G_n_1` is isomorphic to `G_n_2`.


In both calls `msg: String` will contain some information message.


### Isomorphism checking.

Being an important part of algorithm, and, on the other side, a good-studied problem,
the check of isomorphism of graphs is performed by [nauty][1] -- a set of software written in C.

In order to use its functionality the native library `libnauty-scala-interface.so` have been created in C-side
and graph representation conversion -- in Scala-side.

The compilation-tips on `.so` can be found in `nauty-scala-inteface/main.c`.
Pay attention to the location of compiled lib.

I suppose that it could be used by other Scala programmers so maybe there's a point to substrate it to separate hub.


TODO
----

* There is no unit-test and that is very-very bad. So it need to be written.
* The homeomorphism-building algorithm is quite dumb, it could be extended/rewritten to be faster and more accurate.
* Make `libnauty-scala-interface.so` visible from Scala in a more accurate way (m.b. like [here][3]).
* Make it cross-platform (it'd be easy after finishing previous step).
* Make [nauty][1] be a part of [Scala Graph][2] (dreams, or dreams...)

Any contributions would be very appreciated!


Credits
-------

Thanks a lot to [Scala Graph][2] team for their great lib.
Another portion of thanks is for Brendan McKay for [nauty][1] that was already mentioned.



[1]: http://pallini.di.uniroma1.it/ "Nauty website"
[2]: http://www.scala-graph.org/ "Scala Graph website"
[3]: http://stackoverflow.com/questions/19710836/anthow-to-add-so-file-into-a-jar-and-use-it-within-jarset-the-java-library-p#comment29282627_19710836 "Pack .so to .jar"