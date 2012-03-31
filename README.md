The Harbinger application
=========================

__Author:__ Valery Meleshkin ([`valery.meleshkin@gmail.com`](mailto:valery.meleshkin@gmail.com))

Distributed Pub/Sub for Erlang.
Experimental.

Now:
----

* Topic based subscription + filter-fun
* Multicasting to all `connected` nodes

Roadmap:
--------
* Node monitoring, "try other route", undelivered messages preservation
* Gossiping nodes state and optimal route detection
* Overload protection
* Multidimensional topic space
* Specialized structures for filtering with funs (CobasTree or similar technique)

