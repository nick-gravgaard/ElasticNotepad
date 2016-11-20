# Elastic tabstops demo

This is a simple demo of elastic tabstops which uses Swing and is written in
Scala. It was originally based on some Java code I wrote back in 2006 but has
been rewritten in a more functional style. I think this makes the core algorithm
easier to understand, which hopefully makes it easier for other people to
reimplement.

It is intended to demonstrate the concept of elastic tabstops, rather than the
most efficient implementation. It is very inefficient as it rescans the entire
buffer every time a change is made, rather than caching everything and only
rescanning the parts that have changed. Unless performance is not an issue,
those wishing to reimplement elastic tabstops should find a way to cache cell
widths per line.
