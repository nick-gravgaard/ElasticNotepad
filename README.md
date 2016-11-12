# Elastic tabstops demo

This is a simple demo of elastic tabstops which uses Swing and is written in
Scala. It was originally based on some Java code I wrote back in 2006 but has
been partially rewritten in a more functional style. There is still room for
improvement.

It is intended to demonstrate the concept of elastic tabstops, rather than the
most efficient implementation. It is hugely inefficient as it rescans the entire
document every time the buffer changes, rather than caching everything and only
rescanning the parts that have changed.
