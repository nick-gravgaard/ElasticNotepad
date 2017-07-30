# Elastic Notepad

Wouldn't it be nice if we could use proportional fonts to write code? Well,
thanks to [elastic tabstops](http://nickgravgaard.com/elastic-tabstops/), now
we can. This editor implements that invention and should serve as a reference
for anyone who wants to implement it in other editors.

The reference implementation of the core elastic tabstops algorithm can be
found in [elasticTabstops.scala](src/main/scala/elasticTabstops.scala).

## Prerequisites

The current version's settings default to using the fonts
[Merriweather](https://fonts.google.com/specimen/Merriweather) and
[Droid Sans Mono](https://fonts.google.com/specimen/Droid+Sans+Mono). You can
change your settings of course, but I recommend installing these fonts so you
can try them out.
