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
[Inconsolata](https://fonts.google.com/specimen/Inconsolata). If you don't have
them installed, your system's default Serif and Monospaced fonts will be used
instead, and you can change Elastic Notepad's settings to use whatever fonts
you like, but I recommend trying it with these fonts first.

You'll also need to have Java installed.

## Running it

You can run it like this:

	java -jar ElasticNotepad.jar
