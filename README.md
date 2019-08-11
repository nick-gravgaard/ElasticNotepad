# Elastic Notepad

Wouldn't it be nice if we could use proportional fonts to write code and still
have things line up? Well, thanks to
[elastic tabstops](http://nickgravgaard.com/elastic-tabstops/), now we can. This
editor implements that invention, and should serve as a reference for anyone who
wants to implement it in other editors.

The reference implementation of the core elastic tabstops algorithm can be
found in [elasticTabstops.scala](src/main/scala/elasticTabstops.scala).

## Prerequisites

The current version's settings default to using the fonts
[Merriweather](https://fonts.google.com/specimen/Merriweather) and
[Inconsolata](https://fonts.google.com/specimen/Inconsolata). If you don't have
them installed, your system's default Serif and Monospaced fonts will be used
instead, and you can change Elastic Notepad's settings to use whatever fonts
you like, but I recommend trying it with these fonts first.

Since Elastic Notepad is written in Scala, you'll also need to have Java
installed to run it, and sbt installed if you want to build it.

On Windows at least, if you install Java for the sake of running this, it seems
you'll need to restart your system before Java can use logical fonts properly.
(So restart your system if you notice that toggling elastic mode off doesn't
switch to a monospaced font and text doesn't *appear* lined up as a result.)

## Running it

To run the jar file, use this:

	java -jar elastic-notepad.jar

## Changing it

When developing, it can be run with:

	mill app.run

You can build a new jar file with:

	mill clean && mill app.assembly

## Ugly font rendering?

Some systems do a poor job of rendering fonts in Java GUIs. On my Linux system
I've added the following line to `$HOME/.profile` to fix this:

	export _JAVA_OPTIONS="$_JAVA_OPTIONS -Dawt.useSystemAAFontSettings=on"
