package object assets:
  val InitialText = StringContext.processEscapes(
    """
      |
      |\t--------|\t--------------------------\t|--------
      |\t--------|\tWelcome to Elastic Notepad\t|--------
      |\t--------|\t--------------------------\t|--------
      |\t--------|\t\t|--------
      |\t--------|\tThis is your scratch file.\t|--------
      |\t--------|\tIt's initialised with this\t|--------
      |\t--------|\ttext so you can play with\t|--------
      |\t--------|\telastic tabstops.\t|--------
      |\t--------|\t\t|--------
      |\t--------|\t--------------------------\t|--------
      |
      |
      |int someDemoCode(\tint start,
      |\tint length)
      |{
      |\tx()\t/* try making\t*/
      |\tprint(\"hello!\")\t/* this comment\t*/
      |\tdoSomethingComplicated()\t/* a bit longer\t*/
      |\tfor (i in range(start, length))
      |\t{
      |\t\tif (isValid(i))
      |\t\t{
      |\t\t\tcount++
      |\t\t}
      |\t}
      |\treturn count
      |}
      |
      |
      |You can use elastic tabstops with tables and TSV files too
      |
      |Title\tAuthor\tPublisher\tYear
      |Generation X\tDouglas Coupland\tAbacus\t1995
      |Informagic\tJean-Pierre Petit\tJohn Murray Ltd\t1982
      |The Cyberiad\tStanislaw Lem\tHarcourt Publishers Ltd\t1985
      |The Selfish Gene\tRichard Dawkins\tOxford University Press\t2006""".stripMargin
  )
