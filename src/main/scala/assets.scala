package object assets {
  val InitialText = StringContext.treatEscapes(
    """/* Hopefully this editor widget should demonstrate how elastic tabstops work.\t*/
      |/* Try inserting and deleting different parts of the text and watch as the tabstops move.\t*/
      |/* If you like this, please ask the writers of your text editor to implement it.\t*/
      |
      |#include <stdio.h>
      |
      |struct ipc_perm
      |{
      |\tkey_t\tkey;
      |\tushort\tuid;\t/* owner euid and egid\t*/
      |\tushort\tgid;\t/* group id\t*/
      |\tushort\tcuid;\t/* creator euid and egid\t*/
      |\tcell-missing\t\t/* for test purposes\t*/
      |\tushort\tmode;\t/* access modes\t*/
      |\tushort\tseq;\t/* sequence number\t*/
      |};
      |
      |int someDemoCode(\tint fred,
      |\tint wilma)
      |{
      |\tx();\t/* try making\t*/
      |\tprintf(\"hello!\\");\t/* this comment\t*/
      |\tdoSomethingComplicated();\t/* a bit longer\t*/
      |\tfor (i = start; i < end; ++i)
      |\t{
      |\t\tif (isPrime(i))
      |\t\t{
      |\t\t\t++numPrimes;
      |\t\t}
      |\t}
      |\treturn numPrimes;
      |}
      |
      |---- and now for something completely different: a table ----
      |
      |Title\tAuthor\tPublisher\tYear
      |Generation X\tDouglas Coupland\tAbacus\t1995
      |Informagic\tJean-Pierre Petit\tJohn Murray Ltd\t1982
      |The Cyberiad\tStanislaw Lem\tHarcourt Publishers Ltd\t1985
      |The Selfish Gene\tRichard Dawkins\tOxford University Press\t2006""".stripMargin
  )
}
