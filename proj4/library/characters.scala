def printChar(c: Char): Unit = putchar(c.toInt);

def charRead(): Char = getchar().toChar;

def println(): Unit = printChar('\n');

val int0 = '0'.toInt;
val int9 = '9'.toInt;
def isCharDigit(c: Char): Boolean = {
  val intC = c.toInt;
  int0 <= intC && intC <= int9
};

def charDigitToInt(c: Char): Int = c.toInt - int0;

def intCharDigit(i: Int): Char = (i + int0).toChar;
