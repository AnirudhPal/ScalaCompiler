val zero: Int = '0'.toInt;

def printDigit(x: Int): Unit = {
  putchar(zero + x)
};

def myPrintInt(x: Int): Unit = { // only 1 digits!
  if (x >= 0 && x <= 9)
    printDigit(x)
};

def makeAdder(x: Int): Int => Int = (y: Int) => x + y;

val inc = makeAdder(1);
myPrintInt(inc(3));
putchar(10);
0
