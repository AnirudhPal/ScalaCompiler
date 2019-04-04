val zero = '0'.toInt;

def myPrintInt(x: Int) = { // only 1 digits!
  if (x >= 0 && x <= 9)
    printDigit(x)
};

def printDigit(x: Int): Unit = {
  putchar(zero + x)
};

def fibonacci(x: Int): Int = {
	if(x == 0 || x == 1)  1
	else fibonacci(x-1)+fibonacci(x-2)
};

val r = fibonacci(4);
myPrintInt(r);
putchar(10);
0
