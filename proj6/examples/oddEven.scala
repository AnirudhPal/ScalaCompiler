val zero = '0'.toInt;

def printDigit(x: Int) = {
  putchar(zero + x)
};

def myPrintInt(x: Int) = { // only 1 digits!
  if (x >= 0 && x <= 9)
    printDigit(x)
};

def isEvenRec(n: Int) : Int = {
    if (n == 0) 1
    else isOddRec(n - 1)
};

def isOddRec(n: Int) : Int = {
    if (n == 0) 0
    else isEvenRec(n - 1)
};

var pr = 9;
if (isEvenRec(7)==1) pr = 4;
myPrintInt(pr);
putchar(10);
1
