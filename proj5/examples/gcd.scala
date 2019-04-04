def gcd(x: Int, y: Int): Int =
	if (y == 0)
		x
	else
		gcd(y, y%x);

gcd(2016, 714)
