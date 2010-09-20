
/* A simple C function to be called from Pure. Computes the gcd of two
   integers using Euclid's algorithm. */

int mygcd(int x, int y)
{
  if (y == 0)
    return x;
  else
    return mygcd(y, x%y);
}
