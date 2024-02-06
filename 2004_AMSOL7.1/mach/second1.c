#include <stdio.h>
#include <time.h>
double second1_()
{
   clock_t mtime;
   int i;
   mtime = clock();
   return ((double) mtime)/CLOCKS_PER_SEC;
}

