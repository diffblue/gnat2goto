#include <assert.h>

int foo(unsigned char* arg)
{
   if (arg[0] == 't')
      return 1;
   else
      return 0;
}

int main()
{
   int res = foo((unsigned char *)"test");
   assert(res == 0);
   return 0;
}
