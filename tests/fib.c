
volatile int *output = (int*)0x40000000;

void run()
{
   int i;

   for(i = 0; i < 10; i++) {
      *output = fib(i);
   }

}

int fib(int n)
{
   if(n <= 1) {
      return 1;
   } else {
      return fib(n - 1) + fib(n - 2);
   }
}

