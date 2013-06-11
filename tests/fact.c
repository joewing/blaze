
volatile int *output = (int*)0x40000000;

void run()
{
   int i;

   for(i = 0; i < 10; i++) {
      *output = fact(i);
   }

}

int fact(int n)
{
   if(n <= 1) {
      return 1;
   } else {
      return n * fact(n - 1);
   }
}

