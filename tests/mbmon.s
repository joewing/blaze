
.equ  initial_sp,    0x1000
.equ  monitor_size,  0x100

   .align   2
   .org     0

   addik    r1, r0,  initial_sp   
   addik    r14, r0, idle_loop
   addk     r15, r0, r14
   addk     r16, r0, r14
   addk     r17, r0, r14
   bri      run

idle_loop:
   bri      idle_loop

   .org     monitor_size
program_start:

