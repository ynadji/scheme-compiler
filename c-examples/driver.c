#include <stdio.h>
 
#define fixnum_mask       3
#define fixnum_tag        0
#define fixnum_shift      2
#define char_mask         255
#define char_tag          15
#define char_shift        8
#define boolean_mask      127
#define boolean_tag       63
#define boolean_shift     7
#define empty_list        47

void bin_prnt_byte(int x)
{
   int n;
   for(n=0; n < 8; n++)
   {
      if((x & 0x80) !=0)
      {
         printf("1");
      }
      else
      {
         printf("0");
      }
      if (n==3)
      {
         printf(" "); /* insert a space between nybbles */
      }
      x = x<<1;
   }
}

void bin_prnt_int(int x)
{
   int hi, lo;
   hi=(x>>8) & 0xff;
   lo=x&0xff;
   bin_prnt_byte(hi);
   printf(" ");
   bin_prnt_byte(lo);
}
 
int main(int argc, char** argv)
{
  int val = scheme_entry ();
 
  if ((val & fixnum_mask) == fixnum_tag) 
  {
    printf("%d\n", val >> fixnum_shift);
  } 
  else if ((val & char_mask) == char_tag) 
  {
    printf("%c\n", val >> char_shift);
  } 
  else if ((val & boolean_mask) == boolean_tag) 
  {
    printf("%d\n", val >> boolean_shift);
  } 
  else if (val == empty_list) 
  {
    printf("()\n");
  } 
  else 
  {
    printf("WTF: ");
    bin_prnt_int(val);
    printf("\n");
  }
  return 0;
}

