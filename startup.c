/* a simple driver for scheme_entry */
#include <stdio.h>

#define fixnum_mask	3
#define fixnum_shift	2
#define fixnum_tag	0

#define char_shift	8
#define char_mask	0xff
#define char_tag	0x0f

#define hex_null	0x2f

#define bool_tag	0x3f
#define bool_mask	0x7f
#define bool_shift	7

#define heapobj_mask	0x7

#define pair_tag	0x1
#define vect_tag	0x2
#define str_tag		0x3
#define symb_tag	0x5
#define clos_tag	0x6

#define heapobj_shift	0x3

#define WORD_SIZE	4

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

int main(int argc, char** argv) {
	int val = scheme_entry();

	if ((val & fixnum_mask) == fixnum_tag) 
	{
		printf("%d\n", val >> fixnum_shift);
	}
	else if ((val & char_mask) == char_tag)
	{
		printf("%c\n", val >> char_shift);
	}
	else if ((val & bool_mask) == bool_tag)
	{
		printf("%s\n", val >> bool_shift ? "#t" : "#f");
	}
	else if (val == hex_null)
	{
		printf("()\n");
	}
	// gotta shift >> heapobj_shift for all of these
	else if ((val & heapobj_mask) == pair_tag)
	{
		printf("lol pair\n");
	}
	else if ((val & heapobj_mask) == vect_tag)
	{
		printf("lol vector\n");
	}
	else if ((val & heapobj_mask) == str_tag)
	{
		printf("lol string\n");
	}
	else if ((val & heapobj_mask) == symb_tag)
	{
		printf("lol symbol\n");
	}
	else if ((val & heapobj_mask) == clos_tag)
	{
		printf("#<procedure>\n");
	}
	else
	{
		printf("WTF: ");
		bin_prnt_int(val);
		printf("\n");
	}

	return 0;
}
