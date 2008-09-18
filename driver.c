/* a simple driver for scheme_entry */
#include <stdio.h>

#define fixnum_mask	3
#define fixnum_shift	2
#define fixnum_tag	0

#define char_shift	8
#define char_mask	0xff
#define char_tag	0x0f

#define hex_null	0x2f

#define bool_t		0x9f
#define bool_f		0x1f

#define WORD_SIZE	4

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
	else if (val == bool_t)
	{
		printf("#t\n");
	}
	else if (val == bool_f)
	{
		printf("#f\n");
	}
	else if (val == hex_null)
	{
		printf("()\n");
	}

	return 0;
}
