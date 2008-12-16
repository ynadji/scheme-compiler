#include <stdio.h>
#include <stdlib.h>

int main() {
	char *buf = (char *) malloc(10*sizeof(char));
	int i;
	for (i = 0; i < 9; i++) {
		buf[i] = 'a';
	}
	buf[i] = '\0';

	printf("stuff: %s\n", buf);

	return 0;
}
