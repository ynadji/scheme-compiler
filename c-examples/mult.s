	.text
.globl _mult
_mult:
	movl	8(%esp), %eax
	imull	4(%esp), %eax
	ret
	.subsections_via_symbols
