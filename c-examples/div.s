	.text
.globl _mult
_mult:
	movl	4(%esp), %eax
	cltd
	idivl	8(%esp)
	ret
	.subsections_via_symbols
