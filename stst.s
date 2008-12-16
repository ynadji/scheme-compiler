.text
.globl _scheme_entry
_scheme_entry:
	movl $260, %eax
	sall $6, %eax
	orl $15, %eax
	ret
