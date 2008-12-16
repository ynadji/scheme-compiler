	.text
.globl _func1
_func1:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	call	L_func2$stub
	leave
	ret
.globl _func2
_func2:
	pushl	%ebp
	movl	%esp, %ebp
	movl	12(%ebp), %eax
	addl	8(%ebp), %eax
	addl	16(%ebp), %eax
	popl	%ebp
	ret
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_func2$stub:
	.indirect_symbol _func2
	hlt ; hlt ; hlt ; hlt ; hlt
	.subsections_via_symbols
