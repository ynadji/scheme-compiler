	.cstring
LC0:
	.ascii "stuff: %s\12\0"
	.text
.globl _main
_main:
	pushl	%ebx
	subl	$24, %esp
	call	___i686.get_pc_thunk.bx
"L00000000001$pb":
	movl	$10, (%esp)
	call	L_malloc$stub
	xorl	%edx, %edx
L2:
	movb	$97, (%edx,%eax)
	addl	$1, %edx
	cmpl	$9, %edx
	jne	L2
	movb	$0, 9(%eax)
	movl	%eax, 4(%esp)
	leal	LC0-"L00000000001$pb"(%ebx), %eax
	movl	%eax, (%esp)
	call	L_printf$stub
	xorl	%eax, %eax
	addl	$24, %esp
	popl	%ebx
	ret
	.section __IMPORT,__jump_table,symbol_stubs,self_modifying_code+pure_instructions,5
L_malloc$stub:
	.indirect_symbol _malloc
	hlt ; hlt ; hlt ; hlt ; hlt
L_printf$stub:
	.indirect_symbol _printf
	hlt ; hlt ; hlt ; hlt ; hlt
	.subsections_via_symbols
	.section __TEXT,__textcoal_nt,coalesced,pure_instructions
.weak_definition	___i686.get_pc_thunk.bx
.private_extern	___i686.get_pc_thunk.bx
___i686.get_pc_thunk.bx:
	movl	(%esp), %ebx
	ret
