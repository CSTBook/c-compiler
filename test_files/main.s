	.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$-20, %rsp
	movl	$1, -4(%rbp)
	movl	-4(%rbp), %r11d
	imull	$2, %r11d
	movl	%r11d, -4(%rbp)
	movl	$8, %eax
	cdq
	movl	$2, %r10d
	idivl	%r10d
	movl	%eax, -8(%rbp)
	movl	$4, -12(%rbp)
	movl	-8(%rbp), %r10d
	addl	%r10d, -12(%rbp)
	movl	$3, -16(%rbp)
	movl	-16(%rbp), %r11d
	imull	-12(%rbp), %r11d
	movl	%r11d, -16(%rbp)
	movl	-4(%rbp), %r10d
	movl	%r10d, -20(%rbp)
	movl	-16(%rbp), %r10d
	subl	%r10d, -20(%rbp)
	movl	-20(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret