.text
#if (__APPLE__)
	.global _entry_point

_entry_point:
#else
	.global entry_point

entry_point:
#endif
	push %rbp	# save stack frame for C convention
	mov %rsp, %rbp

	# beginning generated code
	movq $8, %rax
	pushq %rax
	movq $7, %rax
	pushq %rax
	movq $2, %rax
	pushq %rax
	movq $3, %rax
	pushq %rax
	movq $2, %rax
	popq %rbx
	addq %rbx, %rax
	popq %rbx
	movq $0, %rdx
	idivq %rbx
	popq %rbx
	imulq %rbx, %rax
	popq %rbx
	subq %rbx, %rax
	# end generated code
	# %rax contains the result

	mov %rbp, %rsp	# reset frame
	pop %rbp
	ret



