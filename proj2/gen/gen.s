.text
#if(__APPLE__)
	.global _entry_point

_entry_point:
#else
	.global entry_point

entry_point:
#endif
	push %rbp	# save stack frame for C convention
	mov %rsp, %rbp

	# beginning generated code
	movq $10, %rbx
	movq $1, %rcx
	jmp loop1_cond
loop1_body:
	movq %rcx, %rdi
	movq $5, %rsi
	cmpq %rsi, %rdi
	je if2_then
	movq %rcx, %rdi
	movq $7, %rsi
	addq %rsi, %rdi
	movq %rdi, %rcx
	jmp if2_end
if2_then:
	movq %rcx, %rdi
	movq $7, %rsi
	addq %rsi, %rdi
	movq %rdi, %rcx
if2_end:
loop1_cond:
	movq %rbx, %rdi
	movq %rcx, %rsi
	cmpq %rsi, %rdi
	jg loop1_body
	movq %rcx, %rdi
	movq %rdi, %rcx
	movq %rcx, %rbx
	movq %rbx, %rax
	# end generated code
	# %rax contains the result

	mov %rbp, %rsp	# reset frame
	pop %rbp
	ret



