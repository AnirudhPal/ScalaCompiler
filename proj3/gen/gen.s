.text
.global putchar, getchar, entry_point

################# FUNCTIONS #####################
#################################################


###################### MAIN #####################
entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	movq $10, %rdi
	movq $2, %rsi
	subq %rsi, %rdi
	movq %rdi, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret
#################################################


#################### DATA #######################

.data
heap:	.quad 0
#################################################
