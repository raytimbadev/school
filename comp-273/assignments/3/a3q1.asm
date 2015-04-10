	.data
arr:	10, 5, 2, 20, 20, -5, 3, 19, 9, 1
arrlen: 10
nl:	'\n' '\0'

	.text
	j	main
	
pnl:	la	$a0, nl
	li	$v0, 4
	syscall
	jr	$ra
		
	
prarr:	move	$t1, $sp		# back up the stack pointer (the addresses above $t1 are the function arguments)
	sub	$sp, $sp, 4		# depress the stack to store the return address
	sw	$ra, ($sp)		# store the return address
	sub	$sp, $sp, 4		# depress the stack to store locals: one word for the counter
	lw	$t2, ($t1)		# load the first argument (starting index) into t2
	mul	$t2, $t2, 4		# multiply the starting index by 4 to get the offset in bytes
	lw	$t3, 4($t1)		# load the second argument (ending index) into t3
	mul	$t3, $t3, 4		# multiply the ending index by 4 to get the offset in bytes
	lw	$t4, arrlen		# load the array length (in words) into t4
	mul	$t4, $t4, 4		# multiply the array length by 4 to get its length in bytes
	sw	$t2, ($sp)		# initialize the counter to the starting index
	blt	$t2, $t3, pve		# if start < end, then the offset will be positive
nve:	li	$t6, -4			# else it is negative
	xor 	$t2, $t2, $t3		# swap the start and end
	xor	$t3, $t3, $t2
	xor	$t2, $t2, $t3
	j	loop
pve:	li	$t6, 4
loop:	lw	$t5, ($sp)		# load the counter into t5
	blt	$t5, 0, prarr_		# if the counter is less than zero, abort
	bge	$t5, $t4, prarr_ 	# if the counter is greater than or equal to the array size, abort
	bgt	$t5, $t3, prarr_	# if the counter is greater than the ending index, abort
	blt	$t5, $t2, prarr_	# if the counter is less than the starting index, abort 
	li	$v0, 1			# prepare the print_int syscall
	lw	$a0, arr($t5)		# load the int to print into the syscall argument register
	syscall				# print the integer
	jal	pnl			# print a newline
	add	$t5, $t5, $t6		# increment/decrement the counter by 4, according to the sign of t6
	sw	$t5, ($sp)		# overwrite the value of the counter that we have on the stack
	j	loop			# loop back around
prarr_:	lw	$t2, -4($t1)		# load the return address into t2
	jr	$t2			# jump to the return address
	
main:	li	$v0, 5		# read one integer (the starting index)
	syscall
	move	$t1, $v0	# store the integer
	li	$v0, 5		# read another integer (the ending index)
	syscall
	move	$t2, $v0	# store the integer on the stack
	# prepare to do a function call by the C calling convention
	sub	$sp, $sp, 4	# depress the stack by one word for the second argument
	sw	$t2, ($sp)	# write the ending index onto the stack
	sub	$sp, $sp, 4	# depress the stack by one word for the first argument
	sw	$t1, ($sp)	# write the starting index onto the stack
	jal	prarr
	add	$sp, $sp, 8	# restore the stack pointer
	