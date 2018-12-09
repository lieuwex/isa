	li r15, 4000
	call r14, main
	jnz r0, exit

putchar:
	li r13, 8
	sub r15, r15, r13
	s64 r15, r1
	add r1, r15, r13
	l8 r1, r1
	li r13, 80
	s8 r13, r1
	l64 r1, r15
	li r13, 8
	add r15, r15, r13
	mv r0, r14

