init:
	li r1, 8
	li r10, 0x50
	li r14, 10
main:
	li r13, 104
	call r15, appendchar
	li r13, 101
	call r15, appendchar
	li r13, 108
	call r15, appendchar
	li r13, 108
	call r15, appendchar
	li r13, 111
	call r15, appendchar
	li r13, 10
	call r15, appendchar
	li r13, 0
	call r15, appendchar

	li r14, 10

	call r15, printstr ; (uses r13), prints the char given on r14, returns to r15
	call r0, end

xxx:
	s8 r10, r13 ; print char
	add r14, r14, r1 ; move pointer
printstr:
	l8 r13, r14 ; load char
	jnz r13, xxx ; if char isnt zero -> xxx
	mv r0, r15 ; otherwise goto caller

appendchar:
	s8 r14, r13
	add r14, r14, r1
	mv r0, r15

end:
