#define STACK_TOP 4000
#define HEAP_BASE 1048576
#define OUTPUT_PORT 80
#define INPUT_PORT 81

	li r15, STACK_TOP
	call r14, main
	jnz r0, exit


putchar:
	li r13, 8
	sub r15, r15, r13
	s64 r15, r1
	add r1, r15, r13
	l8 r1, r1
	li r13, OUTPUT_PORT
	s8 r13, r1
	l64 r1, r15
	li r13, 8
	add r15, r15, r13
	mv r0, r14


getchar:
	li r13, INPUT_PORT
	l16 r13, r13
	mv r0, r14


#define __MALLOC_CHAIN_HEAD HEAP_BASE
#define __MALLOC_CHAIN_LAST (HEAP_BASE+8)
#define __MALLOC_TAKEN (HEAP_BASE+16)
#define __MALLOC_BASE (HEAP_BASE+24)

; struct block_t {
; 	size_t size;
; 	struct block_t *next;
; 	bool free;
; }

__malloc_init:
	li r13, 8
	sub r15, r15, r13
	s64 r15, r1

	li r13, 0
	li r1, __MALLOC_CHAIN_HEAD
	s64 r1, r13
	li r1, __MALLOC_CHAIN_LAST
	s64 r1, r13
	li r1, __MALLOC_TAKEN
	s64 r1, r13

	li r13, 8
	l64 r1, r15
	add r15, r15, r13
	mv r0, r14


; args: size_t size
; return: struct block_t*
__malloc_allocate_new_block:
	li r13, 8
	sub r15, r15, r13
	s64 r15, r1
	sub r15, r15, r13
	s64 r15, r2
	sub r15, r15, r13
	s64 r15, r3
	sub r15, r15, r13
	s64 r15, r4
	sub r15, r15, r13
	s64 r15, r5

	; r1 = taken
	li r1, __MALLOC_TAKEN
	l64 r1, r1
	; r2 = block
	li r2, __MALLOC_BASE
	add r2, r1, r2

	; r3 = size
	li r3, 40
	add r3, r15, r3
	l64 r3, r3

	; taken += sizeof(struct block_t) + size
	li r4, 24
	add r4, r4, r3
	add r4, r4, r1
	li r5, __MALLOC_TAKEN
	s64 r5, r4

	; block->size = size
	s64 r2, r3

	; block->next = NULL
	li r4, 8
	add r4, r2, r4
	li r5, 0
	s64 r4, r5

	; r4 = chain_last
	li r4, __MALLOC_CHAIN_LAST
	l64 r4, r4

	; if chain_last > 0 then:
	li r5, 0
	lt r5, r5, r4
	jz r5, __malloc_allocate_new_block__L1

	;     chain_last->next = block
	li r5, 8
	add r4, r4, r5
	s64 r4, r2
	jnz r0, __malloc_allocate_new_block__L2

	; else:
	;     chain_head = chain_last = block
__malloc_allocate_new_block__L1:
	li r5, __MALLOC_CHAIN_LAST
	s64 r5, r2
	li r5, __MALLOC_CHAIN_HEAD
	s64 r5, r2

__malloc_allocate_new_block__L2:
	; return block
	mv r12, r2

	li r13, 8
	l64 r5, r15
	add r15, r15, r13
	l64 r4, r15
	add r15, r15, r13
	l64 r3, r15
	add r15, r15, r13
	l64 r2, r15
	add r15, r15, r13
	l64 r1, r15
	add r15, r15, r13
	mv r13, r12
	mv r0, r14


; args: size_t size
; return: struct block_t*
__malloc_obtain_free_block:
	li r13, 8
	sub r15, r15, r13
	s64 r15, r1
	sub r15, r15, r13
	s64 r15, r2
	sub r15, r15, r13
	s64 r15, r3
	sub r15, r15, r13
	s64 r15, r4

	; r1 = block = chain_head
	li r1, __MALLOC_CHAIN_HEAD
	l64 r1, r1

	; r4 = size
	li r4, 32
	add r4, r15, r4
	l64 r4, r4

	; while block > NULL:
__malloc_obtain_free_block__cond:
	li r2, 0
	lt r2, r2, r1
	jz r2, __malloc_obtain_free_block__after

__malloc_obtain_free_block__body:
	;     r2 = block->free
	li r2, 16
	add r2, r1, r2
	l8 r2, r2

	;     r3 = block->size
	li r3, 8
	add r3, r1, r3
	l64 r3, r3

	;     r3 = size <= block->size
	lte r3, r4, r3

	;     r2 = block->free && size <= block->size
	and r2, r2, r3

	;     if r2 then break
	jnz r2, __malloc_obtain_free_block__after

	;     block = block->next
	li r2, 8
	add r1, r1, r2
	l64 r1, r1

	jnz r0, __malloc_obtain_free_block__cond

__malloc_obtain_free_block__after:
	; return block
	mv r12, r1

	li r13, 8
	l64 r4, r15
	add r15, r15, r13
	l64 r3, r15
	add r15, r15, r13
	l64 r2, r15
	add r15, r15, r13
	l64 r1, r15
	add r15, r15, r13
	mv r13, r12
	mv r0, r14


; args: size_t size
; return: void*
malloc:
	li r13, 8
	sub r15, r15, r13
	s64 r15, r14
	sub r15, r15, r13
	s64 r15, r1
	sub r15, r15, r13
	s64 r15, r2
	sub r15, r15, r13
	s64 r15, r3
	sub r15, r15, r13
	s64 r15, r4

	; r1 = size
	li r1, 40
	add r1, r15, r1
	l64 r1, r1

	; if size <= 0 then return 0
	; put the zero in r12, since that's the returned register as well
	li r12, 0
	lte r2, r1, r12
	jnz r2, malloc__return

	; r2 = chain_head
	li r2, __MALLOC_CHAIN_HEAD
	l64 r2, r2

	; final block will be put in r12
	; if chain_head <= 0 then perform allocation; else try obtain
	; r3 is still 0
	lte r3, r2, r3
	jnz r3, malloc__perform_alloc

	; try obtain
	li r3, 8
	sub r15, r15, r3
	s64 r15, r1
	call r14, __malloc_obtain_free_block
	add r15, r15, r3

	jz r13, malloc__perform_alloc

	mv r12, r13
	jnz r0, malloc__obtained

	; perform allocation
malloc__perform_alloc:
	li r3, 8
	sub r15, r15, r3
	s64 r15, r1
	call r14, __malloc_allocate_new_block
	add r15, r15, r3
	mv r12, r13

malloc__obtained:
	; block->free = false
	li r1, 16
	add r1, r12, r1
	li r2, 0
	s8 r1, r2

	; return sizeof(struct block_t)
	li r1, 24
	add r12, r12, r1

malloc__return:
	li r13, 8
	l64 r4, r15
	add r15, r15, r13
	l64 r3, r15
	add r15, r15, r13
	l64 r2, r15
	add r15, r15, r13
	l64 r1, r15
	add r15, r15, r13
	l64 r14, r15
	add r15, r15, r13
	mv r13, r12
	mv r0, r14


; args: void *ptr
free:
	li r13, 8
	sub r15, r15, r13
	s64 r15, r1
	sub r15, r15, r13
	s64 r15, r2

	; r1 = ptr
	li r1, 16
	add r1, r15, r1
	l64 r1, r1

	; if r1 <= 0 then return
	li r2, 0
	lte r2, r1, r2
	jnz r2, free__return

	; block = ptr - sizeof(struct block_t)
	; r1 = &block->free = ptr - 24 + 16 = ptr - 8
	li r2, 8
	sub r1, r1, r2

	; r2 = block->free
	l8 r2, r1

	; if r3 > 0 then bail
	li r13, 0
	lt r13, r13, r2
	jnz r13, free__bail_double_free

	; block->free = true
	li r2, 1
	s8 r1, r2

free__return:
	li r13, 8
	l64 r2, r15
	add r15, r15, r13
	l64 r1, r15
	add r15, r15, r13
	mv r0, r14
