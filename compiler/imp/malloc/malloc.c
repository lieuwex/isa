#define _GNU_SOURCE  // MAP_ANONYMOUS
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/mman.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <ucontext.h>

#if 0
#define DEBUGGER() __asm("int3\n\t")
#else
#define DEBUGGER() {}
#endif


static void bail(void) {
	_Exit(errno);
}


#define MAPPING_SIZE (256 * 1024 * 1024)

static void *gmapping = NULL;

struct block_t {
	size_t size;
	struct block_t *next;
	bool free;
};

static struct block_t *chain_head = NULL;
static struct block_t *chain_last = NULL;
static size_t mapping_taken = 0;

static void number_to_string(char *dest, size_t value, size_t base) {
	static const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
	int len;
	for (len = 0; value > 0; len++) {
		dest[len] = digits[value % base];
		value /= base;
	}
	dest[len] = '\0';
	for (int i = 0; i < len / 2; i++) {
		char t = dest[i];
		dest[i] = dest[len - 1 - i];
		dest[len - 1 - i] = t;
	}
}

static size_t mul_bailoverflow(size_t a, size_t b) {
	if (a == 0) {
		return 0;
	}

	if (SIZE_MAX / a < b) {
		bail();
	}

	return a * b;
}

static void debug_mapping(void) {
	char msg[256];
	msg[0] = '\0';
	strcat(msg, "mapping_taken is now ");

	char number[40];
	number_to_string(number, mapping_taken, 10);
	strcat(msg, number);

	strcat(msg, " (of max ");
	number_to_string(number, MAPPING_SIZE, 10);
	strcat(msg, number);

	strcat(msg, ").\n");

	write(2, msg, strlen(msg));
}

static void debug_could_reuse(size_t blocksize, size_t allocsize) {
	char msg[256];
	msg[0] = '\0';
	strcat(msg, "could reuse allocation of size ");

	char number[40];
	number_to_string(number, blocksize, 10);
	strcat(msg, number);

	strcat(msg, " for malloc of size ");
	number_to_string(number, allocsize, 10);
	strcat(msg, number);

	strcat(msg, ".\n");

	write(2, msg, strlen(msg));
}

static void debug_malloc(size_t size, void *res) {
	char msg[256];
	msg[0] = '\0';
	strcat(msg, "malloc(");

	char number[40];
	number_to_string(number, size, 10);
	strcat(msg, number);

	strcat(msg, ") -> 0x");

	number_to_string(number, (size_t)res, 16);
	strcat(msg, number);
	
	strcat(msg, ".\n");

	write(2, msg, strlen(msg));
}

static void debug_free(size_t blocksize) {
	char msg[256];
	msg[0] = '\0';
	strcat(msg, "free() blocksize=");

	char number[40];
	number_to_string(number, blocksize, 10);
	strcat(msg, number);

	strcat(msg, ".\n");

	write(2, msg, strlen(msg));
}

static void debug_functions(void);

static void signal_handler(int signum, siginfo_t *info, void *context_) {
	ucontext_t *context = (ucontext_t*)context_;
	size_t pc = context->uc_mcontext.gregs[REG_RIP];

	if (signum == SIGSEGV) {
		char msg[256];
		msg[0] = '\0';
		strcat(msg, "SIGSEGV RAISED: FAULT ADDR 0x");

		char number[40];
		number_to_string(number, (size_t)info->si_addr, 16);
		strcat(msg, number);

		strcat(msg, " AT INSTR 0x");

		number_to_string(number, pc, 16);
		strcat(msg, number);

		strcat(msg, "!\n");

		write(2, msg, strlen(msg));

		msg[0] = '\0';
		strcat(msg, "RBX = 0x");

		number_to_string(number, context->uc_mcontext.gregs[REG_RBX], 16);
		strcat(msg, number);

		strcat(msg, ".\n");

		write(2, msg, strlen(msg));

		for (size_t i = 0; i < 128; i++) {
			write(2, (void*)pc + i - 7, 1);
		}

		bail();
	}
}

static void install_signal_handler(void) {
	struct sigaction act;
	sigemptyset(&act.sa_mask);
	act.sa_flags = SA_RESTART | SA_SIGINFO;
	act.sa_sigaction = signal_handler;

	if (sigaction(SIGSEGV, &act, NULL) != 0) {
		bail();
	}
}

static void gmapping_initialise(void) {
	DEBUGGER();
	gmapping = mmap(NULL, MAPPING_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
	if (gmapping == MAP_FAILED) {
		bail();
	}

	debug_functions();
	install_signal_handler();
}

static struct block_t* allocate_new_block(size_t size) {
	if (gmapping == NULL) {
		gmapping_initialise();
	}

	if (mapping_taken + sizeof(struct block_t) > MAPPING_SIZE) {
		bail();
	}
	if (size > MAPPING_SIZE - mapping_taken - sizeof(struct block_t)) {
		bail();
	}

	struct block_t *block = gmapping + mapping_taken;
	mapping_taken += sizeof(struct block_t) + size;

	block->size = size;
	block->next = NULL;

	if (chain_last != NULL) {
		chain_last->next = block;
	} else {
		chain_head = chain_last = block;
	}

	debug_mapping();

	return block;
}

static struct block_t* obtain_free_block(size_t size) {
	struct block_t *block = chain_head;
	while (block != NULL) {
		if (block->free && block->size >= size) {
			debug_could_reuse(block->size, size);
			break;
		}
		block = block->next;
	}
	return block;
}

void* malloc(size_t size) {
	DEBUGGER();

	if (size == 0) {
		return NULL;
	}

	struct block_t *block;
	if (chain_head == NULL) {
		block = allocate_new_block(size);
	} else {
		block = obtain_free_block(size);
		if (block == NULL) {
			block = allocate_new_block(size);
		}
	}

	block->free = false;

	void *res = (void*)block + sizeof(struct block_t);

	debug_malloc(size, res);

	return res;
}

void free(void *ptr) {
	DEBUGGER();
	if (ptr == NULL) {
		return;
	}

	struct block_t *block = (struct block_t*)(ptr - sizeof(struct block_t));
	if (block->free) bail();

	debug_free(block->size);

	block->free = true;
}

void* calloc(size_t nmemb, size_t size) {
	DEBUGGER();
	return malloc(mul_bailoverflow(nmemb, size));
}

void* realloc(void *ptr, size_t size) {
	DEBUGGER();
	if (ptr == NULL) {
		return malloc(size);
	}

	if (size == 0) {
		free(ptr);
		return NULL;
	}

	struct block_t *block = (struct block_t*)(ptr - sizeof(struct block_t));

	void *alloc = malloc(size);
	if (alloc == NULL) {
		return NULL;
	}

	size_t min_size = block->size < size ? block->size : size;
	memcpy(alloc, ptr, min_size);
	free(ptr);
	return alloc;
}

void* reallocarray(void *ptr, size_t nmemb, size_t size) {
	DEBUGGER();
	return realloc(ptr, mul_bailoverflow(nmemb, size));
}

static void debug_functions(void) {
	static const char *funcnames[] = {"malloc", "free", "calloc", "realloc", "reallocarray", "allocate_new_block", "obtain_free_block", "bail"};
	static const void *funcptrs[] = {malloc, free, calloc, realloc, reallocarray, allocate_new_block, obtain_free_block, bail};

	for (int i = 0; i < (int)sizeof funcnames / (int)sizeof funcnames[0]; i++) {
		char msg[256];
		msg[0] = '\0';
		strcat(msg, "function address: ");

		strcat(msg, funcnames[i]);

		strcat(msg, " 0x");

		char number[40];
		number_to_string(number, (size_t)funcptrs[i], 16);
		strcat(msg, number);

		strcat(msg, "\n");

		write(2, msg, strlen(msg));
	}
}
