CC = gcc
CFLAGS = -Wall -Wextra -std=c11 -O0 -g

.PHONY: all clean

all: malloc.so

clean:
	rm -f malloc.so *.o


malloc.so: $(patsubst %.c,%.o,$(wildcard *.c))
	$(CC) $(CFLAGS) -fPIC -shared -o $@ $^

%.o: %.c $(wildcard *.h)
	$(CC) $(CFLAGS) -c -o $@ $<
