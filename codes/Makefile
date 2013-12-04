CC=gcc
CFLAG=-Wall -g
LIBS=-lm
source=jpeg-grab.c zig-zag.c dct.c huffman.c

include $(sources:.c=.d)

all=jpeg-grab

%.d: %.c
	set -e; rm -f $@; \
	$(CC) -MM $(CFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

jpeg-grab: jpeg-grab.o zig-zag.o dct.o huffman.o
	gcc -o $@ $^ ${CFLAG} ${LIBS}
