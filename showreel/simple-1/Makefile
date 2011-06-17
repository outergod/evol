CC = cc
SOURCES = $(shell ls *.c)
OBJECTS = $(SOURCES:.c=.o)

all: main

$(OBJECTS):
	${CC} ${CFLAGS} -c -o $@ $(@:.o=.c)

main: $(OBJECTS)

clean:
	rm -f $(OBJECTS) main
