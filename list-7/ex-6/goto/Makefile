CFLAGS += -O2 -pipe -Wall -Wextra

CC = clang

HEADERS = $(wildcard *.h)
SOURCES = $(wildcard *.c)
OBJECTS := $(SOURCES:.c=.o)

main: $(OBJECTS)
	$(CC) $(CFLAGS) -o main $(OBJECTS)

clean:
	rm -f $(OBJECTS) main
