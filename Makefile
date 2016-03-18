PROGRAM := tiny-lisp
OBJECTS := tiny-lisp.o

CFLAGS += -std=gnu11 -Wall -pedantic

PREFIX ?= /usr/local

.PHONY: all
all: CPPFLAGS += -DNDEBUG
all: CFLAGS   += -O3 -ggdb -fno-omit-frame-pointer
all: LDFLAGS  += -lm
all: $(PROGRAM)

.PHONY: debug
debug: CPPFLAGS += -DDEBUG
debug: CFLAGS   += -g
debug: LDFLAGS  += -g -lm
debug: $(PROGRAM)

.PHONY: clean
clean:
	$(RM) $(PROGRAM) $(OBJECTS)

$(PROGRAM): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

.PHONY: install
install: all
	mkdir -p $(PREFIX)/bin
	install $(PROGRAM) $(PREFIX)/bin
