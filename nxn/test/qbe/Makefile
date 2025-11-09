SOURCES = $(wildcard *.ss)
OBJECTS = $(SOURCES:.ss=.s)
EXECUTABLES = $(OBJECTS:.s=.out)

all: $(EXECUTABLES)

%.out: %.s
	cc $< -o $@

%.s: %.ss
	qbe $< -o $@

clean:
	rm *.s *.out

.PHONY: all clean
