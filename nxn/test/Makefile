SOURCES = $(wildcard *.ll)
OBJECTS = $(SOURCES:.ll=.bc)
EXECUTABLES = $(OBJECTS:.bc=.out)

all: $(EXECUTABLES)

%.out: %.bc
	clang $< -o $@

%.bc: %.ll
	llvm-as $< -o $@

clean:
	rm *.bc *.out

.PHONY: all clean
