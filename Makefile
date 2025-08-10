all:
	zig build -Doptimize=Debug
test:
	zig build test -Doptimize=Debug
run:
	zig build -Doptimize=Debug run
