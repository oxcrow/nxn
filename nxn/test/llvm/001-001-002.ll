; Return an exit code of integer value after adding two integers

target triple = "x86_64-pc-linux-gnu"

define i32 @add(i32 %a, i32 %b) {
begin:
    %sum = add i32 %a, %b
    ret i32 %sum
}

define i32 @main() {
begin:
    %sum = call i32 @add(i32 4, i32 4)
    ret i32 %sum
}
