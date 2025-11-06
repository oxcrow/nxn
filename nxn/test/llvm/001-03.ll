; Swap two integers using pointers

target triple = "x86_64-pc-linux-gnu"

define void @swap(i32* %a, i32* %b) {
begin:
    %a.value = load i32, i32* %a
    %b.value = load i32, i32* %b
    store i32 %b.value, i32* %a        ; a = b
    store i32 %a.value, i32* %b        ; b = a
    ret void
}

define i32 @main() {
begin:
    %a = alloca i32                    ; stack allocate pointer to i32 value
    %b = alloca i32                    ; stack allocate pointer to i32 value
    store i32 8, i32* %a               ; store value 8 in stack memory
    store i32 7, i32* %b               ; store value 7 in stack memory
    call void @swap(i32* %a, i32* %b)  ; swap values using pointers
    %a.new = load i32, i32* %a         ; load value from stack memory
    ret i32 %a.new
}
