# Return an exit code of integer value after adding two integers

function w $add(w %a, w %b) {
@start
    %sum = w add %a, %b
    ret %sum
}

export function w $main() {
@start
    %sum = w call $add(w 4, w 4)
    ret %sum
}
