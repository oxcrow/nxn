Require Import Coq.Strings.String. (* string operations *)
Require Import Coq.Strings.Ascii.  (* ascii operations *)
Require Import Coq.Arith.Arith.    (* arithmetic operations such as (n <=? 0) *)

Definition token := string.

Definition is_digit(c: ascii) : bool :=
    let n := nat_of_ascii c in
    (48 <=? n) && (n <=? 57).

Example test_is_digit:
    (is_digit "0" = true) /\
    (is_digit "1" = true) /\
    (is_digit "2" = true) /\
    (is_digit "3" = true) /\
    (is_digit "4" = true) /\
    (is_digit "5" = true) /\
    (is_digit "6" = true) /\
    (is_digit "7" = true) /\
    (is_digit "8" = true) /\
    (is_digit "9" = true) /\
    (is_digit "!" = false).
Proof.
    repeat split.
    all: unfold is_digit; cbn; reflexivity.
Qed.
