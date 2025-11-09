Require Import Coq.Strings.String. (* string operations *)
Require Import Coq.Strings.Ascii.  (* ascii operations *)
Require Import Coq.Arith.Arith.    (* arithmetic operations such as (n <=? 0) *)

Definition token := string.

Definition bounds(index start last: nat): bool :=
  (start <=? index) && ( index <? last).

Definition is_digit(c: ascii): bool :=
  let n := nat_of_ascii c in
  (bounds n 48 58).

Definition is_alpha(c: ascii): bool :=
  let n := nat_of_ascii c in
  (bounds n 65 91) || (bounds n 97 123).

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

Example test_is_alpha:
  (* ASCII code 65 to 90 *)
  (is_alpha "A" = true) /\
  (is_alpha "Z" = true) /\
  (* ASCII code 91 to 96 *)
  (is_alpha "[" = false) /\
  (is_alpha "\" = false) /\ (* " \" *)
  (is_alpha "]" = false) /\
  (is_alpha "^" = false) /\
  (is_alpha "_" = false) /\
  (is_alpha "`" = false) /\
  (* ASCII code 97 to 122 *)
  (is_alpha "a" = true) /\
  (is_alpha "z" = true).
Proof.
  repeat split.
  all: unfold is_alpha; cbn; reflexivity.
Qed.
