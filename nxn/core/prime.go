package core

import "fmt"

// Convert character to a unique prime number
func CharToPrime(c byte) int {
	return Prime(CharToInt(c))
}

// Convert character to a unique integer number
func CharToInt(c byte) int {
	i := 0
	if c >= '0' && c <= '9' {
		i = int(c) - int('0')
	} else if c >= 'a' && c <= 'z' {
		i = 10 + int(c) - int('a')
	} else if c >= 'A' && c <= 'Z' {
		i = (10 + 26) + int(c) - int('A')
	} else {
		panic(fmt.Sprintf("Character '%v' can not be converted to Integer", string(c)))
	}
	return i
}

// i'th prime number
//
// This can be used for creating unique integer IDs from strings.
//
// As of now we support 62 prime numbers for,
// - 10 digits
// - 26 lower case alphabets
// - 26 upper case alphabets
func Prime(i int) int {
	primes := [70]int{
		2, 3, 5, 7, 11, 13, 17, 19, 23, 29,
		31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
		73, 79, 83, 89, 97, 101, 103, 107, 109, 113,
		127, 131, 137, 139, 149, 151, 157, 163, 167, 173,
		179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
		233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
		283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
	}
	return primes[i]
}
