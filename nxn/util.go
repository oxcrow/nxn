package main

import "os"

func ignore(a ...any) {
}

func readFile(filename string) (string, error) {
	bytes, err := os.ReadFile(filename)
	if err != nil {
		return "", err
	}
	content := string(bytes)
	return content, nil
}
