package core

import "os"

func ReadFile(filename string) (string, error) {
	bytes, err := os.ReadFile(filename)
	if err != nil {
		return "", err
	}
	content := string(bytes)
	return content, nil
}
