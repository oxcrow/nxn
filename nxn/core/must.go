package core

func Must[T any](x T, e error) T {
	if e != nil {
		panic(e)
	}
	return x
}
