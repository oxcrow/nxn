package core

type Span struct {
	Start int
	Endxx int
}

func NewSpan(start int, end int) Span {
	return Span{Start: start, Endxx: end}
}

func (s *Span) Len() int {
	return s.Endxx - s.Start
}
