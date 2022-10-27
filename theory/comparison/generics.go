package main

import "fmt"

type Stringer interface {
	String() string
}

func Stringify[T Stringer](s []T) (ret []string) {
	for _, v := range s {
		ret = append(ret, v.String())
	}
	return ret
}

type My_t struct {
}

func (m My_t) String() string {
	return "X"
}

func main() {
	var s [3]My_t

	fmt.Println(Stringify(s[0:3]))
}
