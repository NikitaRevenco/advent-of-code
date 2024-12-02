package main

import (
	"fmt"
	"os"
)

func read_input(day int) string {
	contents, err := os.ReadFile(fmt.Sprint("inputs/day_", day, ".txt"))
	if err != nil {
		panic(fmt.Sprint("Could not find input file for day", day))
	}

	return string(contents)
}

func assert_eq(lhs string, rhs string) {
	if lhs != rhs {
		panic(fmt.Sprint("Assertion failed for: ", lhs, " = ", rhs))
	}
}

func main() {
	assert_eq(day1_part1(read_input(1)), "1759")
	assert_eq(day1_part2(read_input(1)), "1805")
}
