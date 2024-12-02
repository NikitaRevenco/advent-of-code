// https://adventofcode.com/2021/day/1

package main

import (
	"fmt"
	"strconv"
	"strings"
)

func parse_input(input string) []string {
	return strings.Split(strings.TrimSpace(input), "\n")
}

func string_to_int(input string) int {
	int, err := strconv.Atoi(input)
	if err != nil {
		panic(fmt.Sprint("Expected item to be a number, but got: ", input))
	}
	return int
}

func day1_part1(input string) string {
	parsed_input := parse_input(input)

	var greater_than_prev_count int

	for i := range parsed_input {
		// the first element does not have a previous value to compare to, so we skip
		if i == 0 {
			continue
		}

		prev := string_to_int(parsed_input[i-1])
		current := string_to_int(parsed_input[i])

		if current > prev {
			greater_than_prev_count++
		}
	}

	return strconv.Itoa(greater_than_prev_count)
}

func day1_part2(input string) string {
	parsed_input := parse_input(input)

	// the current window of 3 numbers summed together is greater than a window starting 1 index earlier
	var window3_greater_than_prev_count int

	for i := range parsed_input {
		if i < 3 {
			continue
		}

		first := string_to_int(parsed_input[i])
		second := string_to_int(parsed_input[i-1])
		third := string_to_int(parsed_input[i-2])
		fourth := string_to_int(parsed_input[i-3])

		window3_sum := first + second + third
		prev_window3_sum := second + third + fourth

		if window3_sum > prev_window3_sum {
			window3_greater_than_prev_count++
		}
	}

	return strconv.Itoa(window3_greater_than_prev_count)
}
