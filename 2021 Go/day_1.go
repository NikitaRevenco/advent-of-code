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

func day1_part1(input string) string {
	parsed_input := parse_input(input)

	var greater_than_prev_count int

	for i, line := range parsed_input {
		// the first element does not have a previous value to compare to, so we skip
		if i == 0 {
			continue
		}

		prev, err := strconv.Atoi(parsed_input[i-1])
		if err != nil {
			panic(fmt.Sprint("Expected item to be a number, but got: ", parsed_input[i-1]))
		}

		current, err := strconv.Atoi((line))
		if err != nil {
			panic(fmt.Sprint("Expected item to be a number, but got: ", line))
		}

		if current > prev {
			greater_than_prev_count++
		}
	}

	return strconv.Itoa(greater_than_prev_count)
}

func day1_part2(input string) string {
	parsed_input := parse_input(input)

	var lol int

	for i := range parsed_input {
		if i == 0 || i == 1 || i == 2 {
			continue
		}
		first, err := strconv.Atoi(parsed_input[i])
		if err != nil {
			panic(fmt.Sprint("Expected item at position ", i, " to be an integer"))
		}
		second, err := strconv.Atoi(parsed_input[i-1])
		if err != nil {
			panic(fmt.Sprint("Expected item at position ", i, " to be an integer"))
		}
		third, err := strconv.Atoi(parsed_input[i-2])
		if err != nil {
			panic(fmt.Sprint("Expected item at position ", i, " to be an integer"))
		}
		fourth, err := strconv.Atoi(parsed_input[i-3])
		if err != nil {
			panic(fmt.Sprint("Expected item at position ", i, " to be an integer"))
		}
		sum := first + second + third
		prev_sum := second + third + fourth
		fmt.Println(first, second, third)

		if sum > prev_sum {
			lol++
		}
	}

	return strconv.Itoa(lol)
}
