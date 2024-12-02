# https://adventofcode.com/2016/day/1


from enum import Enum
from functools import reduce
from typing import TypeAlias


Coordinates: TypeAlias = tuple[int, int]


class Direction(Enum):
    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3

    def turn_right(self):
        return Direction((self.value + 1) % 4)

    def turn_left(self):
        return Direction((self.value - 1) % 4)

    def from_turn(self, turn: str):
        match turn:
            case "R":
                return self.turn_right()
            case "L":
                return self.turn_left()
            case _:
                raise Exception(f"Expected 'L' or 'R' for Direction, but found {turn}")

    def compute_position_from_step(self, coords: Coordinates, distance: int):
        x, y = coords
        match self:
            case Direction.NORTH:
                return (x, y + distance)
            case Direction.EAST:
                return (x + distance, y)
            case Direction.SOUTH:
                return (x, y - distance)
            case Direction.WEST:
                return (x - distance, y)


State: TypeAlias = tuple[Coordinates, Direction]

InputChunk: TypeAlias = tuple[str, int]


def compute_state_from_step(current_state: State, next_step: InputChunk) -> State:
    # turn: "R" or "L", distance: how much we need to travel
    turn, distance = next_step
    # x, y: current coordinates, direction: current direction
    coords, direction = current_state

    new_direction = direction.from_turn(turn)

    new_position = new_direction.compute_position_from_step(coords, distance)

    return (new_position, new_direction)


def parse_input(input: str) -> map[InputChunk]:
    split = input.strip().split(", ")

    return map(lambda n: (n[:1], int(n[1:])), split)


def get_taxicab_distance(x: int, y: int) -> int:
    return abs(x) + abs(y)


initial_state = ((0, 0), Direction.NORTH)


def get_all_points_on_line(x1: int, y1: int, x2: int, y2: int) -> list[Coordinates]:
    """
    Obtain all integer points on a straight line between the first and the last point, and including the first and the last point
    """
    is_vertical_line = x2 - x1 == 0
    is_horizontal_line = y2 - y1 == 0

    if not (is_vertical_line) and not (is_horizontal_line):
        raise ValueError(
            f"Expected a straight line facing either North, South, West or East but found shape from ({x1}, {y1}) to ({x2}, {y2})"
        )

    start, end = (y1, y2) if is_vertical_line else (x1, x2)
    step_direction = 1 if end > start else -1

    range_of_values = range(start, end, step_direction)

    # nothing would change if we used x2 and y2 instead
    return [
        (x1, value) if is_vertical_line else (value, y1) for value in range_of_values
    ]


def part1(input: str):
    parsed = parse_input(input)

    ((x, y), __direction__) = reduce(
        compute_state_from_step, list(parsed), initial_state
    )

    return get_taxicab_distance(x, y)


def part2(input: str):
    parsed = parse_input(input)

    previous_states: list[State] = [initial_state]
    previous_positions: list[Coordinates] = []

    for turn, distance in list(parsed):
        coords, direction = previous_states[-1]
        new_direction = direction.from_turn(turn)

        new_position = new_direction.compute_position_from_step(coords, distance)

        new_state = (new_position, new_direction)

        for point in get_all_points_on_line(*coords, *new_position):
            if point in previous_positions:
                return get_taxicab_distance(*point)
            previous_positions.append(point)

        previous_states.append(new_state)
