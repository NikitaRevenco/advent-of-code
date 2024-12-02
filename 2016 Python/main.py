import day_1


def main():
    assert day_1.part1(read_input(1)) == 278
    assert day_1.part2(read_input(1)) == 161


def read_input(day: int):
    file = open(f"inputs/day_{day}.txt", "r")
    contents = file.read()
    file.close()
    return contents


if __name__ == "__main__":
    main()
