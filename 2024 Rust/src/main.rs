mod day_1;

fn main() {
    assert_eq!(day_1::part1(read_input(1)), 2192892);
    assert_eq!(day_1::part2(read_input(1)), 22962826);
}

fn read_input(day: usize) -> String {
    std::fs::read_to_string(format!("./inputs/day_{}.txt", day)).unwrap()
}
