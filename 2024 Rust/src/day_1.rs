/// Splits two columns of numbers into two vectors, where each vector corresponds to a column
///
/// # Examples
///
/// ```
/// let input = "\
/// 1    4
/// 2  5
/// 3   6";
///
/// assert_eq!(split_columns(input), (vec![1, 2, 3], vec![4, 5, 6]));
/// ```
///
/// # Panics
///
/// - If any of the lines of the input do not have at least 2 items separated by whitespace
/// - If any of the 2 items cannot be parsed as a number
pub fn split_columns(input: &str) -> (Vec<isize>, Vec<isize>) {
    input
        .lines()
        .map(|line| {
            line.split_once("   ")
                .and_then(|(first, second)| {
                    Some((
                        first.parse::<isize>().expect("First value is a number"),
                        second.parse::<isize>().expect("Second value is a number"),
                    ))
                })
                .expect("Expected two numbers in each column separated by 3 spaces")
        })
        .unzip()
}

// https://adventofcode.com/2024/day/1
pub fn part1(input: String) -> isize {
    let (mut column_1, mut column_2) = split_columns(&input);

    column_1.sort();
    column_2.sort();

    column_1
        .into_iter()
        .zip(column_2)
        .map(|(first, second)| isize::abs(second - first))
        .sum()
}

// https://adventofcode.com/2024/day/1#part2
pub fn part2(input: String) -> isize {
    let (column_1, column_2) = split_columns(&input);

    column_1
        .iter()
        .map(|&number| number * column_2.iter().filter(|&&num| num == number).count() as isize)
        .sum()
}
