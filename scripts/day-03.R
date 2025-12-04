# Program: Advent of Code Day 03
# Date Created: 2025-12-03

## Part 1 ----------------------------------------------------------------------
# Read example input
example_input <- readLines("data/day-03_part1_example.txt")

# bank: Individual power bank
# num_batteries: total number in bank to turn on
# index_battery: specific index of battery to turn on s.t. 
#   1 <= index_battery <= num_batteries
# max_index: index of the previous max
get_joltage <- function(bank, num_batteries, index_battery, max_index) {
  max_value <- 0
  for (i in (max_index + 1):(nchar(bank) - (num_batteries - index_battery + 1) + 1)) {
    if (i == index_battery) {
      max_value <- as.integer(substr(bank, i, i))
      max_index <- i
    } else if (i > index_battery) {
      tmp_max <- as.integer(substr(bank, i, i))
      if (tmp_max > max_value) {
        max_value <- tmp_max
        max_index <- i
      }
    }
  }
  return(list(max_value, max_index))
}

bank_battery_on <- rep(0, length(example_input))
for (j in 1:length(example_input)) {
  battery1 <- get_joltage(example_input[j], 2, 1, 0)
  battery2 <- get_joltage(example_input[j], 2, 2, battery1[[2]])
  bank_battery_on[j] <- as.integer(paste0(battery1[[1]], battery2[[1]]))
}

example_answer <- sum(bank_battery_on)

# Answer
input <- readLines("data/day-03_part1.txt")
bank_battery_on <- rep(0, length(input))
for (j in 1:length(input)) {
  battery1 <- get_joltage(input[j], 2, 1, 0)
  battery2 <- get_joltage(input[j], 2, 2, battery1[[2]])
  bank_battery_on[j] <- as.integer(paste0(battery1[[1]], battery2[[1]]))
}

answer <- sum(bank_battery_on)
