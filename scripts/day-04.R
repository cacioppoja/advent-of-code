# Program: Advent of Code Day 04
# Date Created: 2025-12-09

## Part 1 ----------------------------------------------------------------------

find_all_symbols <- function(input) {
  
  # Convert to one character per matrix cell
  matrix <- matrix(
    unlist(strsplit(input, "")), 
    nrow = length(input),
    byrow = TRUE
  )
  
  all_symbols <- which(matrix == "@", arr.ind = TRUE)
  return(list(matrix, all_symbols))
}

check_all_adjacent <- function(input, all_symbols) {
  size <- dim(input)
  counter <- rep(0, dim(all_symbols)[1])
  # For each index in all_symbols, count how many adj
  for (i in 1:dim(all_symbols)[1]) {
    r <- all_symbols[i, 1]
    c <- all_symbols[i, 2]
    # Row above symbol
    if (r - 1 > 0) {
      # Column left of symbol
      if (c - 1 > 0) {
        if (input[r - 1, c - 1] == "@") {
          counter[i] <- counter[i] + 1
        }
      }
      # Column of symbol
      if (input[r - 1, c] == "@") {
        counter[i] <- counter[i] + 1
      }
      # Column right of symbol
      if (c + 1 <= dim(input)[2]) {
        if (input[r - 1, c + 1] == "@") {
          counter[i] <- counter[i] + 1
        }
      }
    }
    # Row of symbol
    # Column left of symbol
    if (c - 1 > 0) {
      if (input[r, c - 1] == "@") {
        counter[i] <- counter[i] + 1
      }
    }
    # Column right of symbol
    if (c + 1 <= dim(input)[2]) {
      if (input[r, c + 1] == "@") {
        counter[i] <- counter[i] + 1
      }
    }
    # Row below symbol
    if (r + 1 <= dim(input)[1]) {
      # Column left of symbol
      if (c - 1 > 0) {
        if (input[r + 1, c - 1] == "@") {
          counter[i] <- counter[i] + 1
        }
      }
      # Column of symbol
      if (input[r + 1, c] == "@") {
        counter[i] <- counter[i] + 1
      }
      # Column right of symbol
      if (c + 1 <= dim(input)[2]) {
        if (input[r + 1, c + 1] == "@") {
          counter[i] <- counter[i] + 1
        }
      }
    }
  }
  return(counter)
}

# Read example input
example_input <- readLines("data/day-04_part1_example.txt")
example_all_symbols <- find_all_symbols(example_input)
example_counter <- check_all_adjacent(example_all_symbols[[1]], example_all_symbols[[2]])
length(example_counter[example_counter < 4])

# Read real input
real_input <- readLines("data/day-04_part1.txt")
input_all_symbols <- find_all_symbols(real_input)
real_counter <- check_all_adjacent(input_all_symbols[[1]], input_all_symbols[[2]])
length(real_counter[real_counter < 4])

