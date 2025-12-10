# Program: Advent of Code Day 04
# Date Created: 2025-12-09

## Part 1 ----------------------------------------------------------------------
# Find the indices of the symbols
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

# Check all adjacent indices of the symbols to count how many are removable
check_all_adjacent <- function(input, all_symbols) {
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

## Part 2 ----------------------------------------------------------------------
# Same function as Part 1 except the input starts as a matrix
find_all_symbols <- function(input) {
  all_symbols <- which(input == "@", arr.ind = TRUE)
  return(list(input, all_symbols))
}
# Same function as Part 1 except the counter is added as a column to the input
check_all_adjacent <- function(input, all_symbols) {
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
  output_all_symbols <- cbind(all_symbols, counter)
  return(output_all_symbols)
}

# Remove the symbols that are removable
remove_symbols <- function(input, counter) {
  # For each index in counter, remove the roll if possible
  #  i.e. if < 4 replace @ with x
  for (i in 1:dim(counter)[1]) {
    r <- counter[i, 1]
    c <- counter[i, 2]
    if (counter[i, 3] < 4) {
      input[r, c] <- "x"
    }
  }
  return(input)
}

# Example input
# Convert to one character per matrix cell
example_matrix <- matrix(
  unlist(strsplit(example_input, "")), 
  nrow = length(example_input),
  byrow = TRUE
)
example_all_symbols <- find_all_symbols(example_matrix)
example_counter <- check_all_adjacent(example_all_symbols[[1]], example_all_symbols[[2]])
# sum(example_counter[,3] < 4)
example_update <- remove_symbols(example_all_symbols[[1]], example_counter)

total_counter <- 0
total_counter <- total_counter + sum(example_counter[,3] < 4)
while (any(example_counter[,3] < 4)) {
  example_all_symbols <- find_all_symbols(example_update)
  example_counter <- check_all_adjacent(example_all_symbols[[1]], example_all_symbols[[2]])
  print(example_counter[,3])
  print(sum(example_counter[,3] < 4))
  example_update <- remove_symbols(example_all_symbols[[1]], example_counter)
  total_counter <- total_counter + sum(example_counter[,3] < 4)
}
total_counter

# Real Input
real_matrix <- matrix(
  unlist(strsplit(real_input, "")), 
  nrow = length(real_input),
  byrow = TRUE
)
input_all_symbols <- find_all_symbols(real_matrix)
real_counter <- check_all_adjacent(input_all_symbols[[1]], input_all_symbols[[2]])
# sum(real_counter[,3] < 4)
real_update <- remove_symbols(input_all_symbols[[1]], real_counter)

total_counter <- 0
total_counter <- total_counter + sum(real_counter[,3] < 4)
while (any(real_counter[,3] < 4)) {
  input_all_symbols <- find_all_symbols(real_update)
  real_counter <- check_all_adjacent(input_all_symbols[[1]], input_all_symbols[[2]])
  print(sum(real_counter[,3] < 4))
  real_update <- remove_symbols(input_all_symbols[[1]], real_counter)
  total_counter <- total_counter + sum(real_counter[,3] < 4)
}
total_counter
