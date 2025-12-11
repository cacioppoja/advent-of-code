# Program: Advent of Code Day 09
# Date Created: 2025-12-10

## Part 1 ----------------------------------------------------------------------
# Read in example input
example_input <- readLines("data/day-09_part1_example.txt")
# Real input
input <- readLines("data/day-09_part1.txt")

convert_to_matrix <- function(input) {
  matrix <- matrix(
    as.numeric(unlist(strsplit(input, ","))),
    ncol = 2,
    byrow = TRUE
  )
  return(matrix)
}

# This works on the example, but it is way too slow to loop through each index
calculate_area <- function(input) {
  output <- data.frame(
    x1 = rep(0, dim(input)[1]**2),
    y1 = rep(0, dim(input)[1]**2),
    x2 = rep(0, dim(input)[1]**2),
    y2 = rep(0, dim(input)[1]**2),
    area = rep(0, dim(input)[1]**2)
  )
  k <- 0
  for (i in 1:dim(input)[1]) {
    for (j in 1:dim(input)[1]) {
      k <- k + 1
      output[k, c("x1", "y1")] <- input[i,]
      output[k, c("x2", "y2")] <- input[j,]
    }
  }
  output$area <- (abs(output$x1 - output$x2) + 1) * (abs(output$y1 - output$y2) + 1)
  return(output)
}

# This is much faster because it is vectorized
calculate_area <- function(input) {
  output <- data.frame(
    x1 = rep(input[,1], each = dim(input)[1]),
    y1 = rep(input[,2], each = dim(input)[1]),
    x2 = rep(input[,1], times = dim(input)[1]),
    y2 = rep(input[,2], times = dim(input)[1])
  )
  output$area <- (abs(output$x1 - output$x2) + 1) * (abs(output$y1 - output$y2) + 1)
  return(output)
}

# Example input
example_matrix <- convert_to_matrix(example_input)
example_area <- calculate_area(example_matrix)
max(example_area$area)

output_matrix <- convert_to_matrix(input)
output_area <- calculate_area(output_matrix)
max(output_area$area)
