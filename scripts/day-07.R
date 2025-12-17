# Program: Advent of Code Day 07
# Date Created: 2025-12-08

## Part 1 ----------------------------------------------------------------------
# Read in example input
example_input <- readLines("data/day-07_part1_example.txt")
# Real input
input <- readLines("data/day-07_part1.txt")

# Convert to data frame
convert_to_matrix <- function(input) {
  df <- as.data.frame(
    matrix(
      unlist(strsplit(input, "")),
      nrow = length(input),
      byrow = TRUE
    )
  )
  # Start the beam at the S
  beam_col <- which(df[1,] == "S")
  df[2, beam_col] <- "|"
  return(df)
}

# Split action occurs when beam hits a splitter
split_beam <- function(input_list, beam_row, beam_col) {
  # Follow beam until it hits a splitter or end of data
  beam_split <- 0
  row_inc <- 1
  input <- input_list[[1]]
  split_df <- input_list[[2]]
  split_count <- input_list[[3]]
  while (!(beam_row + row_inc > dim(input)[1] | beam_split == 1)) {
    if (input[beam_row + row_inc, beam_col] == "^"){
      beam_split <- 1
      split_count <- split_count + 1
      split_df$split_row[split_count] <- beam_row + row_inc
      split_df$split_col[split_count] <- beam_col
      if (beam_col > 1) {
        input[beam_row + row_inc, beam_col - 1] <- "|"
      }
      if (beam_col < dim(input)[2]) {
        input[beam_row + row_inc, beam_col + 1] <- "|"
      }
    } else if (input[beam_row + row_inc, beam_col] == ".") {
      input[beam_row + row_inc, beam_col] <- "|"
    }
    row_inc <- row_inc + 1
  }
  return(list(input, split_df, split_count))
}

# Loop is reprocessing some rows, easiest is to just not count the duplicates
# Best practice would be to make this efficient instead
split_all_beams <- function(input) {
  split_df <- data.frame(
    split_row = rep(0, dim(input)[2]*dim(input)[2]),
    split_col = rep(0, dim(input)[2]*dim(input)[2])
  )
  split_count <- 0
  input_list <- list(input, split_df, split_count)
  # Here is why it does repeats
  # It checks every row, even though the splitter function carries the beam
  #  down more than one row
  for (i in 1:dim(input)[1]) {
    all_beam_col <- which(input[i,] == "|") 
    n_beams <- length(all_beam_col)
    if (n_beams != 0) {
      for (b in all_beam_col) {
        input_list <- split_beam(input_list, i, b)
        input <- input_list[[1]]
        split_df <- input_list[[2]]
        split_count <- input_list[[3]]
      }
    }
  }
  return(input_list)
}

# Run on example data
example_matrix <- convert_to_matrix(example_input)
test_example <- split_all_beams(example_matrix)

# To get unique splits, delete duplicate rows and the unused rows
test_example_unique_splits <- unique.data.frame(test_example[[2]])
dim(test_example_unique_splits[!(test_example_unique_splits$split_row == 0 & test_example_unique_splits$split_col == 0),])[1]

# Run on real data                               
input_matrix <- convert_to_matrix(input)
output_matrix <- split_all_beams(input_matrix)
output_unique_splits <- unique.data.frame(output_matrix[[2]])
dim(output_unique_splits[!(output_unique_splits$split_row == 0 & output_unique_splits$split_col == 0), ])[1]
