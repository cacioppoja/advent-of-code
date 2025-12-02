# Program: Advent of Code Day 02
# Date Created: 2025-12-02

## Part 1 ----------------------------------------------------------------------
# Read example input
example_input <- read.csv("data/day-02_part1_example.txt", header = FALSE)

get_numbers_in_sequence <- function(data_raw){
  data_raw2 <- data.frame(
    matrix(
      unlist(strsplit(data_raw[,1], split = "-")), 
      ncol = 2,
      byrow = TRUE
    )
  )
  colnames(data_raw2) <- c("start", "end")
  number_list <- vector("list", nrow(data_raw2))
  for (i in 1:nrow(data_raw2)) {
    number_list[[i]] <- data_raw2[i, "start"]:data_raw2[i, "end"]
  }
  data_cle <- as.data.frame(unlist(number_list))
  colnames(data_cle) <- "id"
  return(data_cle)
}

repeated_digits <- function(input_data) {

  # Only numbers with an even number of digits will work
  # E.g. 10's not 100's, 1,000's not 10,000's
  input_data$num_digits <- nchar(input_data$id)
  input_data$even_digits <- ifelse(input_data$num_digits %% 2 == 0, 1, 0)
  # First half of number and second half need to match so split it
  # E.g. 6464 = 6400 + 64 = 64*10^2 + 64
  input_data$first_half <- floor(
    input_data$id / (10^(input_data$num_digits / 2))
  )
  input_data$second_half <- input_data$id %% (10^(input_data$num_digits / 2))
  input_data$is_repeat_digit <- ifelse(
    input_data$first_half == input_data$second_half & 
      input_data$even_digits == 1, 1, 0
    )
  return(input_data)
}

which_repeats <- function(input_data) {
  input_data[input_data$is_repeat_digit == 1,]
}

# Test on example input
example_sequence <- get_numbers_in_sequence(example_input)
example_repeats <- repeated_digits(example_sequence)
sum(which_repeats(example_repeats)$id)

# Read in real data
input <- read.csv("data/day-02_part1.txt", header = FALSE)
# Find the answer
answer_sequence <- get_numbers_in_sequence(input)
answer_repeats <- repeated_digits(answer_sequence)
sum(which_repeats(answer_repeats)$id)
