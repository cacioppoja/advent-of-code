# Program: Advent of Code Day 06
# Date Created: 2025-12-06

# Part 1 -----------------------------------------------------------------------
# Read example input
example_input <- readLines("data/day-06_part1_example.txt")

# Read real input
real_input <- readLines("data/day-06_part1.txt")

input_to_dataframe <- function(input) {
  # Collapse out white space
  input_list <- strsplit(input, " ")
  # Separate operators
  operator_list <- input_list[[length(input)]]
  # Keep only numbers in data frame
  input_list[[length(input)]] <- NULL
  df <- as.data.frame(sapply(input_list, \(x) as.numeric(x[x != ""])))
  operator <- operator_list[operator_list != ""]
  
  # Separate into multiple data frames by operator
  operator_unique <- unique(operator)
  df_list <- vector("list", length(operator_unique))
  for (i in 1:length(operator_unique)) {
    df_list[[i]] <- df[operator == operator_unique[i],]
  }
  return(list(operator, operator_unique, df_list))
}

apply_operator <- function(operator, operator_unique, df) {
  total <- vector("list", length(operator_unique))
  for (i in 1:length(operator_unique)) {
    total[[i]] <- eval(parse(text = paste(df[[i]], collapse = operator_unique[i])))
  }
  return(total)
}

example_clean <- input_to_dataframe(example_input)
example_answer <- sum(
  unlist(
    apply_operator(example_clean[[1]], example_clean[[2]], example_clean[[3]])
  )
)

input_clean <- input_to_dataframe(real_input)
answer <- sum(
  unlist(
    apply_operator(input_clean[[1]], input_clean[[2]], input_clean[[3]])
  )
)

# Part 2 -----------------------------------------------------------------------
cephalopod_math <- function(input) {
  # Each character in row,col
  matrix <- matrix(
    unlist(strsplit(input, "")), 
    nrow = length(input),
    byrow = TRUE
  )
  
  # Index of first operator character
  first_index <- which(matrix[dim(matrix)[1],] != " ")
  # Last is 2 before the first because of column space separator
  last_index <- c(first_index[2:length(first_index)] - 2, dim(matrix)[2] - 1)
  last_index[length(last_index)] <- last_index[length(last_index)] + 1
  
  # Repeat the operator character and separate columns by |
  clean_matrix <- matrix
  for (i in 1:length(first_index)) {
    char <- matrix[dim(matrix)[1], first_index[i]]
    clean_matrix[dim(matrix)[1], first_index[i]:(last_index[i] - 1)] <- char
    clean_matrix[dim(matrix)[1], last_index[i]] <- "|"
  }
  # Write the equations
  equation <- unlist(
    strsplit(paste(clean_matrix, collapse = ""), split = "|", fixed = TRUE)
  )
  total <- 0
  for (i in 1:length(equation)) {
    total <- total + eval(parse(text = equation[i]))
  }
  return(list(matrix, clean_matrix, equation, total))
}

example_part2 <- cephalopod_math(example_input)
real_part2 <- cephalopod_math(real_input)
format(real_part2[[4]], scientific = FALSE)
