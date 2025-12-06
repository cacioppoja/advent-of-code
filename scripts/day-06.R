# Program: Advent of Code Day 06
# Date Created: 2025-12-06

# Part 1 -----------------------------------------------------------------------
# Read example input
example_input <- readLines("data/day-06_part1_example.txt")

# Read real input
real_input <- readLines("data/day-06_part1.txt")

length(example_input)
nchar(example_input)
length(real_input)
nchar(real_input)


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
