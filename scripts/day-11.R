# Program: Advent of Code Day 11
# Date Created: 2026-01-13

## Part 1 ----------------------------------------------------------------------
# Read in example input
example_input <- readLines("data/day-11_part1_example.txt")
# Real input
input <- readLines("data/day-11_part1.txt")

# Convert to list
convert_to_list <- function(input) {
  outputs <- gsub("([a-z]+): (.*)", "\\2", input, ignore.case = TRUE)
  server_list <- strsplit(outputs, " ")
  names(server_list) <- gsub(
    "([a-z]+): (.*)", "\\1", input, ignore.case = TRUE
  )
  return(server_list)
}

# Follow the paths from inputs to outpus
follow_path <- function(input_list, input_df, start, end){
  output_df <- input_df
  outputs <- input_list[[start]]
  i <- 1
  # Move through full circuit until end criteria is reached
  while(is.null(outputs) == FALSE) {
    outputs_old <- outputs
    # Count how many times get to the end
    outputs_increase <- table(outputs)
    output_df[names(outputs_increase), "count"] <- 
      output_df[names(outputs_increase), "count"] + outputs_increase
    
    # New outputs to enter loop with are all inputs except end criteria
    outputs <- outputs[outputs != "out"]
    outputs <- unlist(
      lapply(outputs, \(x) input_list[[x]])
    )
    i <- i + 1
  }
  return(output_df)
}

## Run on example --------------------------------------------------------------
example_list <- convert_to_list(example_input)
example_dataframe <- data.frame(
  count = rep(0, length(example_list) + 1)
)
rownames(example_dataframe) <- c(names(example_list), "out")

example_output <- follow_path(example_list, example_dataframe, "you", "out")
example_output[rownames(example_output) == "out", ] 

## Run on input ----------------------------------------------------------------
input_list <- convert_to_list(input)
input_dataframe <- data.frame(
  count = rep(0, length(input_list) + 1)
)
rownames(input_dataframe) <- c(names(input_list), "out")

input_output <- follow_path(input_list, input_dataframe, "you", "out")
input_output[rownames(input_output) == "out", ]
