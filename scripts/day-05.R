# Program: Advent of Code Day 05
# Date Created: 2025-12-05

## Part 1 ----------------------------------------------------------------------
# Read example input
example_input <- readLines("data/day-05_part1_example.txt")

# Separate ranges from ingredients then check if in range
is_fresh_ingredient <- function(input) {
  list_break <- which(input == "")
  fresh_ranges <- input[1:(list_break - 1)]
  ids <- as.numeric(input[(list_break + 1):length(input)])

  fresh_min_max <- as.data.frame(
    matrix(unlist(strsplit(fresh_ranges, "-")), ncol = 2, byrow = TRUE)
  )
  colnames(fresh_min_max) <- c("range_min", "range_max")
  fresh_min_max$range_min <- as.numeric(fresh_min_max$range_min)
  
  fresh_min_max$range_max <- as.numeric(fresh_min_max$range_max)
  fresh_id <- data.frame(
    id = ids,
    is_fresh = rep(0, length(ids)),
    fresh_range_index = rep("", length(ids))
  )
  for (i in 1:dim(fresh_min_max)[1]) {
    for (j in 1:length(ids)) {
      if (ids[j] <= fresh_min_max$range_max[i] &
        ids[j] >= fresh_min_max$range_min[i]) {
        fresh_id[j, "is_fresh"] <- 1
        if (fresh_id$fresh_range_index[j] == "") {
          fresh_id[j, "fresh_range_index"] <- i
        } else {
          fresh_id[j, "fresh_range_index"] <- paste(
            fresh_id[j, "fresh_range_index"], i,
            sep = "|"
          )
        }
      }
    }
  }
  # return(list(fresh_id, fresh_ranges, fresh_min_max, ids))
  return(list(fresh_id, fresh_min_max))
}
example_fresh_ids <- is_fresh_ingredient(example_input)
example_answer <- sum(example_fresh_ids[[1]]$is_fresh)

# Read real instructions
real_input <- readLines("data/day-05_part1.txt")
real_fresh_ids <- is_fresh_ingredient(real_input)
real_answer <- sum(real_fresh_ids[[1]]$is_fresh)

## Part 2 ----------------------------------------------------------------------
get_all_numbers <- function(input) {
  # Sort so the lowest starting ranges are first
  input_sort <- input[order(input$range_min),]
  
  # Count unique values at each step
  input_sort$counter <- rep(0, dim(input_sort)[1])

  for (i in 1:dim(input_sort)[1]) {
    if (i == 1) {
      input_sort$new_min[i] <- input_sort$range_min[i]
      input_sort$new_max[i] <- input_sort$range_max[i]
      input_sort$counter[i] <- input_sort$new_max[i] - input_sort$new_min[i] + 1
    } else {
      # If the min and max are less than the previous max, then the whole range
      #  is already counted
      # Adjust the min i and max i to the max i-1 and do not increase counter
      if (input_sort$range_min[i] <= input_sort$new_max[i-1] &
          input_sort$range_max[i] <= input_sort$new_max[i-1]) {
        input_sort$new_min[i] <- input_sort$new_max[i-1]
        input_sort$new_max[i] <- input_sort$new_max[i-1]
        input_sort$counter[i] <- 0
        
      }
      # If min is less than max i-1 and max is gt max i-1 there are new digits
      else if (input_sort$range_min[i] <= input_sort$new_max[i-1] &
                 input_sort$range_max[i] > input_sort$new_max[i-1]) {
          input_sort$new_min[i] <- 1 + input_sort$new_max[i-1]
          input_sort$new_max[i] <- input_sort$range_max[i]
          input_sort$counter[i] <- input_sort$new_max[i] - input_sort$new_min[i] + 1
      }
      # If min is gt than max i-1 then the whole range is new
      else if (input_sort$range_min[i] > input_sort$new_max[i-1]) {
        input_sort$new_min[i] <- input_sort$range_min[i]
        input_sort$new_max[i] <- input_sort$range_max[i]
        input_sort$counter[i] <- input_sort$new_max[i] - input_sort$new_min[i] + 1
      }
    }
    
  }
  return(input_sort)
}
# Example
# Get the fresh ranges from part 1
example_fresh_ranges <- example_fresh_ids[[2]]
example_part2 <- get_all_numbers(example_fresh_ranges)
sum(example_part2$counter)

# Create new test set with more cases
test_case2 <- example_fresh_ids[[2]]
test_case2[5:9, ] <- data.frame(
  range_min = c(3, 25, 27, 31, 16),
  range_max = c(5, 30, 28, 31, 17)
)
test_case2_output <- get_all_numbers(test_case2)
sum(test_case2_output$counter)

# Real answer
real_fresh_ranges <- real_fresh_ids[[2]]
answer_part2 <- get_all_numbers(real_fresh_ranges)
format(sum(answer_part2$counter), scientific = FALSE)
