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
