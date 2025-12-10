# Program: Advent of Code Day 01
# Date Created: 2025-12-01

## Part 1 ----------------------------------------------------------------------
dial_min <- 0
dial_max <- 99
start <- 50

turn_dial <- function(code) {
  # Separate out the direction from the count
  direction <- substr(code, 1, 1)
  direction <- ifelse(direction == "L", -1, ifelse(direction == "R", 1, 0))
  count <- as.numeric(substr(code, 2, nchar(code)))
  # Turn dial the correct direction and distance
  spin <- direction * count
 
  return(spin) 
}

count_stops <- function(stop_num) {
  counter_stop <- 0
  for (i in 1:length(all_dial_turns)) {
    start <- (start + all_dial_turns[i]) %% (dial_max - dial_min + 1)
    if (start == 0) {
      counter_stop <- counter_stop + 1
    }
  }
  return(counter_stop)
}

# Example Instructions
instructions <- readLines("data/day-01_part1_example.txt")
all_dial_turns <- turn_dial(instructions)

example_part1 <- count_stops(stop_num = 0)

# Real Instructions
real_instructions <- readLines("data/day-01_part1.txt")
all_dial_turns <- turn_dial(real_instructions)

answer <- count_stops(stop_num = 0)

## Part 2 ----------------------------------------------------------------------

count_passes <- function() {
  counter_pass <- 0
  for (i in 1:length(all_dial_turns)) {
    new <- start + all_dial_turns[i]
    passes <- abs(floor(new / (dial_max - dial_min + 1)))
    counter_pass <- counter_pass + passes
    start <- new %% (dial_max - dial_min + 1)
    print(paste(new, passes, counter_pass, start))
  }
  return(counter_pass)
}

all_dial_turns <- turn_dial(instructions)
example_part2 <- count_passes()

all_dial_turns <- turn_dial(real_instructions)
answer_part2 <- count_passes()
# 6173 too high

## Part 2, New Test ------------------------------------------------------------
all_dial_turns <- turn_dial(real_instructions[50:99])
test_part2 <- count_passes()

count_passes_v2 <- function() {
  counter_pass <- 0
  for (i in 1:length(all_dial_turns)) {
    new <- start + all_dial_turns[i]
    # Take absolute value prior to floor
    # floor(-3.4) = 4 but floor(3.4) = 3
    passes <- floor(abs(new / (dial_max - dial_min + 1)))
    # If it lands on 0 it still needs to count
    if (new == 0) {
      passes <- passes + 1
    }
    counter_pass <- counter_pass + passes
    print(
      paste("Start:", start, ",", "Add:", all_dial_turns[i], "i:", i,
                "New:", new, ",", "Passes:", passes, ",", "Total", counter_pass)
      )
    start <- new %% (dial_max - dial_min + 1)
  }
  return(counter_pass)
}

all_dial_turns <- turn_dial(real_instructions)
test_part2 <- count_passes_v2()
# 5072 is too low as suspected since I'm not counting when landing on 0

all_dial_turns <- turn_dial(real_instructions)
test_part2 <- count_passes_v2()
# 5482 is still too low

# But I didn't test on example and it is clearly wrong
all_dial_turns <- turn_dial(instructions)
example_part2_v2 <- count_passes_v2()

count_passes_v3 <- function() {
  counter_pass <- 0
  for (i in 1:length(all_dial_turns)) {
    new <- start + all_dial_turns[i]
    passes <- floor(abs(new / (dial_max - dial_min + 1)))
    if (passes == 0 & sign(new) != sign(start) & start != 0) {
      passes <- passes + 1
    }
    counter_pass <- counter_pass + passes
    print(
      paste("Start:", start, ",", "Add:", all_dial_turns[i], "i:", i,
            "New:", new, ",", "Passes:", passes, ",", "Total", counter_pass)
    )
    start <- new %% (dial_max - dial_min + 1)
  }
  return(counter_pass)
}
all_dial_turns <- turn_dial(instructions)
example_part2_v3 <- count_passes_v3()

real_instructions <- readLines("data/day-01_part1.txt", )
all_dial_turns <- turn_dial(real_instructions)
answer_part2_v3 <- count_passes_v3()
# 5879 is wrong

all_dial_turns <- turn_dial(c("L51", "L101"))
count_passes_v3()
