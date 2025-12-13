# Program: Advent of Code Day 08
# Date Created: 2025-12-11

## Part 1 ----------------------------------------------------------------------
# Read in example input
example_input <- readLines("data/day-08_part1_example.txt")
# Real input
input <- readLines("data/day-08_part1.txt")

# Convert data to matrix
convert_to_matrix <- function(input) {
  matrix <- matrix(
    as.numeric(unlist(strsplit(input, ","))),
    ncol = 3,
    byrow = TRUE
  )
  return(matrix)
}

# Matrix pairs
pairs <- function(input) {
  dim_row <- dim(input)[1]
  output <- data.frame(
    x1 = rep(input[, 1], each = dim_row),
    y1 = rep(input[, 2], each = dim_row),
    z1 = rep(input[, 3], each = dim_row),
    x2 = rep(input[, 1], times = dim_row),
    y2 = rep(input[, 2], times = dim_row),
    z2 = rep(input[, 3], times = dim_row),
    # I can't remember why, but I referred to a pair of boxes as a row and col,
    # where the first box in the pair is row and second is col
    row = rep(1:dim_row, each = dim_row),
    col = rep(1:dim_row, times = dim_row)
  )
  # Remove the self pairs
  # output <- output[rowSums(output[,1:3] == output[,4:6]) != dim(input)[2],]
  # Remove the repeat diagonal
  row_keep <- c()
  for (i in 1:(dim_row - 1)) {
    row_keep <- c(row_keep, ((dim_row * (i - 1) + i + 1):(dim_row * i)))
  }
  output <- output[row_keep, ]
  return(output)
}

# Calculate Euclidean distance
distance <- function(x1, y1, z1, x2, y2, z2) {
  distance <- sqrt(((x1 - x2)**2) + ((y1 - y2)**2) + ((z1 - z2)**2))
  return(distance)
}

get_circuits <- function(input_dist, input_matrix, n_circuit_max){
  dim_nrow <- dim(input_matrix)[1]
  df <- as.data.frame(input_matrix)
  df$circuit <- rep(0, dim_nrow)
  circuit_count <- 1
  for (i in 1:n_circuit_max) {
    # Set the first circuit to 1
    if (i == 1) {
      df$circuit[dist_matrix[i, "row"]] <- 1
      df$circuit[dist_matrix[i, "col"]] <- 1
    } else if (i > 1) {
      # Get the two pairs in the distance matrix
      row <- dist_matrix[i, "row"]
      col <- dist_matrix[i, "col"]
      # Get the current circuit for each junction box
      row_circuit <- df$circuit[row]
      col_circuit <- df$circuit[col]
      
      # If one of the boxes is part of a circuit, then the other joins it
      if (row_circuit > 0 & col_circuit == 0) {
        df$circuit[col] <- row_circuit
      } else if (row_circuit == 0 & col_circuit > 0) {
        df$circuit[row] <- col_circuit
      } else if (row_circuit == 0 & col_circuit == 0) {
        # If neither is in a circuit, then they create a new circuit together
        circuit_count <- circuit_count + 1
        df$circuit[row] <- circuit_count
        df$circuit[col] <- circuit_count
      } else if (row_circuit >  0 & col_circuit > 0) {
        # If they are on the same circuit then do nothing,
        #  otherwise join all items on the circuit together
        if (row_circuit != col_circuit){
          circuit_count <- circuit_count + 1
          df[df$circuit == row_circuit, "circuit"] <- circuit_count
          df[df$circuit == col_circuit, "circuit"] <- circuit_count
        }
      }
    }
  }
  return(df)
}

calculate_circuit_size <- function(input, n_size) {
  # To get the number of total circuits:
  #  separate count is number of unique above 0, and each 0 is unique
  length(unique(example_answer$circuit[example_answer$circuit > 0])) +
    length(example_answer$circuit[example_answer$circuit == 0 ]) 
  
  # Number of boxes per circuit
  circuit_size <- as.data.frame(table(input$circuit))
  colnames(circuit_size)[1] <- "circuit_num"
  circuit_size <- circuit_size[rev(order(circuit_size$Freq)),]
  # Remove circuit 0 because those are all unconnected boxes
  circuit_size <- circuit_size[circuit_size$circuit_num != 0, ]
  
  # Top size circuits
  top_n_circuits <- circuit_size[1:n_size,]
  answer <- eval(parse(text = paste(top_n_circuits$Freq, collapse = "*")))
  
  return(answer)
}

# Run on example
example_matrix <- convert_to_matrix(example_input)
example_diag <- pairs(example_matrix)
example_dist <- distance(
  example_diag$x1, example_diag$y1, example_diag$z1, 
  example_diag$x2, example_diag$y2, example_diag$z2
)

dist_matrix <- example_diag
dist_matrix$distance <- example_dist
dist_matrix <- dist_matrix[order(dist_matrix$distance),]

example_answer <- get_circuits(dist_matrix, example_matrix, n_circuit_max = 10)

calculate_circuit_size(example_answer, 3)


# Run on input
input_matrix <- convert_to_matrix(input)
input_diag <- pairs(input_matrix)
input_dist <- distance(
  input_diag$x1, input_diag$y1, input_diag$z1, 
  input_diag$x2, input_diag$y2, input_diag$z2
)

dist_matrix <- input_diag
dist_matrix$distance <- input_dist
dist_matrix <- dist_matrix[order(dist_matrix$distance),]

input_answer <- get_circuits(dist_matrix, input_matrix, n_circuit_max = 1000)

calculate_circuit_size(input_answer, 3)
