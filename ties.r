tie_checker <- function(vector_in) {
  # Returns a list of the positions of ties, in clusters.
  # (i.e. a cluster is a list of positions in which all the elements
  # at those positions are equal)
  tie_positions <- list()
  len <- length(vector_in)
  
  # For each position in the vector, search along the rest of the vector
  # and count the ties, and record where they are
  for (i in seq(len - 1)) {
    # Create a vector which records all the positions in a particular
    # cluster:
    tie_cluster <- c(i)
    
    # If we've already recorded a tie in this position we can move on:
    if (i %in% unlist(tie_positions)) {
      next
    }
    else {
      for (j in seq(i + 1, len)) {
        # If we find a tie, add it to the cluster
        if (vector_in[i] == vector_in[j]) {
          tie_cluster <- c(tie_cluster, j)
        }
      }
      # If the only element in the cluster is the one we added
      # at the start of the iteration, there are no ties
      if (length(tie_cluster) == 1) {
        next
      } else {
        # Otherwise we can add the cluster to the end of tie_positions
        tie_positions[[length(tie_positions) + 1]] <- tie_cluster
      }
    }
  }
  
  if (length(tie_positions) == 0) {
    print("There are no ties.")
  } else {
    print("There are ties in the following positions:")
    print(tie_positions)
  }
  
  invisible(tie_positions)
}
