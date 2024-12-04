parse_age_groups <- function(age_groups) {
  parse_age_group <- function(s) {
    if (grepl("-", s)) {
      bounds <- as.numeric(strsplit(s, "-")[[1]])
      lower <- bounds[1]
      upper <- bounds[2] + 0.9999  # Adjust upper bound slightly
    } else if (grepl("^<", s)) {
      upper <- as.numeric(sub("^<", "", s))
      lower <- -Inf
    } else if (grepl("\\+$", s)) {
      lower <- as.numeric(sub("\\+$", "", s))
      upper <- Inf
    } else {
      stop("Unrecognized format: ", s)
    }
    return(list(lower = lower, upper = upper))
  }
  
  # Parse each age group
  bounds_list <- lapply(age_groups, parse_age_group)
  
  # Create a data frame with age groups and their bounds
  age_df <- data.frame(
    age_group = age_groups,
    lower = sapply(bounds_list, function(x) x$lower),
    upper = sapply(bounds_list, function(x) x$upper)
  )
  
  return(age_df)
}


assign_age_group_vectorized <- function(ages, age_df) {
  # Initialize a vector to store age groups
  age_groups <- rep(NA, length(ages))
  
  # Convert ages to numeric vector
  ages <- as.numeric(ages)
  
  # Loop over age intervals
  for (i in seq_len(nrow(age_df))) {
    lower <- age_df$lower[i]
    upper <- age_df$upper[i]
    
    # Modify comparison to include the upper bound
    in_interval <- (is.infinite(lower) | ages >= lower) &
      (is.infinite(upper) | ages <= upper)
    
    # Assign the age group label to matching ages
    age_groups[in_interval] <- age_df$age_group[i]
  }
  
  return(age_groups)
}




