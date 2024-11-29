
# Function to parse age group strings into lower and upper bounds
parse_age_groups <- function(age_groups) {
  parse_age_group <- function(s) {
    if (grepl("-", s)) {
      bounds <- as.numeric(strsplit(s, "-")[[1]])
      lower <- bounds[1]
      upper <- bounds[2]
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

