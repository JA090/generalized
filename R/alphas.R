
#' Function to organize user-entered alpha levels.
#' @param a0 Up to 3 alpha levels.
#' @param a1 Up to 3 alpha levels.
#' @param a2 Up to 3 alpha levels.
#' @return A numeric vector of confidence levels of length 3, ordered in increasing order with repetitions of the smallest value.
#' @description Duplicate values if needed to make 3. An error message is provided if there is not at least one valid level.

alphas <- function(a0, a1, a2) {
  alps <- NA
  if (!is.na(as.numeric(a0))) {
    alps <- as.numeric(a0)
    if (!is.na(as.numeric(a1)))
    {
      alps <- c(alps, as.numeric(a1))
      if (!is.na(as.numeric(a2)))
        alps <- c(alps, as.numeric(a2))
    }
    else if (!is.na(as.numeric(a2)))
      alps <- c(alps, as.numeric(a2))
  }
  else {
    if (!is.na(as.numeric(a1))) {
      alps <- as.numeric(a1)
      if (!is.na(as.numeric(a2)))
        alps <- c(alps, as.numeric(a2))
    }
    else if (!is.na(as.numeric(a2)))
      alps <- as.numeric(a2)
  }



  # force values into valid range and duplicate if needed so there are always three alpha levels
  if (!is.na(alps[1])) {
    for (k in 1:length(alps))
      alps[k] =round(min(.99999, max(0, alps[k])), 4)
    # convert and sort
    alps <- sort(1 - alps)

    if (length(alps) == 1) {
      alps[2] = alps[1]
      alps[3] = alps[1]
    }
    if (length(alps) == 2) {
      alps[3] = alps[2]
      alps[2] = alps[1]
    }
  }

  # Provide error message if no valid level entered
  validate(need(!is.na(alps[1]), "Please enter at least one positive test level"))
  return(alps)
}
