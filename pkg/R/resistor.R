#' Resistors and resistance.
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/Electrical_resistance}
#'
#' @author Nathan Campos \email{nathanpc@@dreamintech.net}

#' Calculates the resistance in series.
#' @param rv Vector of resistors (Ohms).
#' @return Resistance value.
resistor.series <- function (rv) {
  rr = 0
  
  # Loop through the values.
  for (r in rv) {
    rr = rr + r
  }
  
  return(rr)
}

#' Calculates the resistance in parallel.
#' @param rv Vector of resistors (Ohms).
#' @return Resistance value.
resistor.parallel <- function (rv) {
  rr = 0
  
  # Loop through the values.
  for (r in rv) {
    rr = rr + (1 / r)
  }
  
  # Get the denominator.
  rr = rr ^ -1
  return(rr)
}