#' Inductors and inductance.
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/Inductor}
#'
#' @author Nathan Campos \email{nathanpc@@dreamintech.net}

#' Calculates the inductance in series.
#' @param lv Vector of inductors (Henry).
#' @return inductance value.
resistor.series <- function (lv) {
  lr = 0
  
  # Loop through the values.
  for (l in lv) {
    lr = lr + l
  }
  
  return(lr)
}

#' Calculates the inductance in parallel.
#' @param lv Vector of inductors (Henry).
#' @return inductance value.
resistor.parallel <- function (lv) {
  lr = 0
  
  # Loop through the values.
  for (l in lv) {
    lr = lr + (1 / l)
  }
  
  # Get the denominator.
  lr = lr ^ -1
  return(lr)
}