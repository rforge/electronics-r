#' The basics.
#'
#' @author Nathan Campos \email{nathanpc@@dreamintech.net}

#' Ohm's Law.
#'
#' @param v Voltage.
#' @param r Resistance.
#' @param i Current.
#' @return The resulting value depending on the parameters passed to the formula.
#'
#' @examples
#' ohms_law(v = 12, r = 1)   # Current for 12V across a 1 ohm resistor.
#' ohms_law(v = 5, i = 0.1)  # Resistance for 5V at 100mA.
ohms_law <- function (r = NULL, i = NULL, v = NULL) {
  # V = RI
  
  # Only allow 2 arguments to be specified.
  if (!is.null(r) & !is.null(i) & !is.null(v)) {
    stop("Too many arguments.")
  }

  if (!is.null(r) & !is.null(i)) {
    # Voltage.
    return(r * i)
  } else if (!is.null(v) & !is.null(r)) {
    # Current.
    return(v / r)
  } else if (!is.null(v) & !is.null(i)) {
    # Resistance.
    return(v / i)
  } else {
    stop("Not enough arguments.")
  }
}

#' Calculates the power.
#'
#' @param i Current.
#' @param v Voltage.
#' @param r Resistance.
#' @return The power in Watts.
#' 
#' @examples
#' power(v = 12, r = 10)  # Power dissipated by a 10 ohm resistor at 12V
power <- function (i = NULL, v = NULL, r = NULL) {
  # Only allow 2 arguments to be specified.
  if (!is.null(i) & !is.null(v) & !is.null(r)) {
    stop("Too many arguments.")
  }

  if (!is.null(i) & !is.null(v)) {
    # P = VI
    return(i * v)
  } else if (!is.null(i) & !is.null(r)) {
    # P = I^2 * R
    return((i^2) * r)
  } else if (!is.null(v) & !is.null(r)) {
    # P = V^2 / R
    return ((v^2) / r)
  } else {
    stop("Not enough arguments.")
  }
}