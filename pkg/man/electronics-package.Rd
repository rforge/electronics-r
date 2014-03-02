\name{electronics-package}
\alias{electronics-package}
\alias{electronics}
\docType{package}
\title{Electronics}
\description{
This package provides a lot of useful functions for electronics-related applications.
}
\details{
\tabular{ll}{
Package: \tab electronics\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-01-11\cr
License: \tab MIT License\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Maintainer: Nathan Campos <nathanpc@dreamintech.net>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
# Ohm's law.
ohms_law(v = 12, r = 1)   # Current for 12V across a 1 ohm resistor.
ohms_law(v = 5, i = 0.1)  # Resistance for 5V at 100mA.

# Power.
power(v = 12, r = 10)  # Power dissipated by a 10 ohm resistor at 12V

# RC filters.
filter.rc(10000, 0.0001)  # Cutoff frequency of a filter with a 10k ohms resistor and a 100uF capacitor.
filter.rc.r(1000, 0.0001)  # Resistor for a 1kHz filter with a 100uF capacitor.
filter.rc.r(1000, 10000)  # Capacitor for a 1kHz filter with a 10k ohms resistor.
}
