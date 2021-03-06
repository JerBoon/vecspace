% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{Utils.UnitVector}
\alias{Utils.UnitVector}
\title{Convert input vector to unit length}
\usage{
Utils.UnitVector(a)
}
\arguments{
\item{a}{The vector}
}
\value{
A vector in the same direction as the input vector, but of unit length
  Will "work" on vectors of any number of dimensions, although it should
  be noted it's designed for 3D space use.
  For expediency, the function doesn't check the input vector is non-zero
  in length, and will return a vector of NaN values in that case.
}
\description{
Convert input vector to unit length
}
\examples{
  Utils.UnitVector(c(1,2,3))  [1] 0.2672612 0.5345225 0.8017837
  Utils.UnitVector(c(0,0,0)) [1] NaN NaN NaN
}
\seealso{
Other utils: \code{\link{Utils.CrossProduct}},
  \code{\link{Utils.DotProduct}},
  \code{\link{Utils.VectorLength}}
}
