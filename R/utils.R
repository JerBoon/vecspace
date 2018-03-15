#---------------------------------------------------------------------
# Just a bunch of generic Euclidean 3D utility functions
# They tend to all assume input vectors are trios of numbers (x,y,z)
#
# Inspired because it took me ages ages to work out that
# there are two different meanings for the term "cross product"
# the one which applies to 3D Euclidean geometry, and the statistical
# meaning, which is therefor more prevalent within R terminology.
# Who knew?
#---------------------------------------------------------------------


#---------------------------------------------------------------------
#' Cross Product of two direction vectors in 3D space
#'
#' @param a First direction vector
#' @param b Second direction vector
#'
#' @return Three element vector which is perpentical to the plane containing
#'   the two input vectors. Returns c(0,0,0) is the two inputs are parallel
#'   or if either are zero length.
#' 
#'  Note: Utils.CrossProduct(a, b) == - Utils.CrossProduct(b, a)
#'
#'  This function is specifically coded for 3D space, i.e. totally expects
#'  both a and b to be 3 element vectors
#'
#' @export
#'
#' @family utils
#'
#' @examples
#'   Utils.CrossProduct(c(3,2,1),c(1,2,3))  # [1]  4 -8  4
#'   Utils.CrossProduct(c(1,1,1),c(2,2,2))  # [1] 0 0 0
#'
#' @references
#'   \url{https://en.wikipedia.org/wiki/Cross_product}

Utils.CrossProduct <- function (a,b) {
  return(c(a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2] - a[2]*b[1]))
}


#---------------------------------------------------------------------
#' Dot (Scalar) Product of two direction vectors in 3D space
#'
#' @param a First direction vector
#' @param b Second direction vector
#'
#' @return Scalar value for dot rpdocut.
#'   Returns zero if the two inputs are perpendicular
#'   or if either are of zero length.
#'
#' @export
#'
#' @family utils
#'
#' @examples
#'   Utils.DotProduct(c(3,2,1),c(1,2,3))  # [1] 10
#'   Utils.DotProduct(c(1,1,0),c(1,-1,0)) # [1] 0
#'
#' @references
#'   \url{https://en.wikipedia.org/wiki/Dot_product}

Utils.DotProduct <- function (a,b) {
  return (sum(a*b))
}


#---------------------------------------------------------------------
#' The scalar length of a vector
#'
#' @param a The vector
#'
#' @return Scalar length - think Pythagorus in 3D.
#'   Will "work" on vectors of any number of dimensions, although it should
#'   be noted it's designed for 3D space use.
#'
#' @export
#'
#' @family utils
#'
#' @examples
#'   Utils.VectorLength(c(1,2,3))  # [1] 3.741657
#'   Utils.VectorLength(c(3,4,0))  # [1] 5

Utils.VectorLength <- function(a) {
  return (sqrt(sum(a^2)))
}


#---------------------------------------------------------------------
#' Convert input vector to unit length
#'
#' @param a The vector
#'
#' @return A vector in the same direction as the input vector, but of unit length
#'   Will "work" on vectors of any number of dimensions, although it should
#'   be noted it's designed for 3D space use.
#'   For expediency, the function doesn't check the input vector is non-zero
#'   in length, and will return a vector of NaN values in that case.
#'
#' @export
#'
#' @family utils
#'
#' @examples
#'   Utils.UnitVector(c(1,2,3))  [1] 0.2672612 0.5345225 0.8017837
#'   Utils.UnitVector(c(0,0,0)) [1] NaN NaN NaN

Utils.UnitVector <- function(a) {
  return (a / Utils.VectorLength(a))
}
