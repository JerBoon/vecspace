

#---------------------------------------------------------------------
#' Return an elementary triangle object
#'
#' @param A,B,C A, B, and C define the corners of the triangle in space
#' @param properties Package-independent object defining additional sphere properties
#'     Default NA
#'
#' @return Elementary triangle object. The function neither checks that the 
#'   3 input points are not colinear, nor that any of the 3 points are equal.
#'   Expect odd-ish results in either of those cases, since the resulting
#'   object will we either a line or a point with zero area.
#' 
#' @export
#'
#' @family constructors
#'
#' @examples
#'   t <- Spc.MakeTriangle(c(0,0,0), c(1,1,1), c(2,0,10), surface_props)

Spc.MakeTriangle <- function (A,B,C, properties=NA) {

  if ((typeof(A) != "double") || (length(A) != 3) ||
      (typeof(B) != "double") || (length(B) != 3) ||
      (typeof(C) != "double") || (length(C) != 3)) {
    print("Spc.MakeTriangle: each point should be a 3 number vector")
    return(NA)
  }

  r <- list(A=A, B=B, C=C)
  class(r) = append(class(r),"SpcTriangle")

  if (!is.na(properties))
    attr(r,"properties") <- properties

  return(r)
} 

#------------------------------------------------------------------------------

.Spc.Translate.SpcTriangle <- function(vector, triangle) {

  triangle$A <- triangle$A + vector
  triangle$B <- triangle$B + vector
  triangle$C <- triangle$C + vector

  return(triangle)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcTriangle <- function(triangle) {

  return(list(pmax(triangle$A,triangle$B,triangle$C),
              pmin(triangle$A,triangle$B,triangle$C)))
}

#==============================================================================

.Spc.Polylines.SpcTriangle <- function(triangle) {

  r = matrix(c(triangle$A,triangle$B,triangle$C,triangle$A),ncol=3,byrow=TRUE)
  return (list(r))
}



