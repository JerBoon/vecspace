

#---------------------------------------------------------------------
#' Return an elementary triangle object
#'
#' @param A,B,C A, B, and C define the corners of the triangle in space
#' @param properties Package-independent object defining additional triangle properties.
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

.Spc.Translate.SpcTriangle <- function(triangle, vector) {

  triangle$A <- triangle$A + vector
  triangle$B <- triangle$B + vector
  triangle$C <- triangle$C + vector

  return(triangle)
}

#------------------------------------------------------------------------------
.Spc.Rotate.SpcTriangle <- function(triangle, pivot.point, pivot.rotMatrix) {

  triangle$A <- (pivot.rotMatrix %*% (triangle$A - pivot.point)) + pivot.point
  triangle$B <- (pivot.rotMatrix %*% (triangle$B - pivot.point)) + pivot.point
  triangle$C <- (pivot.rotMatrix %*% (triangle$C - pivot.point)) + pivot.point

  return(triangle)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcTriangle <- function(triangle, approx) {

  return(list(pmax(triangle$A,triangle$B,triangle$C),
              pmin(triangle$A,triangle$B,triangle$C)))
}

#==============================================================================
# verts defines which sides to return in the list - by default will draw a>B>C>D
# which is the whole triangle. Otherwise can be overridden as needed

.Spc.Polylines.SpcTriangle <- function(triangle, flatten=FALSE, verts="ABCA") {

  # The triangle itself
  r = matrix(c(triangle$A,triangle$B,triangle$C,triangle$A),ncol=3,byrow=TRUE)
  attr(r,"plot.type") <- "object"

  r <- numeric(0)
  for (a in strsplit(verts,"")[[1]])  {
    if (a == "A") r <- append(r, triangle$A)
    else if (a == "B") r <- append(r, triangle$B)
    else if (a == "C") r <- append(r, triangle$C)
  }
  r <- matrix(r,ncol=3,byrow=TRUE)
  attr(r,"plot.type") <- "object"
    

  #The normal; tc = triangle centre; n = normal
  tc <- (triangle$A + triangle$B + triangle$C) /3
  n <- Utils.UnitVector(Utils.CrossProduct(triangle$B - triangle$A, triangle$C - triangle$A))
  nm  <- matrix(c(tc,tc+n), ncol=3, byrow=TRUE)
  attr(nm,"plot.type") <- "normal"

  return (list(r,nm))
}



