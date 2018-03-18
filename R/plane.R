
#---------------------------------------------------------------------
#' Return an elementary plane object
#'
#' @param point A point on the plane
#' @param normal A vector which is normal to the direction of the plane
#' @param properties Package-independent object defining additional plane properties.
#'     Default NA
#'
#' @return Elementary plane object. The function neither checks that the 
#'   3 input points are not colinear, nor that any of the 3 points are equal.
#'   Expect odd-ish results in either of those cases, since the resulting
#'   object will we either a line or a point with zero area.
#' 
#' @export
#'
#' @family constructors
#'
#' @examples
#'   pl <- Spc.MakePlane(c(0,0,0), c(0,1,0), "Dave")

Spc.MakePlane <- function (point, normal, properties=NA) {

  if ((typeof(point) != "double") ||
      length(point) != 3) {
    print("Spc.MakePlane: point should be a 3 number vector")
    return(NA)
  }
  if ((typeof(normal) != "double") ||
      length(normal) != 3) {
    print("Spc.MakePlane: normal should be a 3 number vector")
    return(NA)
  }

  r <- list(point=point,normal=normal)

  class(r) = append(class(r),"SpcPlane")

  if (!is.na(properties))
    attr(r,"properties") <- properties

  return(r)
} 

#==============================================================================

.Spc.Translate.SpcPlane <- function(plane, vector) {

  plane$point <- plane$point + vector

  return(plane)
}

#------------------------------------------------------------------------------
.Spc.Rotate.SpcPlane <- function(plane, pivot.point, pivot.rotMatrix) {

  plane$point <- (pivot.rotMatrix %*% (plane$point - pivot.point)) + pivot.point
  plane$normal <- pivot.rotMatrix %*% plane$normal

  return(plane)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcPlane <- function(plane) {

  return(NA)
}

#==============================================================================

.Spc.Polylines.SpcPlane <- function(plane, flatten=FALSE) {
  return (list())
}
