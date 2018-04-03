
#---------------------------------------------------------------------
#' Return an elementary plane object
#'
#' @param point A point on the plane
#' @param normal A vector which is normal to the direction of the plane
#' @param properties Package-independent object defining additional plane properties.
#'     Default NA
#' @param direction.north,direction.east Nominal north and east directional vectors along the plane.
#'     If supplied, an intersect will also calculate and return components $north and $east, as the
#'     amount of direction of north and east the intersect is from the plane centre point.
#'     Alse, where supplied, none of north, east or normal should be parallel.
#'
#' @return Elementary plane object.
#' 
#' @export
#'
#' @family constructors
#'
#' @examples
#'   pl <- Spc.MakePlane(c(0,0,0), c(0,1,0), "Dave")

Spc.MakePlane <- function (point, normal, properties=NA, direction.north=NA, direction.east=NA) {

  #----- Some validation checks -----
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
  if (!is.na(direction.north) && 
      (typeof(direction.north) != "double" || length(direction.north) != 3 ||
       sum(Utils.CrossProduct(direction.north, normal)) == 0)) {
    print("Spc.MakePlane: if supplied, direction.north should be a 3 number vector, and which is not parallel to normal")
    return(NA)
  }
  if (!is.na(direction.east) && 
      (typeof(direction.east) != "double" || length(direction.east) != 3 ||
       sum(Utils.CrossProduct(direction.east, normal)) == 0 ||
       sum(Utils.CrossProduct(direction.east, direction.north)) == 0)) {
    print("Spc.MakePlane: if supplied, direction.east should be a 3 number vector, and which is not parallel to either the normal or direction.north")
    return(NA)
  }
  if (is.na(direction.north[1]) != is.na(direction.east[1])) {
    print("Spc.MakePlane: direction.north and direction.east should be either both supplied, or neither")
    return(NA)
  }

  #------

  r <- list(point=point,normal=normal,direction.north=direction.north,direction.east=direction.east)

  class(r) = append(class(r),"SpcPlane")

  if (!is.na(properties)[1])
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

  if (!is.na(plane$direction.north[1])) {
    plane$direction.north <- pivot.rotMatrix %*% plane$direction.north
    plane$direction.east <- pivot.rotMatrix %*% plane$direction.east
  }
  return(plane)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcPlane <- function(plane, approx) {

  if (approx) {
    #return a little bound around the point value
    return(list(plane$point - 0.1, plane$point + 0.1))
  }

  #Otherwise must return null, since an infinite plane cannot be bound
  return(NA)
}

#==============================================================================

.Spc.Polylines.SpcPlane <- function(plane, flatten=FALSE) {

  n <- Utils.UnitVector(plane$normal)
  nm  <- matrix(c(plane$point,plane$point+n), ncol=3, byrow=TRUE)
  attr(nm,"plot.type") <- "normal"

  return (list(nm))
}
