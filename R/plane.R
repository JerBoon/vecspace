
#==============================================================================

Spc.MakePlane <- function (point, normal) {

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
  return(r)
} 

#==============================================================================

.Spc.Translate.SpcPlane <- function(vector, plane) {

  plane$point <- plane$point + vector

  return(plane)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcPlane <- function(plane) {

  return(NA)
}

#==============================================================================

.Spc.Polylines.SpcPlane <- function(plane) {
  return (list())
}
