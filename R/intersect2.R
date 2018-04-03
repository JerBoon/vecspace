# This is the internal library of compound intersect methods

#==============================================================================

#this is the top level intersect, for a compount list of objects.
#The world, and indeed any sub-object can be a simple list of objects
#In which case we need to traverse the list and return the proerties
#of the closest object

.Spc.Intersect.SpcCompound <- function(ray.origin, ray.vector, objects) {


  if (length(objects) == 0)
    return(NA)

  int <- NA

  for (i in 1:length(objects)) {
    ii <- .Spc.Intersect(ray.origin, ray.vector, objects[[i]])

    if (!is.na(ii) && (is.na(int) || ii$distance < int$distance))
      int <- ii
  }

  #if (is.na(int))
  if (class(int) == "logical")
    return(NA)

  #Properties can be defined hierarchically for compound objects
  if (is.null(int$properties) || is.na(int$properties)) {
    int$properties <- attr(objects,"properties")
  }

  return(int)
}

#--------------------------

.Spc.NoIntersect.SpcCompound <- function(ray.origin, ray.vector, objects) {

  if (length(objects) == 0)
    return(TRUE)

  int <- NA

  for (i in 1:length(objects)) {
    ii <- .Spc.NoIntersect(ray.origin, ray.vector, objects[[i]])

    if (!ii) return(FALSE)
  }
  return(TRUE)
}


