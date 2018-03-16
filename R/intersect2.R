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
    ii <- .RTintersect(ray.origin, ray.vector, objects[[i]])

    if (!is.na(ii) && (is.na(int) || ii$distance < int$distance))
      int <- ii
  }
  #if (is.na(int))
  if (class(int) == "logical")
    return(NA)

  if (is.null(int$surface) || is.na(int$surface)) {
    int$surface <- attr(objects,"surface")
  }

  return(int)
}

