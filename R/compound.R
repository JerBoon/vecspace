
#---------------------------------------------------------------------
# compound onjects are basically just a list of other objects 
# The compound is not a tangible object itself, so each of these functions
# need to iterate through the members in the list, etc ,etc..
#---------------------------------------------------------------------


#---------------------------------------------------------------------
#' Return an object from a list of other objects
#'
#' @param objects A list of sub-objects
#' @param properties Package-independent object defining additional properties.
#'     Default NA
#' @param bound Should a bounding sphere be added to the new object? Default = TRUE
#'
#' @return Returns a compound object consisting of the objects passed to it.
#'   The function constructs an object and assigns it the necessary properties
#'   for the package to recognise it as such. 
#' 
#' @export
#'
#' @family constructors
#'
#' @examples
#'   world <- Spc.CombineObjects(list(my_table, my_chairs), "dining area", TRUE)

Spc.CombineObjects <- function (objects, properties=NA, bound=TRUE) {

  if (class(objects) != "list" || length(objects) < 2) {
    print ("Spc.CombineObjects : objects should be a simple list of 2 or more spatial objects")
    return(NA)
  }
  for (i in 1:length(objects)) {
    if (!sum(class(objects[[i]]) %in% c("SpcTriangle","SpcPlane","SpcSphere","SpcCompound"))) {
      print ("Spc.CombineObjects : objects should be a list of recognised spatial objects")
      return(NA)
    }
    
    if (sum(class(objects[[i]]) == "SpcPlane") && bound)
    {
      print ("Spc.CombineObjects : can't add bounding sphere to objects including a plane")
      return(NA)
    }
  }

  # -- We're good to go --

  r <- objects

  class(r) <- append(class(r),"SpcCompound")

  if (!is.na(properties))
    attr(r,"properties") <- properties

  if (!bound)
    return (r)

  # -- add a bounding sphere --

  bndRec <- .Spc.BoundRec.SpcCompound (r)
  spc <- bndRec[[2]] + (bndRec[[1]] - bndRec[[2]]) /2 
  spr <- Utils.VectorLength(bndRec[[1]] - bndRec[[2]]) / 2
 
  s <- Spc.MakeSphere(spc,spr)
  s$objects <- r

  return(s)
}

#------------------------------------------------------------------------

.Spc.Translate.SpcCompound <- function(move.direction, objects) {

  for (i in 1:length(objects)) {
    objects[[i]] <- .Spc.Translate(move.direction, objects[[i]])
  }
  return (objects)
}

#------------------------------------------------------------------------

.Spc.Rotate.SpcCompound <- function(pivot.point,pivot.rotMatrix, objects) {

  for (i in 1:length(objects)) {
    objects[[i]] <- .Spc.Rotate(pivot.point,pivot.rotMatrix, objects[[i]])
  }
  return (objects)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcCompound <- function(objects) {

  for (i in 1:length(objects)) {

    r <- .Spc.BoundRec(objects[[i]])

    if (is.na(r)[1])
      return (NA)

    if (i == 1) {
      r1 <- r[[1]]
      r2 <- r[[2]]
    } else {
      r1 <- pmax(r[[1]],r1)
      r2 <- pmin(r[[2]],r2)
    }
  }

  return(list(r1,r2))
}


.Spc.Polylines.SpcCompound <- function(objects) {

  r <- list()

  for (i in 1:length(objects)) {
    r <- append(r, .Spc.Polylines(objects[[i]]))
  }

  return(r)
}
