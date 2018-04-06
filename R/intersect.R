# This is the internal library of intersect methods for the elementary object types
# In each case there are 2 functions.
# Spc.Intersect - which returns the distance of intersection, and some surface properties
# Spc.NoIntersect - which'll use the same basic calculation, but simple return whether the
# intersect does not happen within distance 1

#==============================================================================

.Spc.Intersect.SpcTriangle <-function(ray.origin,ray.direction,triangle) {

  #Moller-Trumbore algorith, from
  #https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm

  edge1 <- triangle$B - triangle$A
  edge2 <- triangle$C - triangle$A
  h <- Utils.CrossProduct(ray.direction,edge2)
  a <- c(edge1 %*% h)

  if (a == 0)
    return(NA)

  f <- 1/a
  s <- ray.origin - triangle$A
  u <- f * c(s %*% h)

  if (u < 0 || u > 1)
    return(NA)
    
  q <- Utils.CrossProduct(s,edge1)
  v = f * c(ray.direction %*% q)

  if (v < 0 || u + v > 1)
    return(NA)

  t <- f * c(edge2 %*% q)
  if (t > 0) {
    #print(triangle)
    #print(paste(" intersected at ", t))
    return(list(distance=t, normal=Utils.CrossProduct(edge1,edge2), properties=attr(triangle,"properties")))
  } else
    return(NA)
}

#--------------------

.Spc.NoIntersect.SpcTriangle <-function(ray.origin,ray.direction,triangle) {

  edge1 <- triangle$B - triangle$A
  edge2 <- triangle$C - triangle$A
  h <- Utils.CrossProduct(ray.direction,edge2)
  a <- c(edge1 %*% h)

  if (a == 0)
    return(TRUE)

  f <- 1/a
  s <- ray.origin - triangle$A
  u <- f * c(s %*% h)

  if (u < 0 || u > 1)
    return(TRUE)
    
  q <- Utils.CrossProduct(s,edge1)
  v = f * c(ray.direction %*% q)

  if (v < 0 || u + v > 1)
    return(TRUE)

  t <- f * c(edge2 %*% q)
  return (t < 0 || t >= 1)
}

#==============================================================================

.Spc.Intersect.SpcSphere <-function(ray.origin,ray.direction,sphere) {


  #from http://cosinekitty.com/raytrace/chapter06_sphere.html
  a <- sum(ray.direction^2)
  b <- 2 * c((ray.origin - sphere$centre) %*% ray.direction)
  c <- sum((ray.origin - sphere$centre)^2) - sphere$radius^2

  d <- b^2 - 4 * a * c

  if (d > 0) {
    t1 <- (-b - sqrt(d)) / (2 * a)
    t2 <- (-b + sqrt(d)) / (2 * a)
    if (t1 < t2 && t1 > 0) {
      t <- t1
    } else {
      t <- t2
    }
  } else {
    return(NA)
  }

  if (t <= 0)
    return(NA)


  #if $objects is defined then the sphere is merely a bounding object
  #First up, though, if it's not a bounding sphere, it's an actual object...
  if (length(sphere$objects) == 0) {
  
    normal <- (ray.origin + ray.direction * t) - sphere$centre

    r <- list(distance=t,
                normal=normal,
                properties=attr(sphere,"properties"))

    # -- calculate x and y if the plane has direction vectors
    if (!is.na(sphere$direction.pole[1])) {
  
      #degrees north is easy enough
      north <- 90 - (acos(c(Utils.UnitVector(sphere$direction.pole) %*% Utils.UnitVector(normal))) * 180 / pi)

      #degrees east is a tad more complicated
      cp.meridian <- Utils.UnitVector(Utils.CrossProduct(sphere$direction.pole,sphere$direction.meridian))
      cp.normal <- Utils.UnitVector(Utils.CrossProduct(sphere$direction.pole,normal))

      east <- acos(c(cp.meridian %*% cp.normal)) *180 / pi

      #This'll either point to north or south pole. If south, then adjust the eastings accordingly
      cp.axis <- Utils.UnitVector(Utils.CrossProduct(cp.meridian,cp.normal))
      if (east > 0 && east < 180 && Utils.VectorLength(cp.axis + Utils.UnitVector(sphere$direction.pole)) < 0.1)
        east <- 360 - east


      #append these two to the returned object
      r <- append(r, list(north=north, east=east))
    }

    return(r)
  }

  #It's a bounding object, so pass $objects as a list and return that
    return(.Spc.Intersect(ray.origin,ray.direction,sphere$objects))
  
}

#--------------------

.Spc.NoIntersect.SpcSphere <-function(ray.origin,ray.direction,sphere) {

  a <- sum(ray.direction^2)
  b <- 2 * c((ray.origin - sphere$centre) %*% ray.direction)
  c <- sum((ray.origin - sphere$centre)^2) - sphere$radius^2

  d <- b^2 - 4 * a * c

  if (d > 0) {
    t1 <- (-b - sqrt(d)) / (2 * a)
    t2 <- (-b + sqrt(d)) / (2 * a)
    if (t1 < t2 && t1 > 0) {
      t <- t1
    } else {
      t <- t2
    }
  } else {
    return(TRUE)
  }

  if (t <= 0)
    return(TRUE)

  #If it's not a bounding sphere
  if (length(sphere$objects) == 0)
    return (t >= 1)

  return(.Spc.NoIntersect(ray.origin,ray.direction,sphere$objects))
}

#==============================================================================

.Spc.Intersect.SpcPlane <-function(ray.origin,ray.direction,plane) {

  #From formula on https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
  t <- sum((plane$point - ray.origin) * plane$normal) / sum( ray.direction * plane$normal)

  if (t >= 0)
    r <- list(distance=t, normal=plane$normal, properties=attr(plane,"properties"))
  else
   return (NA)

  # -- calculate x and y if the plane has north and east
  if (!is.na(plane$direction.north[1])) {

    int.vector <- (ray.origin + ray.direction * t) - plane$point

    r <- append(r, list(north=c(Utils.UnitVector(plane$direction.north) %*% int.vector)/Utils.VectorLength(plane$direction.north),
                        east=c(Utils.UnitVector(plane$direction.east) %*% int.vector/Utils.VectorLength(plane$direction.east))))
  }

  return(r)
}

#--------------------

.Spc.NoIntersect.SpcPlane <-function(ray.origin,ray.direction,plane) {

  t <- sum((plane$point - ray.origin) * plane$normal) / sum( ray.direction * plane$normal)

  return (t < 0 || t >= 1)
}


