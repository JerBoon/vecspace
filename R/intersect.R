# This is the internal library of intersect methods for the elementary object types

#==============================================================================

.Spc.Intersect.SpcTriangle <-function(ray.origin,ray.direction,triangle) {

  #Moller-Trumbore algorith, from
  #https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm

  edge1 <- triangle$B - triangle$A
  edge2 <- triangle$C - triangle$A
  h <- Utils.CrossProduct(ray.direction,edge2)
  a <- Utils.DotProduct(edge1,h)

  if (a == 0)
    return(NA)

  f <- 1/a
  s <- ray.origin - triangle$A
  u <- f * Utils.DotProduct(s,h)

  if (u < 0 || u > 1)
    return(NA)
    
  q <- Utils.CrossProduct(s,edge1)
  v = f * Utils.DotProduct(ray.direction,q)

  if (v < 0 || u + v > 1)
    return(NA)

  t <- f * Utils.DotProduct(edge2,q)
  if (t > 0) {
    #print(triangle)
    #print(paste(" intersected at ", t))
    return(list(distance=t, properties=attr(triangle,"properties")))
  } else
    return(NA)
}

#==============================================================================

.Spc.Intersect.SpcSphere <-function(ray.origin,ray.direction,sphere) {


  #from http://cosinekitty.com/raytrace/chapter06_sphere.html
  a <- sum(ray.direction^2)
  b <- 2 * Utils.DotProduct((ray.origin - sphere$centre),ray.direction)
  c <- sum((ray.origin - sphere$centre)^2) - sphere$radius^2

  d <- b^2 - 4 * a * c

  if (d > 0) {
    t1 <- (-b - sqrt(d)) / (2 * a)
    t2 <- (-b + sqrt(d)) / (2 * a)
    if (t1 < t2) {
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
  if (length(sphere$objects) == 0)
    return(list(distance=t, properties=attr(sphere,"properties")))

  #It's a bounding object, so pass $objects as a list and return that
    return(.Spc.Intersect(ray.origin,ray.direction,sphere$objects))
  
}

#==============================================================================

.Spc.Intersect.SpcPlane <-function(ray.origin,ray.direction,plane) {

  #From formula on https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
  t <- sum((plane$point - ray.origin) * plane$normal) / sum( ray.direction * plane$normal)

  if (t >= 0)
    return (list(distance=t, properties=attr(plane,"properties")))
  else
   return (NA)
}
