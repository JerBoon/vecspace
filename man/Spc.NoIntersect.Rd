% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classdefs.R
\name{Spc.NoIntersect}
\alias{Spc.NoIntersect}
\title{Kind of the opposite of Spc.Intersect(). Takes a ray origin and a direction vector
of a given length, and returns whether any object falls between along the length of
the direction vector. For use by shadowing and illumination calculations}
\usage{
Spc.NoIntersect(ray.origin, ray.direction, object)
}
\arguments{
\item{ray.origin}{A positional vector of where the ray originates}

\item{ray.direction}{A directional vector for the ray's direction}

\item{object}{The object (elementary or compound) we're testing for intersection}
}
\value{
Logical value - TRUE if nothing false along the ray direction vector, FALSE otherwise.
}
\description{
Kind of the opposite of Spc.Intersect(). Takes a ray origin and a direction vector
of a given length, and returns whether any object falls between along the length of
the direction vector. For use by shadowing and illumination calculations
}
\examples{
  if (Spc.NoIntersect (c(0,0,0), c(0,1,0),  my_world)) { do_something }
}
