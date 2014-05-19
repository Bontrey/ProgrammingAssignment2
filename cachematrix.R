## Functions contained within this file implement a matrix that
## caches its inverse after the first time it is solved for.

# Makes a matrix object that can cache its own inverse.
#
# Args:
#   m: The matrix value to construct with.
# Returns:
#   A list with methods to get and set the internal matrix data
#   and the inverse.
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(mat) {
    m <<- mat
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Gets the inverse of a matrix object created by makeCacheMatrix.
# Returns the inverse directly if it exists, otherwise solves for
# it
#
# Args:
#   m: A matrix object created by makeCacheMatrix.
# Returns:
#   The inverse of the matrix.
cacheSolve <- function(m, ...) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setinverse(inv)
  inv
}
