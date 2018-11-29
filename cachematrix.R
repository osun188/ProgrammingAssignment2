## These two functions are used to create a special matrix object that stores a matrix
## and caches its inverse. This is useful because sometimes calculating the inverse of
## a matrix may require a lot of computations, and caching the result may be more
## beneficial than continually recalculating the inverse for that same matrix.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix created by makeCacheMatrix. If
## the inverse has already been calculated, it retrieves the inverse from the cache.
## Otherwise, it will compute the inverse and set that result in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
