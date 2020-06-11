## Caching the Inverse of a Matrix


## Creates a matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_x <<- inverse
  getInverse <- function() inv_x
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes inverse of matrix created using makeCreateMatrix function defined above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInverse()
  if (!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  m <- x$get()
  inv_x <- solve(m, ...)
  x$setInverse(inv_x)
  inv_x
}
