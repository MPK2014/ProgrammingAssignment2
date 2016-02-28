## This functions cache the inverse of a matrix.


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mtrx_inv <- NULL
  set <- function(y) {
    x <<- y
    mtrx_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mtrx_inv <<- inv
  getinv <- function() mtrx_inv
  list(set = set, get = get, setinv= setinv, getinv = getinv)
}


## Computes the inverse or retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  mtrx_inv <- x$getinv()
  if(!is.null(mtrx_inv)) {
    message("getting cached data")
    return(mtrx_inv)
  }
  data <- x$get()
  mtrx_inv <- solve(data, ...)
  x$setinv(mtrx_inv)
  mtrx_inv
}

