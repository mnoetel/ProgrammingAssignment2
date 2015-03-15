## Learning R Homework Assignment 2
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
  inv <- NULL
  set <- function(y) {
    X <<- y
    inv <<- NULL
  }
  get <- function() X
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above.
## If the inverse has already been calculated
## (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(X, ...) {
  inv <- X$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- X$get()
  inv <- solve(data, ...)
  X$setinv(inv)
  inv
}
