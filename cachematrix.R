# functions that cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse
  setinv = function(inverse) inv <<- inverse
  # get the inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}

# cacheSolve: This function computes the inverse of the makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
