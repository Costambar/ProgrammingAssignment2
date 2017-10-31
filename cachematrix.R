# functions that cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y # assigns the input argument to the x object
    inv <<- NULL # clears any value of inv cached by prior executions of cachesolve()
  }
  # get the matrix
  get <- function() x # x is retrieved from parent environment makeCacheMatrix()
  # set the inverse
  setinv = function(inverse) inv <<- inverse
  # get the inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv) # puts each function in a list()

}

# cacheSolve: This function computes the inverse of the makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # gets the inverse from x and stores it in inv
  if(!is.null(inv)) { # checks if not NULL and returns cache
    message("getting cached data")
    return(inv)
  }
  # if NULL gets new inverse value and sets in parent environment
  data <- x$get() 
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
