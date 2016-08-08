
## creates a cached inverse of a square matrix to be retrieved later if the value of the inverse does not change

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() x
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## accesses the cached inverse of the square matrix created from makeCachMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix.data = x$get()
  inv = solve(matrix.data, ...)
  
  x$setinv(inv)
  return(inv)

}
