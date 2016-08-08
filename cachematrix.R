
## this function saves the inverse of a matrix (cache) and if the inverse of that matrix does't change, then it accesses the cached copy to save the work of computing the inverse of a matrix over and over again. 

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


## this will use the cached matrix assigned y, to skip timely computations of the inverse of a sqaure matrix. 

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

        ## Return a matrix that is the inverse of 'x'
}
