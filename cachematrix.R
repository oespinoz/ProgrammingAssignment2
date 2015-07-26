## The following two functions will compute and cache the 
## inverse of a given matrix.

## makeCacheMatrix will create matrix and cache functions and values
## for later use.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #nulling m to start
  set <- function(y) {
    x <<- y;
    m <<- NULL
  }
  get <- function() return(x)
  setinv <- function(m) inverse <<- m
  getinv <- function() return(inverse)
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Now cacheSolve will return the inverse of the matrix with data
## stored in the 'special' list/function above.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- x$get()
  invserse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}
