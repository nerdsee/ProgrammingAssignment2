## The two functions in this source implement a cached version
## of a matrix inversion

## makeCacheMatrix generates a special matrix, which is a list with
## a getter and a setter for the value of the matrix
## a getter and a setter for the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(my) {
    x <<- my
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## the function calculates the inverse of the matrix.
## the result is cached and a repeated call with the same
## matrix will return the inverse from the cache
## the matrix needs to be constructed, using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
