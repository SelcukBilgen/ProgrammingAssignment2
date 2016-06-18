## 1. Function(makeCacheMatrix) simply caches the matrix
## that was calculated previously.

## 2. Function(CacheSolve) calculates the reverse of a square matrix
## after it checks the matrix is cached or not in order to avoid calculating
## the reverse of the same matrix again.

## This function creates a list that holds the matrix and its necessary
## methods.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setReverse <- function(solve) m <<- solve
  getReverse <- function() m
  list(set = set, get = get,
       setReverse = setReverse,
       getReverse = getReverse)
}


## This function checks if the matrix is cached or not. If so, it simply
## return the reverse of the matrix using the getReverse method of the matrix.
## If the matrix is not cached, then it calculates the reverse of the matrix
## using solve function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getReverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setReverse(m)
  m
  
}
