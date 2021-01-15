## The functions here work together to create a matrix and compute the inverse
## of the created matrix if it is not already cached.

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated, cachesolve 
## retrieves the inverse from the cache instead of calculating it again.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  return(i)
}
