## Function that creates a matrix as an object and also caches its inverse
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list (set=set, get=get,
      setmatrix=setmatrix,
      getmatrix=getmatrix)
}

## Calculates the inverse matrix created with the previous function and
## that uses the cached value in case it is necessary
cacheSolve <- function(x, ...) {
m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
