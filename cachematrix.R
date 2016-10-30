## This filecontains 2 functions that cache the inverse of a matrix
## 

## This function creates a special "matrix"object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
}


## This function computes the inverse of the special "matrix"returned by MakeCacheMatrix above.
## If the inverse has already been calculated ( and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- solve(x)
  if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(x,...)
  m
}
