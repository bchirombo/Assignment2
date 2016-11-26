## This filecontains 2 functions that cache the inverse of a matrix
## 

## This function creates a special "matrix"object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {               ## Set the value of the matrix    
                x <<- y
                m <<- NULL                
        }
        get <- function() x                       ##  Get the value o fthe matrix
        setinverse <- function(solve) m <<- solve  ## Set the inverse of the matrix
        getinverse <- function() m                 ## get the inverse of the matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"returned by MakeCacheMatrix above.
## If the inverse has already been calculated ( and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()           ## Checkif the matrix has changed and inverse as been cached
  if(!is.null(m)) {             ## Get inverse of matrix from cache
          message("getting cached data")
          return(m)
  }
  data <- x$get()        
  m <- solve(data,...)             ## Compute the inverse of the matrix
  x$setinverse(m)               ## Set the inverse into cache
  m
}
