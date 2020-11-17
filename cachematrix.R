## Put comments here that give an overall description of what your
## functions do

## To make the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  f <- NULL                  ## initializing inverse as null
  set <- function(y) {
      x <<- y
      f <<- NULL
    }
  get <- function() x                   ## function to get matrix x
  setInverse <- function(inverse)f <<- inverse
  getInverse <- function() f            ## function to obtain inverse of the matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## To get the cached data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  f <- x$getInverse()
  if(!is.null(f)){                        ## checking whether inverse is null
    message("getting cached data")
    return(f)                             ## returns inverse value
  }
  mat <- x$get()
  f <- solve(mat,...)                     ## calculates inverse value
  x$setInverse(f)
  f                                      ## returns a matrix that is the inverse of x
}
