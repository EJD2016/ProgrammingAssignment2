## The CacheMatrix.R program will contain two fucntions that will 
## cache the inverse of a matrix.


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inverse_matrix <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse
  setInverse <- function(inverse) inverse_matrix <<- inverse
  ## get the value of the inverse
  getInverse <- function() inverse_matrix
  ## return a cached matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## function should retrieve the inverse from the cache memory.

cacheSolve <- function(x, ...) { 
  inverse_matrix <- x$getInverse()
  if (!is.null(inverse_matrix)) {
    message("Retrieve the cached memory version of the matrix")
    return(inverse_matrix)
  }
  my_matrix <- x$get()
  ## Solve for the inverse of my matrix.
  inverse_matrix <- solve(my_matrix, ...)
  x$setInverse(inverse_matrix)
  ## return the inverse of my matrix.
  inverse_matrix
} 
