
## ramascaro - Raùl Mascarò

## Function 1
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function 2
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  invMat <- x$getInverse()
  if(!is.null(invMat)) {
    message("Getting from Cache...")
    return(invMat)
  }
  matrix <- x$get()
  invMat <- solve(matrix)
  x$setInverse(invMat)
  invMat
}

## Sample to Run:
x = matrix(c(1, -2,-3, 4),ncol=2,nrow=2)
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)