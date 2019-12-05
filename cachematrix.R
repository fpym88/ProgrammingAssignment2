## A set of functions that look at caching and retirinvg inverse matricies
## 

## The first function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv <<- inv
  getinverse <- function() inv
  list(set = set, get= get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function looks for inverse of matrix and reports

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Test with a simple matrix

m1 <- matrix(c(1/2,-1/3,-2/3,3/4), nrow = 2, ncol = 2)

testmatrix <- makeCacheMatrix(m1)

cacheSolve(testmatrix)

## Second test with a simple matrix

m2 <- matrix(c(5/7,-2/3,-3/8,5/9), nrow = 2, ncol = 2)

testmatrix2 <- makeCacheMatrix(m2)

cacheSolve(testmatrix2)
