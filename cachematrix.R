## The goal of this assignnment is to write a pair of functions that that cache the inverse
## of a matrix. Matrix inversion is usually costly, so this is more efficient than computing
## repeatedly

## The function below makeCaheMatrix creates a special object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## The function below cacheSolve computes the inverse of the matrix returned by the function
## above. If inverse is calculated and matrix has not changed then this function should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}
