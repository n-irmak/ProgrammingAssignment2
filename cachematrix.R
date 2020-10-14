## makeCacheMatrix function creates a matrix that 
##can cache its inverse and cacheSolve returns 
##the inverse.

## This function creates a matrix that can cache 
##its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) 
    i <<- inverse
  getInverse <- function() i
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function calculates the inverse of matrix 
##created by makeCacheMatrix and if inverse is
##already taken (and the matrix hasn't changed)
##it return the inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <<- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}