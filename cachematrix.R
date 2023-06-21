## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix returns a list of functions that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the inverse of the matrix
## 4. gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## If the matrix already has an inverse, the inverse is returned.
## Otherwise, the inverse is solved for and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
