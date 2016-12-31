## Put comments here that give an overall description of what your
## functions do

## Two methods, makeCacheMatrix in which the matrix and its inverse can be cache,
## and cacheSolve where the inverse matrix can be calculated if it wasn't before.

## Write a short comment describing this function
# Name: makeCacheMatrix
# Argument: x as an Invertible matrix 
# Returns a list with 4 methods
#       setMatrix, set the invertible matrix
#       getMatrix, get the invertible matrix
#       setInverse, set the inverse of the matrix
#       getInverse, get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse_aux) x_inverse <<- inverse_aux
  getInverse <- function() x_inverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# Name: cacheSolve
# Argument: x as a List of Functions returned by makeCacheMatrix (the invertible matrix within)
# Returns the Inverse of the cointened matrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$getMatrix()
  inv <- solve(matr)
  x$setInverse(inv)
  inv
  
}
