## Coursera - Programming in R - Programming Assignment 2

## The following two functions are used to calculate and cache the inverse of a matrix.
## This way, the inverse of matrix must only be calculated once if the matrix itself does not change.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix (setMatrix)
## get the value of the matrix (getMatrix)
## set the value of the inverse matrix (setInverseMatrix)
## get the value of the inverse matrix (getInverseMatrix)

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverseMatrix <- function(y) inverseMatrix <<- y
  
  getInverseMatrix <- function() inverseMatrix
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## cacheSolve computes the inverse of the special "matrix" x returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the function  
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        
  inverseMatrix <- x$getInverseMatrix()
  
  if(!is.null(inverseMatrix)) {
      message("getting cached data")
      return(inverseMatrix)
  }
  
  matrix <- x$getMatrix()
  inverseMatrix <- solve(matrix)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
