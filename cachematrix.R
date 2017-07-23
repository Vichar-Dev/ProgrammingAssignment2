## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(matrix)
## This function creates a list containing functions which
## 1. set the value for matrix
## 2. get the value for matrix
## 3. set the value for inverse of the matrix
## 4. set the value for inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
  inverseofmatrix <- NULL
  
  set <- function(y) {
    mat <<- y
    inverseofmatrix <<- NULL
  }
  
  get <- function() {
    mat
  }
  
  setinverseofmatrix <- function(inverse) {
    inverseofmatrix <<- inverse
  }
  
  getinverseofmatrix <- function() {
    inverseofmatrix
  }
  
  list(set = set, get = get,
       setinverseofmatrix = setinverseofmatrix,
       getinverseofmatrix = getinverseofmatrix)
}


## cacheSolve(cacheMatrix, ...)
## Returns the inverse of the passed in cacheMatrix object
## If the inverse has already been calculated and the matrix 
## not changed since the last invocation, returns the cached 
## version. If the inverse has not been calculated,  the 
## function computes the value with a call to solve(...)

cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'cacheMatrix'
  inverse <- cacheMatrix$getinverseofmatrix()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data")
    return(inverse)
  }
  
  data <- cacheMatrix$get()
  inverse <- solve(data, ...)
  cacheMatrix$setinverseofmatrix(inverse)
  inverse 
}
