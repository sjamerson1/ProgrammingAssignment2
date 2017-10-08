## This function creates a "special matrix" object that can cache its inverse
## makeCacheMatrix is a function that creates a list of the following:
## 1. sets the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  
function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
}

##This function computes the inverse of the "special matrix" returned by
##makeCacheMatrix above
##Computing the inverse of a square matrix can be done with the solve 
##function in R. For example, if X is a square invertible matrix, 
##then solve(X) returns its inverse.
##For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    if (!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
    return(inv)
  }
  
