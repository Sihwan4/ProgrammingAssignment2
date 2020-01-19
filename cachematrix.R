## Below are two functions that are used to create a special object
# that stores a matrix and cache's its inverse.
# The matrix is assumed to be always invertible.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    S <- NULL
    set <- function(y) {
      x <<- y
      S <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) S <<- solve
    getSolve <- function() S
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
    
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve retrieve the inverse from the cache.
## The following function calculates the inverse of the set of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets
# the value of the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  S <- x$getSolve()
  if(!is.null(S)){
    message("getting cached data")
    return(S)
  }
  data <- x$get()
  S <- solve(data,...)
  x$setSolve(S)
  S
}
