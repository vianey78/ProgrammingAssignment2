## --------------------------------------------------------------------------
##  This function creates a special "matrix" object 
##   that can cache its inverse. 
##  It uses solve() to calculate the inverse
##   of a given matrix. 
##  As stated in the Assignment explanation, computing 
##   the inverse of a square matrix can be done with the solve 
##   function in R. For example, if X is a square invertible matrix, 
##   then solve(X) returns its inverse.
##
## Usage Example for these Functions (must use a square matrix): 
##    > my_matrix <- matrix(c(2, 4, 1, 3), nrow=2, ncol=2)
##    > my_cached_matrix <- makeCacheMatrix(my_matrix)
##    > cacheSolve(my_cached_matrix)
##    .... outputs the inverted matrix ...
##    > cacheSolve(my_cached_matrix)
##    .... outputs from cache ...
## --------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() { x }
  
  # Set the value of the inverse of the matrix
  setsolve <- function(solve) { m <<- solve }
  
  # Get the value of the inverse of the matrix
  getsolve <- function() m
  
  # Return the result list
  list(
        set = set, 
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
      )
}

## --------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
##  EXAMPLE: cacheSolve(x, ...) - where 'x' is a
##         'makeCacheMatrix' object
##
## --------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getsolve() # retrieve from cache
  
  if(!is.null(m)) { # we got something...
    message("getting cached data")
    return(m)
  }
  
  # In case the cache isn't filled yet
  data <- x$get()       # get it
  m <- solve(data, ...) # solve it
  x$setsolve(m)	      # set it
  m		      # return it
}