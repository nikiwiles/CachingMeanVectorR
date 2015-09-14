############################################################################
## A pair of functions for dealing with caching the inverse of matrices   ##
##                                                                        ##
## Based on "Example: Caching the Mean of a Vector" by Roger D. Peng, PhD ##                                                                ##
##                                                                        ##
############################################################################

## makeCacheMatrix returns a list containing four functions
##  set    - pushes a matrix to a symbol in the parent environment
##  get    - returns the stored matrix
##  setinv - pushes the inverse of a matrix to a symbol in the parent environment
##  getinv - returns the stored inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Inverse is null in the first instance
  set <- function(y) { # Store a matrix
    x <<- y # Push to parent environment 
    inv <<- NULL # Matrix has changed, so reset the inverse to NULL
  }
  get <- function() x # Return the matrix from the parent environment
  setinv <- function(matrixInverse) inv <<- matrixInverse # Push the inverse to the parent environment
  getinv <- function() inv # Retrieve the inverse from the parent environment
  list(set = set, get = get, # Return all four functions as a list
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve returns the inverse of a matrix created by makeCacheMatrix
## If the inverse has been computed before, return the cached result
## ... otherwise compute the inverse using solve() and cache the result

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # Get the stored value for the inverse
  if(!is.null(inv)) { # Did we get a cached result, e.g. not NULL?
    return(inv) # ... if so, return it
  }
  data <- x$get() # ... if not, fetch the matrix
  inv <- solve(data, ...) # ... invert it
  x$setinv(inv) # ... store
  inv # ... and return the result.
}
