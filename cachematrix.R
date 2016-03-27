## cachmatrix.R

## This function creates a special "matrix" object that can cache its inverse
## A list to:
## - Set value of matrix
## - Get value of matrix
## - Set inverse of matrix
## - Get inversive of matrix

## Thus, set, get, setmatrix, getmatrix
makeCacheMatrix <- function(x = matrix()) {
  
  # Create "inv", starts as NULL
  inv <- NULL
  
  set <- function(y) {
    # cache values
    x <<- y
    inv <<- NULL
  }
  
  # Retrieve matrix value
  get <- function() x
  # Take matrix inverse, and store solution in cache as "inv"
  setmatrix <- function(solve) inv<<- solve
  # Retrieve cached "inv" value
  getmatrix <- function() inv
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## This function computes inverse of special "matrix object noted above
## If the the inverse is already available in the cache, cachesolve retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrix()
  # If "inv" value is in cache, retrieve
  if (!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
    #returns cached inverse value if it exists
  }
  
  # Else calculate matrix inverse
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setmatrix(inv)
  inv
  #Return inverse matrix "inv"
}
