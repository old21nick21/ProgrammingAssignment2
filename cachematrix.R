## Coursera Programming Assignment #2
## functions to inverse the content of the matrix
## using the cached inversed matrix, if available

## Main function - calculate the inversion of the matrix
## with get and set functionality
makeCacheMatrix <- function(x = matrix()) {
  ## initiates m value
  m <- NULL 
  ## set the value of the matrix, assining them from the other environment
  ## evoking caheSolve function in process
  set <- function(y) { 
    ## looks for cacheSolve
    x <<- y 
    m <<- NULL 
  }
  ## returns x
  get <- function() x
  ## sets the inverted matrix using the other environment
  setinvertmatrix <- function(solve) m <<- solve
  ## returns the inverted matrix, if any
  getinvertmatrix <- function() m
  # holder for set and get functions
  list(set = set, get = get, 
       setinvertmatrix = setinvertmatrix,
       getinvertmatrix = getinvertmatrix)  
}


## Calculate the inversion of the matrix x, 
## checking if this is already being done
## and returning the cached values, if available
cacheSolve <- function(x, ...) {
  
  ## need to check if the same original matrix is being used
  cur<<-x
  if(!x==cur){
    ## checks if inversion is already calculated
    m <- x$getinvertmatrix()
    if(!is.null(m)) {
      ## uses cashed inversion and giving user a warning about it
      message("using cached inversion")
      return(m)  ## exits the function
    }
  }
  ## finds out the value of x
  matr <- x$get()
  ## calculates the matrix inversion
  m <- solve(matr, ...)
  ## sets the inverted matrix value in the other environment 
  x$setinvertmatrix(m)
  ## returns m (inverted matrix)
  m
}