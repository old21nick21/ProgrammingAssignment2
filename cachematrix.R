## Create a vector using cached mean value, if available
makeVector <- function(x = numeric()) {

  ## Init  value for m
  m <- NULL
  ## set values, assigning them from the other environmnet
  ## evoking cachemean function in process
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## returns x
  get <- function() x
  ## sets the value of mean using the other environment
  setmean <- function(mean) m <<- mean
  ## returns value of the mean, if any
  getmean <- function() m
  ## finally generate a vector with all available values
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## Checks if mean is already calculated, 
## and perfroms the calculation, if mean value is empty
cachemean <- function(x, ...) {
  ## checks getmean values and uses them if available
  ## giving user a warning message
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)  ## exits the function
  }
  ## finds out the value of x
  data <- x$get()
  ## calculates the mean
  m <- mean(data, ...)
  ## sets the mean value in the other environment 
  x$setmean(m)
  ## returns m (mean value)
  m
}
