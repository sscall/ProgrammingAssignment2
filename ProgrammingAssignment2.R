makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m}

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL          #initialize invrs as NULL; will hold value of inverse matrix  
  set <- function(y) { ## set to assign new  function
    x <<- y        ## value of matrix in parent environment
    invrs <<- NULL   ## if there is a new matrix, reset inv to NULL
  }
  get <- function() {x} ## define the get function - returns value of the matrix argument
  setInverse <- function(inverse) {invrs <<- inverse} ## assigns value of inv in parent environment
  getInverse <- function() {invrs}  ## gets the value of inv where called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if(!is.null(invrs)) {    ## if inverse has not been already cached, it is cached and collected
    message("successfully collected cached data") 
    return(invrs)
  }
  m <- x$get()
  invrs <- solve(m, ...)
  x$setInverse(invrs)
  invrs
}





