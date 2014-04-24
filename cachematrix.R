## We set two functions one to set the values and the second to calculate and 
## store the value of the inverse for a matriz after a check for the stored value
## of the inverse of the matrix.

## A function to set and get the values of a matrix to be inverted
## keeping the values in memory as cached objects.

makeCacheMatrix <- function(x = matrix()) {
  ##set the value of the inverse to null
  invMt <- NULL
  ##a set function
  set <- function(y) {
    x <<- y
    invMt <<- NULL
  }
  ##a get function
  get <- function() x
  ##a set function for the inverse
  setinvrs <- function(inv) invMt <<- inv
  ##a get function for the inverse
  getinvrs <- function() invMt
  ##a list with all the values
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## A function that check if the inverse of the matrix is cached
## if the value is in memory the value is returned.
## If the value is not in memory then the function calcuate the value
## and store it in memory.Then the value is returned.

cacheSolve <- function(x, ...) {
  ## get the posssible value of the inverse
  inv <- x$getinv()
  ## check if the inverse value is cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Get the data from the makeCacheMatrix
  data <- x$get()
  ## Calculating the inverse of the matrix 'x'
  inv <- solve(data, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
