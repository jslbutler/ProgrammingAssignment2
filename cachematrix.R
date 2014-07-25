## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # defines a NULL variable
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  ## Cache of inverse solution
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  ## Checks to see if inverse exists
  if(!is.null(m)) {
    message("getting cached data")
    # if the inverse exists it is outputted
    return(m)
  }
  
  data <- x$get() #Retrieves data
  m <- solve(data, ...)
  ## Computes Inverse
  x$setinverse(m) # cahces inverse
  m # outputs result
}
