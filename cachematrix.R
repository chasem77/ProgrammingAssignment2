## Put comments here that give an overall description of what your
## functions do

## This function creates multiple function calls assigned to a variable that canset and get a matrix as well a setting and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invx <<- solve
  getinv <- function() invx
  list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## This function will check to see if the invx of a matrix has been previously defined. If the value has been previously defined it will be returned, otherwise the inverse will be calculated

cacheSolve <- function(x, ...) {
invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  matrix <- x$get()
  invx <- solve(matrix)
  x$setinv(invx)
  invx
        ## Return a matrix that is the inverse of 'x'
}
