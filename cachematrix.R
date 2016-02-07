## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invx <- NULL
  if(!(ncol(x) == nrow(x))) {
    print('Matrix must be square')
    break
  }
  set <- function(y) {
    x <<- y
    invx <<- solve(y)
  }
  get <- function() x
  setinv <- function(solve) invx <<- solve
  getinv <- function() invx
  list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function

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
