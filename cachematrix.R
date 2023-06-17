## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
      x <<- y
      invs <<- NULL
    }
    get <- function() x
    setinvs <- function(inverse) invs <<- inverse
    getinvs <- function() invs
    list(set = set, get = get,
         setinvs = setinvs,
         getinvs = getinvs)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getinvs()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinvs(invs)
  invs
}
