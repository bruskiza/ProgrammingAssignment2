## This function creates a special "matrix" object that can cache its inverse
## This function's structure is largely based on the "Example: Caching the Mean of a Vector"
## provided in the R Programming coursera course.

# this function will make a cache matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function will give an inverse of a matrix.
# If it has been done before, it will provide the cached version for efficiency.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("data cached. using this instead.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
