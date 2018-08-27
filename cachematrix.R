## Put comments here that give an overall description of what your
## functions do
## The following functions are used to create a special object that stores a matrix and caches its inverse. 

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("GETTING CACHED DATA")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
