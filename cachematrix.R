## There are 2 functions in this code.
## makeCacheMatrix and cacheSolve

## The makeCacheMatrix function creates a special matrix
## that is a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse of the matrix
##  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cachesolve function calculates the inverse of the special matrix
## created using the above function. However, it first checks
## If the inverse has already been calculated. 
##  If so, it gets the inverse from the cache and skips the computation 
##  Otherwise, it calculates the inverse of the matrix and
##  sets the value of the inverse in the cache using setsolve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s # inverse of matrix
}
