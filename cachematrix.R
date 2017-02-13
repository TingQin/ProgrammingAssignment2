## The first function sets and get the value of the matrix as well as the value of the inverse of the matrix, the second function calculates the inverse of the special matrix created in the first function if the inverse of the first function has not been calculated. 

## This function creates a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function (solve) m <<- solve
  getsolve <- function () m
  list(set = set, 
       get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## This function computes the inverse of the matrix created by the first function. If the inverse of the first function has been calculated it will skip the computation, otherwise, it will calculate again and sets the value in the cache via setsolve function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}