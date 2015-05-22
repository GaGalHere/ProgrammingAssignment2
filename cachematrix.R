## makeCacheMatrix creates a vector that contains four functions
##  1. set function, does two things, (a) it caches y to x and (b) it
##     initializes invm, which will hold the inverse of matrix
##  2. get function, returns the output of matrix x
##  3. setsolve function, caches the solve function to invm
##  4. getsolve function, returns the inverse matrix, invm
## The output of the makeCacheMatrix function is a list of four functions
## The four functions are made available to a subsequent calling function
## Each of the four functions are given names so that they can be called
## individually in a subsequent function.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() {
    x 
  }
  setsolve <- function(solve) {
    invm <<- solve 
  }
  getsolve <- function() {
    invm  
  }
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is a function which returns the inverse of a matrix
## In this function we first determine the inverse of a matrix, invm
## Next, the variable containing the inverse, invm, is checked if it is 
## not blank, or null.  If it is not null then then the messege "getting
## cached data" will appear and then return the inverse of the matrix
## Otherwise we set the variable 'data' to the matrix we want to invert,
## then we use the 'solve' function to determine the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getsolve()
  if(!is.null(invm)) {
    message("getting cashed data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setsolve(invm)
  invm
}