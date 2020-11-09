## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  set_inv <- function(invcal) {inv <<- invcal}
  get_inv <- function() {inv}
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("getting catched data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

