## This first function creates a special "matrix" object that can cache its inverse.
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


## Then, the following function computes the inverse of the special "matrix" returned 
## ... by makeCacheMatrix above. If the inverse has already been calculated 
## ... (and the matrix has not changed), then the cachesolve should retrieve the 
## ... inverse from the cache.

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
}

