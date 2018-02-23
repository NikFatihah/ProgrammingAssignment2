makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) inv <<- inverse
  get.inv <- function() inv
  list(set=set, get=get, set.inv=set.inv, get.inv=get.inv)
}
cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  if(!is.null(inv)) {
    message("acquire cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set.inv(inv)
  inv
}
