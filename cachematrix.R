## the following pair of functions caches the inverse of a matrix, to avoid computation of inverse matrix repeatedly
## makeCacheMatrix function creates a special vector which is esentially a list containing a funtion to a) set a matrix, b) get a matrix, c) set the inverse matrix, d) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv_matrix <- NULL
  set <- function(y) {
       x <<- y
       inv_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_matrix <<- inverse
  getinv <- function() inv_matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function 'cacheSolve' computes the inverse of the matrix returned the function 'makeCacheMatrix.' 

cacheSolve <- function(x, ...) {
  
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  data_matrix <- x$get()
  inv_matrix <- solve(data_matrix, ...)
  x$setinv(inv_matrix)
  inv_matrix
}