
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get the value of the inverse of the matrix
  getinverse <- function() inv
  
  # return a list of the calculated values
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # check if the inverse is already computed
  inv <- x$getinverse()
  
  # return inverse if it's already been computed
  if(!is.null(inv)) {
    return(inv)
  }
  
  # compute the inverse of the matrix
  data <- x$get()
  inv <- solve(data)
  
  # set the value in the cache
  x$setinverse(inv)
  
  # return the inverse
  inv
}
