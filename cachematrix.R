# has two functions makeCacheMatrix and cacheSolve
#cache solve does the actual calculation and caches the data through
# getters and setters provided by makeCacheMatrix


## makeCacheMatrix returns a list of functions that helps
## to set the value of the matrix,get the value of the matrix,
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  #setter
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #getter
  get <- function() x
  #setter for inverse
  setinverse <- function(inverse) inverse <<- inverse
  #getter for inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve sets the inverse of the matrix and returns the inverse.
## if the inverse has already being calculated it returns the cahced data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
