## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(y = matrix()) {
  
  ##initialize inverse
  inv <- NULL
  
  ##sets matrix
  set <- function (matrix) {
          y <<- matrix
          inv <<- NULL
  }
  
  ##get matrix and return
  get <- function () {
    y
  }
  
  ##set inverse of matrix
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ##get inverse of matrix and return
  getinverse <- function() {
    inv
  }
  
  ##list methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  y <- x$getinverse()
  
  if(!is.null(y)) {
        message("getting cached data")
        return(y)
  }
  
  data <- x$get()
  
  y <- solve(data) %*% data
  
  x$setinverse(y)
  
  y

}
