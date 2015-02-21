## These functions compute and cache the inverse of a square matrix 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix 
##  has not changed), then the cachesolve should retrieve the inverse 
##  from the cache

cacheSolve <- function(x) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix
}
