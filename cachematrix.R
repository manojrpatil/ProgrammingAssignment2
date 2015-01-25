# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## The makeCacheMatrix function creates a  matrix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Set matrix function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get matrix function
  get <- function() x
  ## Setting inverse matrix cache function
  setinverse <- function(inverse) i <<- inverse
  ## Get cached matrix function
  getinverse <- function() i
  ## Returns cached matrix data type
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the  matrix which created with the makeCacheMatrix function.
## However, it first checks if the inverse has already been calculated.
##  If so, it gets the inverse from the cache and skips the computation.
##  Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## check if matrix is altrady been cached
  if(!is.null(inv)) {
    message("getting cached data ...")
    return(inv)
  }
  ## get matrix
  data <- x$get()
  # solve the matrix
  inv <- solve(data, ...)
  ## Set cache
  x$setinverse(inv)
  ## return inverse matrix
  inv
        ## Return a matrix that is the inverse of 'x'
}
