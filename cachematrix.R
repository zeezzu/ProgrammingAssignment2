##  makeCacheMatrix creates a list containig a function to
##  1. set the value of the vector
##  2. get the value of the vector
##  3. set the value of the mean
##  4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve returns the inverse of the matrix from makeCacheMatrix above. 
## by checking if the inverse has already been calculated, 
## if it has, it retrieves the inverse from the cache and skips the computation. 
## Else, it computes the inverse and sets the value in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m      
}