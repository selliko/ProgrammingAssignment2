## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a special matrix which has set, get, setinverse, getinverse functions.
## 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}
 
 
## Write a short comment describing this function
## first, get the cached inverse. If it is not null, return it.
## if NULL, then calculate the inverse and cache it and return the same.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test case
## mat <- matrix(1:4,nrow=2,ncol=2, byrow=TRUE)
## l <- makeCacheMatrix(mat)
## cacheSolve(l)