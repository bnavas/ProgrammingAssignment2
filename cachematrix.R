## Put comments here that give an overall description of what your
## The function calculates the inverse of a matrix, by using function solve() with only 1 parameter
## that this will return the desired value. 
## 

## Write a short comment describing this function

#BN
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) #on the default param gives the inverse
  x$setinverse(inv)
  inv
}


## Return a matrix that is the inverse of 'x'

