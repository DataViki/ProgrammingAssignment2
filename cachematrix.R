### Assignment: Caching the Inverse of a Matrix


## This function makeCacheMatrix creates a special "matrix" object
##that can set the matrix and get the matrix and set the inverse and get
##the Inverse of it and cache its inverse.

### Assumption:
## 1.Assume that the matrix supplied is always invertible

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

## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then the
##`cachesolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Function cacheSolve for any future run getting data from the cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

### Test Run for Results: 
##> x = rbind(c(1, 4), c(4, 1))
##> p=makeCacheMatrix(x)
##> p$get()
##[,1] [,2]
##[1,]    1    4
##[2,]    4    1
## #########################################################################
##During the first call of function cacheSolve(p) the data is not cached but 
##if we call the same function next time, since its already cached it gets the 
##data from cache which you can seen when you call the function again it take
## a While and the comment can be seen which confirms it gets the data from
## Cache
############################################################################
##> cacheSolve(p)
##[,1]        [,2]
##[1,] -0.06666667  0.26666667
##[2,]  0.26666667 -0.06666667
##> cacheSolve(p)
##Function cacheSolve for any future run getting data from the cache
##[,1]        [,2]
##[1,] -0.06666667  0.26666667
##[2,]  0.26666667 -0.06666667

