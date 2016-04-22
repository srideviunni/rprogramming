# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m<<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}




Sample run:
## > x = rbind(c(1, 2), c(2, 1))
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    2    1
##> cacheSolve(m)
##          [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> cacheSolve(m)
##getting cached data.
##           [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
