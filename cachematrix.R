# The makeCacheMatrix function creates a special "vector", which is really a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of inverse of the matrix
# 4.get the value of inverse of the matrix

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


# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed.
# If it is so, it gets the result and skips the computation.
# If not, it then computes the inverse, sets the value in the cache via setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Fetching the cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

##Sample Run
##> x = rbind(c(1, 2), c(2, 1))
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    2    1

##First run
##> cacheSolve(m)
##           [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333

##Second run
##> cacheSolve(m)
##Fetching cached data.
##           [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
