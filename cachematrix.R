## Since inverting a matrix can use valuble computer space and time,
## an option is to use lexical scoping and a cache. Utilizing the cache
## allows you to retrieve a frequently used function return instead of
## having to execute the entire function again.

## This function creates a matrix which will then be stored in the cache.

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


## First, this function checks to see if the required answer is present
## within the cache. If so, the answer requested will be displayed. If
## not, then the function will execute. The function will return the 
## inverse of the matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting data from cache.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run:
## > x = rbind(c(1, 2), c(3, 4))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## Since there is no answer stored in the cache calling the inverse will perform
## the inverse matrix function.
## > cacheSolve(m)
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## Now try to run the same function. Since this answer is now saved in the cache,
## it will be retrieved instead of needing to run the original function again.
## > cacheSolve(m)
## getting data from cache.
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
