## Author: Hanseas
## Based on the example "Caching the Mean of a Vector"

## This function creates a special "matrix" object
## that can cache its inverse.
## makeCacheMatrix creates a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of inverse of the matrix
## 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the matrix. Checks if
## the inverse has already been computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Trying:
# > x = rbind(c(4, 1/5), c(1/5, 4))
# > h = makeCacheMatrix(x)
# > h$get()
# [,1] [,2]
# [1,]  4.0  0.2
# [2,]  0.2  4.0
## Not inverse calculated yet:
# > h$getinverse()
# NULL
## No cache, yet:
# > cacheSolve(h)
# [,1]        [,2]
# [1,]  0.25062657 -0.01253133
# [2,] -0.01253133  0.25062657
## Cache already exist:
# > cacheSolve(h)
# getting cached data
# [,1]        [,2]
# [1,]  0.25062657 -0.01253133
# [2,] -0.01253133  0.25062657
# > h$getinverse()
# [,1]        [,2]
# [1,]  0.25062657 -0.01253133
# [2,] -0.01253133  0.25062657
## Thank you!

