## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create four function to save the cache version for orignal and inverse
## 1. i <- set inverse to null for new matrix
## 2. set <- set the matrix to Cache
## 3. get <- get the orignal matrix
## 4. setInverse <- set the insesve to Cache
## 5. getInverse <- get the inserve result
## 6. return the list to return all function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## get the inverse from makeCacheMatrix
## if the inverse is calculated then return the result then end the function
## recalculate the cached data and set it to Cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}