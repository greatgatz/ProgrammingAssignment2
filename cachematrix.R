#The two functions below are related to matrices and are designed to prevent potentially time-consuming computations.
#by checking whether there is a cached value already existing for the cache function to simply look up,
#as opposed to performing the calculation fully.

#The first function, makeCacheMatrix, creates a matrix that returns a list containing functions that
# 1: Set the matrix
# 2: Get the matrix
# 3: Set the inverse and
# 4: Get the inverse
# For this assignment, we will assume that all matrices produced by this function are invertible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) m <<- inverse
  get.inverse <- function() m
  list(set = set,
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

#This second function, cacheSolve, checks whether or not the inverse of the matrix has been cached
#(i.e. previously calculated and stored). If the inverse is cached, the function will simply return
#this valye, otherwise it will calculate the inverse matrix and store it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get.inverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inverse(m)
  m
}
