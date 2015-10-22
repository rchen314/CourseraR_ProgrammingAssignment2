## Put comments here that give an overall description of what your
## functions do

## Create a matrix that can cache its inverse.  Stores cached value in 
## global environment variable i.

makeCacheMatrix <- function(x = matrix()) {

   i <- NULL
   
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   
   get <- function() x
   
   setinverse <- function(solve) i <<- solve
   getinverse <- function() i
   
   list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## First checks if there is a cached value of the inverse.  If there is,
## it uses that.  Otherwise, it calls solve() to find the value of inverse
## and stores that in the cache.

cacheSolve <- function(x, ...) {

   i <- x$getinverse()
   
   if (!is.null(i)) {
      message("Getting cached data")
      return (i)
   }   
   data <- x$get()
   i <- solve(data, ...)    
   x$setinverse(i)
   i
}


