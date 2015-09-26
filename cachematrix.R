## This is pair of functions that cache the inverse of a matrix.

## The first function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
s <- NULL
set <- function(y)
    {
      x <<- y
      s <<- NULL
    }
get <- function() x
setinverse <- function(solve) s <<- solve #solve(x) will give an inverse of x where x is a square matrix
getinverse <- function() s
list(set=set,get=get,
     setinverse = setinverse,
     getinverse = getinverse)
}



## The second function 'cacheSolve' computes the inverse of the special matrix returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) 
  {
  s <- x$getinverse()
  if(!is.null(s))
  {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setinverse(s)
        ## Return a matrix that is the inverse of 'x'
  s
  
}
