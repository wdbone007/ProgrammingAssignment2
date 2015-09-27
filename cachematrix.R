## Assignment 2
## William Bone
## cachematrix.R
## makeCacheMatrix & cacheSolve
#________________________________________________________________________

## These functions are used to create a matrix object(x), cache its inverse, compute inverse of x,
## and/or retrieve inverse from cache

## The makeCacheMatrix function will set matrix, get matrix, set inverse, and get inverse.

makeCacheMatrix <- function(x = matrix()){ # x is square, invertible matrix; set matrix
  inv = NULL                               # inv = NULL 
  set = function(y){                       # Bring in the y Arg
    x <<- y                                # <<- assigns value to object in different environment
    inv <<- NULL                           # Set inv to NULL in other environment
  }
  get = function() x                       # Get matrix
  setinv = function(inverse) inv <<- inverse # Set inverse
  getinv = function() inv                    # Get inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv) # Returns 4 functions
}

## The cacheSolve function will call inverse from original matrix or inverse in the cache.

cacheSolve <- function(x, ...) {  # x is result of makeCacheMatrix()
  inv = x$getinv()                # call inverse of original matrix input
  if(!is.null(inv)){              # if inverse has been measured
    message("getting cached data") # take it from the cache to save time
    return(inv)
    
  }
  mat.data = x$get()               # otherwise it will calculate the inverse
  inv = solve(mat.data, ...)
  x$setinv(inv)                    # set the value of the inverse in the cache
  return(inv)
        ## Return a matrix that is the inverse of 'x'
}
