## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix creates a special “matrix”, which is a list containing a function to set and get the value of the matrix
## also to set and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set function will set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get function will get the value of the matrix
  get <- function() x
  
  #setinverse function will set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  
  #getinverse function will get the value of the inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve will  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##if the value has been computed, then it retrieves from the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}



