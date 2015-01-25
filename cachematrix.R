## These functions are to initialize the functions and do matrix inversion. The cachesolve
## function will check in the cache and if not available, will perform matrix inversion on 
## the data

## makeCacheMatrix function initialized a variable m, so it is present at the parent 
## environment. The function calls Set, get, Setinverse and getinverse functions. the 
## setInverse function sets the inverse matrix to the variable m. Variable m is present 
## at the parent level.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  
  get <- function() x
  
  setinverse <- function(x){
    print(x)
    m <<- solve(x)
    print(m)
  } 
  
  # getting the inverse matrix
  getinverse <- function() m
  
  # function output
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## CacheSolve function checks to see if the Inverse Matrix is present in cache. If so,
## it return the inverse matrix. If not, the inverse matrix is set and the variable m is 
## returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  print(m)
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  print(data)
  #  m <- solve(data)
  
  
  x$setinverse(data)
  m
  
}
