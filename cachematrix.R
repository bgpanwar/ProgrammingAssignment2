## This function creates a "special matrix" that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  ## m is the cached value and starts as NULL.
  m <- NULL
  
  ## x holds the matrix value; the set function sets its value to y and resets 
  ## the cached value m.
  
    set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  ## get function returns x which is the stored matrix value.
  get <- function() x 
  
  ## setinverse function just sets the cached value to the input parameter inverse.
  setinverse <- function(inverse) m <<- inverse
    
  ## getinverse function returns the cached inverse value, which can be NULL.
  getinverse <- function() m 
  
  
  ## this list groups all the related functions together as the "special" matrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function takes a "special" matrix and computes the matrix inverse, if not in Cache.
## It returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  ## first this function checks to see if the inverse has been already created
  m <- x$getinverse()
  
  ## If inverse already exists, then gets the inverse from the cache and skips computation
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise it calculates the inverse of the data and sets the value of the inverse
  ## in the cache via the set inverse function.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## return the calculated inverse
  m
}

