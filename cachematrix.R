## This function writes a pair of functions that cache the 
## inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)  ##set the value of the matrix
  {
    x <<- y
    inv <<- NULL 
  }
  get <- function() 
  {
    x #get the value of the matrix
  }
  setinv <- function(inverse) inv <<- inverse #set the value of the inverse
  getinv <- function() inv #get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  #checks to see if mean has been cleared
  if(!is.null(inv)) 
  {  
    message("getting cached data")
    return(inv)
  } 
  else
  { 
    data <- x$get()
    inv <- solve(data) #calculates the inverse of the data
    x$setinv(inv) #sets the value of the mean in the cache
    return (inv)
  }
}
