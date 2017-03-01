## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
    invmat <- NULL
    set <- function(y) 
    {
      x <<- y
      invmat <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invmat <<- inverse
    getinv <- function() invmat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
##Computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  
  if(!is.null(invmat))
  {
    message("getting cached data")
    return(invmat)
  }
  mat.data <- x$get()
  invmat <- solve(mat.data,...)
  x$setinv(invmat)
  
  invmat
}
