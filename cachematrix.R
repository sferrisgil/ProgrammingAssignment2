## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 

  m <- NULL
  set <- function(y) {  ## set the matrix
    x <<- y   ## We use the '<<' operator to asign values to variables out of the current environment
    m <<- NULL
  }
  get <- function()     ## get the matrix
  setinverse <- function(solve) m <<- solve ## set the inverse of the matrix
  getinverse <- function() m   ## get the inverse of the matrix
  ## We set a list with names that match the names of functions we want to use
  ## associated with the inverse of a matrix
  list(set = set, get = get,    
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if 
## the inverse of the matrix has already been calculated. If so,
## it gets the inverse of the matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  ## here we check if the inverse it's already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
 ## This part is executed just if we have never calculated the inverse of the matrix before!
  data <- x$get() ## We get the matrix
  m <- solve(data, ...) ## the actual calculation of the inverse it's done here
  x$setinverse(m) ## We put the inverse in the cache for future use!
  m               ## the return of the function is the inverse of the matrix just calculated
}
