## Computing the calculations to perform matrix inversion can be a highly time consuming operatuion
## Caching the result of the inverse matrix to reuse it instead of reapiting the calculation when 
## the matrix values are not changing can save a lot of time.
## These functions allow caching the result of the matrix inversion operation to reuse them while
## values of the matrix are not changed.

##The next function creates a special "matrix", which is really a list containing a function to
## 1.-set the value of the matrix
## 2.-get the value of the matrix
## 3.-set the value of the inverse
## 4.-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

      inverseMatrix <- NULL
      
      ##if the matrix content changes the inverseMatrix is reset to perform a new calculation      
      set <- function(y) {
            x <<- y
            inverseMatrix <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverseMatrix <<- inv
      getinverse <- function() inverseMatrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)      
     
      
      
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inverseMatrix <- x$getinverse()
      ##if inversMatrix is not null we pick the cached value
      if(!is.null(inverseMatrix)) {
            message("getting cached data")
            return(inverseMatrix)
      }
      ## if not, data may be changed so we update data values and perform calculation again
      data <- x$get()
      inverseMatrix <- solve(data)
      x$setinverse(inverseMatrix)
      inverseMatrix      
      

}
