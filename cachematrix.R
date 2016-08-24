## Assigns a global value using <<. It sets a matrix and calculates the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL                              
    set <- function(y) {                     
      x <<- y                             
      inv <<- NULL                        
    }
    get <- function() x                     
    
    inverse_set <- function(inverse) inv <<- inverse  
    inverse_get <- function() inv                     
    list(set = set, get = get, inverse_set = inverse_set, inverse_set = inverse_get)   
    
}


## Checks to see if inverse has been calculated.  
## If not, it calculates the inverse and sets the cache value.

cacheSolve <- function(x, ...) {
  
  matrix_inv <- x$getinverse()
  if(!is.null(matrix_inv)) {
    message("Grabbing Cached Data to Save Time!")
    return(matrix_inv)
  }
  matrix_data <- x$get()
  matrix_inv <- solve(matrix_data, ...)
  x$setinverse(matrix_inv)
  matrix_inv
  
        
}
