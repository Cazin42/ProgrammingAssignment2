##-----------------------------------------------------------------------------------------------------------------------------------------------------
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatly 
## MakeCacheMatrix and cacheSolve are 2 functions used to cache the inverse of a matrix
##-----------------------------------------------------------------------------------------------------------------------------------------------------


##-------------------------------------------------------------------------------
## Use this function "makeCacheMatrix" to create a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
##-------------------------------------------------------------------------------


makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL # stores the result of inversion
    
    set <- function(y) 
    {
        x <<- y
        xinv <<- NULL # initialises xinv to null
    }

    get <- function() x # returns the input matrix
    
    setInv <- function(inv) xinv <<- inv # sets the inversed matrix
    
    getInv <- function() xinv # returns the inversed matrix
 
    list(set = set, get = get,setInv = setInv, getInv = getInv)
                                           }

##-------------------------------------------------------------------------------
## Use this function "cacheSolve" to return the inverse of the matrix
## 1. check if the inverse has already been computed
##   2.  if so, gets the result and skips the computation
##   2b. if not, computes the inverse, sets the value in the cache via setinverse function
## This function assumes that the matrix is always invertible
##-------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  
      mat <- x$getInv() # gets the inversed matrix from object x
           
      if(!is.null(mat)) 
      { 
        message("getting cached data") 
	  return(mat) # if the inversion is not null returns the calculated inversion
      }
      data <- x$get() # if not, we get the matrix object
      mat <- solve(data) # solves it
      x$setInv(mat) # sets it to the object
      mat # returns the solved result
                                }
