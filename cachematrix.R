##    cachematrix.R  03/18/2015  Dale G. Armor
##    There are 2 functions included in this file:
##
##    1)  makeCacheMatrix:    This function creates a special "matrix" object that can cache its inverse.
##    2)  cacheSolve:         This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##                            If the inverse has already been calculated (and the matrix has not changed), then the
##                            cacheSolve function should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## create the variable "m"
      evn <- environment()
      ## Save the current function's environment value


      ## Although the assignment said to assume all matricies could have an inverse created,
      ## that is not true.  By using the following if statement it checks if the matrix can have
      ## a valid inverse calculated.  This is done to prevent any makeCacheMatrix function failures.

      if (det(x)!=0) {

            set <- function(y) {
                  ## save the value of y with label "x" and initialize "m" in the parent environment
                  x <<- y
                  m <<- NULL
            }
            get <- function() x                       ## function to recover matrix data for recalculation from the cache
            setinv <- function(solve) m <<- solve(x)  ## function to set the value of m in the cache
            getinv <- function() m                    ## function to get the value of m from the cache
            getevn<- function() environment()         ## function returns the function environment at time of execution

            ## output the values to set up the functions within their common cache environment

            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv,
                 getevn = getevn)

      } else {

            ## identifies matrix data submitted not valid for this function
            message("This matrix is not valid for inverse calculation")
      }
}

##    Second Function

## This function will either calculate the inverse of a matrix or will recover the previously calculated value from Cache if available

cacheSolve <- function(x, ...) {

      m <- x$getinv()                           ## Gets the cached inverted matrix if available

      if(!is.null(m)) {                         ## Checking if there is a cached matrix
            message("getting cached data")
            return(m)                           ## outputing the cached matrix
      }
      data <- x$get()                           ## Retrieval of matrix data from cache
      m <- solve(data, ...)                     ## Calculating the inverse of the matrix
      x$setinv(m)                               ## Setting the value the calculated  matrix in the cache for future re-use
      m                                         ## Outputing the calculated matrix
}
