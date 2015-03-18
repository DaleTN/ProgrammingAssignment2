makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## create the variable "m"
      evn <- environment()
      ## Save the current function's environment value

      if (det(x)!=0) {

            set <- function(y) {
                  ## save the value of Y with name "x" in the parent environment
                  x <<- y
                  ## create "m" with value "NULL" in the parent environment
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

            message("This matrix is not valid for inverse calculation") ## identifies matrix submitted not valid for this function
      }
}


## Write a short comment describing this function

## This function will either calculate the inverse of a matrix or will recover the previously calculated value from Cache if available

cacheSolve <- function(x, ...) {

      ## Return a matrix that is the inverse of 'x'

      m <- x$getinv()
      ## Return a matrix that is the inverse of the original supplied to function "makeCacheMatrix"

      if(!is.null(m)) {                         ## Checking if there is cached data
            message("getting cached data")
            return(m)                           ## Returning the cached matrix
      }
      data <- x$get()
      m <- solve(data, ...)                     ## Calculating the inverse of the matrix
      x$setinv(m)                               ## Setting the value in the cache for future use
      m
}
