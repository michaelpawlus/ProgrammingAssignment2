## The following functions create a list of functions for setting
## a variable equal to a matrix and for retrieving that variable.

## The functions also compute the inverse of a matrix and assigns
## that to a variable as well as retriving this inverted matrix

## The following function first checks to see if an inverse
## matrix has already been assigned if it has not then it
## returns this cached object, otherwise it inverts the
## matrix set in the previous function and provides the inverse

## The first function creates a list of functions for assigning
## and retrieving a matrix and/or an inverted matrix

makeCacheMatrix <- function(x = matrix()) {
          
          ## the default value is NULL when the variable
          ## holding this list of four function is defined
          m <- NULL
          
          ## The set function takes the value passed in the function
          ## and assigns this to x, as well as return m to NULL
          set <- function(y) {
                  x <<- y
                  m <<- NULL
          }
              
          ## The get function returns the matrix defined in the
          ## set function
          get <- function() x
              
          ## The setinv function sets the variable m to the 
          ## inverse of the matrix passed in the function
          setinv <- function(invm) m <<- solve(invm)
              
          ## The getinv function returns the value of m defined
          ## in the setinv function
          getinv <- function() m
        
          ## The list function creates a list of the 
          ## four functions above
          list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



## This function first looks to see if an inverted matrix has
## already been computed above, if not it does the calculation

cacheSolve <- function(x, ...) {
          ## m is defined by calling the getinv function above
          ## if m has already been defined through the setinv
          ## function then this value is returned with the
          ## message that the retrieved data is from the cache
          m <- x$getinv()
          if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
          }
      
          ## If m is NULL (the default value), then the result of
          ## calling the get function is passed to the data variable
          ## The m variable then takes the inverted matrix and this
          ## is returned
          ## In addition the the reverted matrix is passed back up
          ## through the setinv function so that can be re-inverted
          ## when getinv is called
          ## In this case, if cacheSolve is called again then the
          ## results will be pulled from the cache
          
          data <- x$get()
          m <- solve(data, ...)
          x$setinv(solve(m))
          m
}
