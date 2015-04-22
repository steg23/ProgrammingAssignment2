## The function needs as input a matrix variable, calculate the inverse and store the results in the cache
## After is possible to recall these variables directly from the cache

## makeCacheMatrix: stores functions in a list and cache the calculated variables

makeCacheMatrix <- function(x = matrix()) {

        
        m <- NULL
        
        # changes the matrix stored in the main function 
        set <- function(y) {                    
                x <<- y
                m <<- NULL                      # restore to NULL the value of the inverse calculation
        }
        
        # returns the matrix x stored in the main function
        get <- function() x                      
        
        # store the value of the input in a variable m into the main function
        setsolve <- function(solve) m <<- solve 
        
        # return the value of the variable m 
        getsolve <- function() m                 
        
        
        # store the functions into the main function
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
        
}


## cacheSolve: get the data from cache (if exist) or from variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        m <- x$getsolve()               # get data from makeCacheMatrix function getsolve
        
        # check the value of m, if exist and is not NULL, return the variable m from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if m doesn't exist or is NULL, 
        # calculate the inverse of the matrix and store the results into cache
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
        
        
}
