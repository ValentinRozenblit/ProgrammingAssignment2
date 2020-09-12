############## PROGRAMMING ASSIGNMENT 2: LEXICAL SCOPING ##############

# This code will receive a matrix and return its inverse if not previously calculated.
# If previously calculated, it shall retrieve the inverse from the cache data.

# This function receives a matrix and stores it into the cache. It also sets
# the inverse if it was previously calculated

makeCacheMatrix <- function(x = matrix()) {  # function that receives a square matrix
    m <- NULL                                
    set <- function(y) {                      
        x <<- y                              
        m <<- NULL                           
    }
    get <- function() x                      # the object "get" will be the matrix
    setSolve <- function(solve) m <<- solve  # puts the inverse into de cache
    getSolve <- function() m                 # gets the inverse from the cache
    list(set = set, get = get,               
         setSolve = setSolve,                
         getSolve = getSolve)                
}


# This function receives the "special" matrix created by makeCacheMatrix
# and will return the inverse if it was prevously calculated. But if it's not
# then it will caclute the inverse and set it to the cache once again.

cacheSolve <- function(x, ...) {             # function that solves the inverse
    m <- x$getSolve()                        # gets the solved matrix
    if(!is.null(m)) {                        # if m is not NULL 
        message("getting cached data")       # prints that line
        return(m)                            # returns the inverse matrix
    }
    data <- x$get()                          # data will be the matrix
    m <- solve(data, ...)                    # m is now the inverse matrix
    x$setSolve(m)                            # sets it to the cache
    m                                        # returns the inverse matrix
}
