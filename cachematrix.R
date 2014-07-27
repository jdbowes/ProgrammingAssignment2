## These two functions will calculate the inverse of a supplied matrix and cache the 
## result. If a cache exists the function will use this as opposed to recalculating 
## inverse.

## This function create a special matrix object that will cache its inverse. It creates
## a list of functions:
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set<-function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setmatrix <- function(solve) m<<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}


## Calculate the inverse of a matrix, use a cached matrix
## if one already exists.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmatrix()
    if(!is.null(m)){
            
        message("getting cached data")
        return(m)
    }
    
    matrix <- x$get()
    m <- solve(matrix)
    x$setmatrix(m)
    m
}

