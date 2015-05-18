## makeCacheMatrix and cacheSolve is a pair of functions that cache the inverse of a matrix
##
## How to use them?
## > source("cachematrix.R")
## > mat <- makeCacheMatrix(matrix(data = c(1,2,3,0,1,4,5,6,0), nrow = 3))
## > cacheSolve(mat)
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## > cacheSolve(mat)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
