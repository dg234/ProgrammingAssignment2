## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## This function assumes that x is always invertible
        m <- NULL
        #function to set/modify the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        #function to get the matrix x
        get <- function() x
        #function to set the inverted matrix
        setInverse <- function(solve) m <<- solve
        #function to get the inverted matrix
        getInverse <- function() m
        #list the available functions
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        #check for a cached version of the inverse matrix of x
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                ## Return a matrix that is the inverse of 'x'
                return(m)
        }
        #if no cached version exists get the 
        matrix <- x$get()
        #compute inverse of matrix using solve()
        m <- solve(matrix, ...)
        #cache the inverted matrix
        x$setInverse(m)
        #display inverted result
        m
}