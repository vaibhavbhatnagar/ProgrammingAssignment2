## Put comments here that give an overall description of what your
## functions do
## 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## 
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 
## set the value of the matix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                    # Set the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                             # Get the matrix
    setInverse <- function(inverse) m <<- inverse  # Set the inverse of matrix
    getInverse <- function() m
    list(set = set, get = get,             # Return the list of 4 items
         setInverse = setInverse,
         getInverse = getInverse)
}



## function cacheSolve calculates the inverse of the special "matrix" created 
## with the makeCacheMatrix function. It first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()                 #query the x matrix's cache
    if(!is.null(m)) {                    #Checks whether inverse of matrix is in cache
        message("getting cached data")  # Get the inverse from cache
        return(m)
    }
    data <- x$get()             # Get the matrix
    m <- solve(data, ...)       # Inverse the imput matrix
    x$setInverse(m)             # Set the matrix in cache
    m                           # Output the inverse matrix
}
