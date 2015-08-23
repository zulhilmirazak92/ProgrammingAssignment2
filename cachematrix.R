## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix object x and some associated sub-functions/methods
        
        ## define the cache z
        z <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the parent environment
                z <<- NULL ## re-initialize z in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) z <<- inverse ## set the cache z equal to the inverse of the matrix x
        getinverse <- function() z ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        z <- x$getinverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}

## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
## > cacheSolve(m)
##  getting cached data
##     [,1] [,2]
##[1,]  0.5  0.0
##[2,]  0.0  0.5
