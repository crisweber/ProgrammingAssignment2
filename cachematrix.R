## As computing the inverse of a square matrix can be time-consuming,
## it's good to have a way to save the result, so that when this value is 
## needed again you can lookup for the saved value instead of recomputing it.
## 
## Here we provide two distinct functions that together provides a way to
## hold a matrix, compute it's inverse and cache the result with the
## matrix.


## Creates a list for a matrix with an internal cache aimed to store the 
## inverse of that matrix. This list provides functions to set and retrieve 
## the matrix (set and get, respectively), and to set and retrieve the 
## inverse (setinverse and getinverse, respectively).
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        ## Ensures that the cache will be cleared whenever a new matrix is
        ## defined.
        set <- function(mat) {
                x <<- mat
                inv <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) inv <<- inverse

        getinverse <- function() inv

        ## This way, x and inv are not publicly available, so one can only
        ## manipulate them using the provided functions.
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## A wrapper function for R's solve that computes the inverse of a squared matrix
## and stores the result for future calls.
## This function only works with matrices created by makeCacheMatrix function, 
## and assumes that the matrix is squared and invertible.
## 
## See help(solve) for more details about the solve function.
## 
cacheSolve <- function(x, ...) {

        ## Before computing the inverse of the square matrix, first check if 
        ## it was already computed and cached, returning the cached value if so.
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        ## Compute the inverse of the square matrix held by parameter x and 
        ## cache the inverse for future calls
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}
