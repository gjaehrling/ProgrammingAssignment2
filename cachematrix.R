## the function makeCacheMatrix() returns a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## set the local variable for the inversion of the matrix to NULL
        inv <- NULL
        ## "setter" function for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## "getter" function for the matrix
        get <- function() x
        setmatrix <- function(solve) inv <<- solve
        getmatrix <- function() inv
        ## return a list of the functions created
        ## functions will be used in the cacheSolve() function
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}
## the function cacheSolve() computes the inverse of the matrix. 
## If the inverse was already calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getmatrix()
        ## check if the object m was cached already, if so, return cached data to avoid recalculation
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if inverse matrix is not cached, call the get() function and assign it to variable data
        data <- x$get()
        ## create the inverse of the matrix by using the solve() function and assign to variable inv
        inv <- solve(data, ...)
        ## cache the solved matrix using the setmatrix() function
        x$setmatrix(inv)
        ## return the invers inv
        inv
}
