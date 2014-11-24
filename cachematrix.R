## These pair of functions store a special "matrix" object that contains both:
## (1) the matrix itself
## (2) cached (after the first computation) inverse of the original matrix
## So if a matrix does not change, we compute (and store) the inverse only once,
## and then use this (stored) inverse matrix as much as we need.

## The function makeCacheMatrix input is some invertable matrix.
## The function creates a NULL inverse funcion, and methods set and get for both:
## the matrix and it's inverse.
## The function returns list of these 4 functions (set,get,setinv,getinv)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv_mat) inv <<- inv_mat
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve input is a list we get from the makeCacheMatrix
## that is asociated with a special "matrix" object that contains matrix and
## it's inverse.
## The function tries to retrieve a cached inverse matrix if such exists.
## If exists it prints a message "getting cached data" and return this inverse,
## otherwise it calculates and stores(cashes) the inverse matrix and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
