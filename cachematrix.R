##====================================================================
## The following two functions work in tandem to invert a matrix and 
## cache it for future use 
##====================================================================
## makeCacheMatrix: This function creates a special "matrix" object 
##  that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatInv <- function(mat) m <<- mat
    getMatInv <- function() m
    list(set = set,             #--set the value of the matrix
         get = get,             #--get the value of the matrix
         setMatInv = setMatInv, #--set the value of the matrix inverse
         getMatInv = getMatInv) #--get the value of the matrix inverse
}
##--------------------------------------------------------------------
## cacheSolve: This function computes the inverse of the special 
##   "matrix" returned by makeCacheMatrix above. If the inverse has 
##    already been calculated (and the matrix has not changed), then 
##    the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getMatInv()
    if(!is.null(m)) {
        message("getting cached matrix inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) # The One time Inv calc to cache
    x$setMatInv(m)
    m
}