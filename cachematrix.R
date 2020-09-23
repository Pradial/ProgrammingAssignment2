## Both of this functions calculate the inverse of a matrix, and stores the 
## result in the memory, in order to avoid needing to calculate again 
## the inverse of the same matrix in case its needed again.

## This function takes a matrix and stores it in an object. It also stores four
## functions in the same object: set(), get(), setinv() and getinv(), alongside
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    im<-NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinv <- function(solve) im <<- solve
    getinv <- function() im
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function checks if the inverse of the matrix is stored in the memory (in
## the environment of makeCacheMatrix()). In case its not there, it calculates 
## the inverse of the matrix, and stores the result in the MakeCacheMatrix()
## environment. 

cacheSolve <- function(x, ...) {
    im <- x$getinv()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im
}
## Return a matrix that is the inverse of 'x'

mymatrix<-makeCacheMatrix(matrix(c(1, 4, 5, 5), 2, 2))
cacheSolve(mymatrix)
