## This function creates a special "matrix", that can cache its inverse

## This is accomplished by doing the following tasks:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setMatInv <- function(MatInv) M <<- MatInv
        getMatInv <- function() M
        list(set = set, get = get,
             setMatInv = setMatInv,
             getMatInv = getMatInv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        M <- x$getMatInv()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setMatInv(M)
        M
}
