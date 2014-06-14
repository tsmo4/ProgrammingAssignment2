## This is comprised of 2 functions.
## MakeCacheMatrix coverts an inputted matrix into a "cached matrix", which 
## can "house" the inverse of itself after it's Solved.

## This first function will convert a given matrix, into a "cached matrix".

makeCacheMatrix <- function(x = matrix()) {
        imtx <- NULL
        set <- function(y) {
                x <<- y
                imtx <<- NULL
        }
        get <- function() x
        setimtx <- function(solve) imtx <<- solve
        getimtx <- function() imtx
        list(set = set, get = get,
             setimtx = setimtx,
             getimtx = getimtx)
}


## This function takes a cached matrix as an input, checks if it already 
## "houses" it's inverse.  If it does then it prints it.  If it doesn't, it will
## Solve for it, and then return it to the Cached matrix for future use.

cacheSolve <- function(x, ...) {
        imtx <- x$getimtx()
        if(!is.null(imtx)) {
                message("getting cached data")
                return(imtx)
        }
        data <- x$get()
        imtx <- solve(data, ...)
        x$setimtx(imtx)
        imtx
}