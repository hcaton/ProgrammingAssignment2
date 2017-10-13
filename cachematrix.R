## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 'get' solves the matrix for the inverse, 'set' is not called by 'cacheSolve'
## 'setmatrix' caches its argument, 'getmatrix' returns cached matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() solve(x)
        setmatrix <- function(matrix) {
		m <<- matrix
	  }
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function
## 'cacheSolve' calls functions in 'makeCacheMatrix'
## 'getmatrix' returns cached matrix, 'm'
## if the matrix returned by 'getmatrix' is not null, it is returned and function stops
## if 'm' IS null, 'get' is called
## then 'setmatrix' is called on matrix returned by 'get'
 
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- matrix(data, nrow(data), ncol(data))
        x$setmatrix(m)
        m

}
