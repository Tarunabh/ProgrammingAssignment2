#The R-code here is used to compute inverse of a matrix. 
#The programs are efficient on saving computational time as they cache the inverse of a matrix.
#if the input matrix doesn't change, it is not required to again perform entire the computation 
#The cached value can be looked up to find the inverse value
#scoping rule of R-programming language has also been used here to preserve the value held by an object.


##The first function - makeCacheMatrix() - is performing functions 
##to set the value of the matrix, get the value of the matrix,
##set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


##This calculates the inverse of the matrix which has been taken. 
##Computaion is skipped in case inverse is already there in the cache
##Otherwise inverse is calculated and set in the cache by setinverse function

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
