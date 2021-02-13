## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


## Readme
#Use the following to test the above functions
#> A <- matrix( c(5, 1, 0,
#                3,-1, 2,
#                4, 0,-1), nrow=3, byrow=TRUE)  #replace with any other matrix
#> det(A)  #make sure not zero
#> solve(A)
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500


#> x <- makeCacheMatrix(A)
#> cacheSolve(x)
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500

#> cacheSolve(x)
# getting cached data
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500



