## This function, in general, is able to handling a square matrix and calculates its inverse.
## The differential of this function for an ordinary function of this type is that
## the inverse method is optimized using cache and environments.

## This function have three methods: set,get, SetInverse and getInverse. Also,
## there is a local variable called 'inverse'. Using this function it is possible
## create a matrix, get the value of this matrix, set an inverse and get this value.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<-NULL
        }
        get <- function() x
        setInverse <- function(inverse_input){
                inverse <<- inverse_input
        }
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates a inverse of an matrix of any square dimension.
## if the inverse was already called, the function use the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse_value <- solve(t(data.matrix(data)))
        x$setInverse(inverse_value)
        inverse_value
}
