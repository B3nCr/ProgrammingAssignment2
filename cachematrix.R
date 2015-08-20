# makeCacheMatrix returns an object which will cache a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    my_inverse <- NULL

    # function to return the cached matrix
    getMatrix <- function() { x }

    # updates matrix to new value and clears cache
    setMatrix <- function(m) {
        print('clearing cache')

        ## double arrow <<- sets the variable on the parent scope
        ## that means this set x, the "plain" matrix, to the new value m
        x          <<- m
        # and clears my_inverse, the cached inverse
        my_inverse <<- NULL
    }

    # returns the cached inverse
    getInverse <- function(){ my_inverse }

    ## caches the inverse supplied by i in the parent scope
    setInverse <- function(i) { my_inverse <<- i }

    # return list of functions
    makeCacheMatrix <- list(
        getMatrix = getMatrix,
        setMatrix = setMatrix,
        getInverse = getInverse,
        setInverse = setInverse)
}


## takes a cache object (makeCacheMatrix) and gets the inverse from the cache if it's available
## or calculates the inverse and adds it back to the cache object for next time around
cacheSolve <- function(x, ...) {
    ## get the cached value
    inverse <- x$getInverse()

    # check if it's not null
    if(!is.null(inverse)){
        print('using cached inverse')

        # return the cached inverse
        cacheSolve <- inverse

        # stop processsing
        return
    }

    # cache is null so get the matrix and store in m
    m <- x$getMatrix()

    # invert the matrix and store the result in i
    i <- solve(m)

    # push the inverse back into the cache
    x$setInverse(i)

    # return the inverse
    cacheSolve <- i
}
