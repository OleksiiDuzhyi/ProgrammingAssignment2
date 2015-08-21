## creates a list (kind of an object in OOP) which holds matrix and
## inverse matrix. Whenever matrix is reset - inverse matrix is set 
## to NULL. To access a matrix use $get() list element, to set a 
## matrix usr $set(matrix) list element. To set and get inverted 
## matrix use $setInverted(matrix) and $getInverted() correspondingly
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix) {
        x <<- matrix
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(solve) {
        inverse <<- solve
    }
    getInverse <- function() {
        inverse
    }
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## Calculates an inverse matrix for a given cacheMatrix holder.
## If inverse matrix was calculated before - simply returns 
## cached value, otherwise - calculating the inverse matrix
## and caches (sets) it to a cacheMatrix holder
## NOTE: non-invertable matricies are not handeled at this version
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("using cached value")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
}
