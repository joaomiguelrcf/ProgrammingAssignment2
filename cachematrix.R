## FUNCTION 1

## DESCRIPTION:

# The makeCacheMatrix function, when called, returns a list of 4 functions: set, get, setInverse and createInverse.
# The function also sets 2 variables in those functions' environment - x and inverse:
#   "x" is the matrix whose inverse you wish to calculate 
#   "inverse" is its inverse matrix
# By default, both "x" and "inverse" are empty matrices

# The functions in the list set and get the values for "x" and "inverse":
#   get() and set() return and set, respectively, the value of "x"
#   getInverse() and setInverse() return and set, respectively, the value of "inverse"

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse){
        inverse <<- newInverse
    } 
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## FUNCTION 2

## DESCRIPTION:

# The following function calculates the inverse of the matrix "x" passed to the above function
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
# of the matrix and sets the value of the inverse in the cache via the setInverse() function.


cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    return(inverse)
}


##TESTING

#Matrices for testing:

A = matrix(c( 7, 2, 1,
              0, 3,-1,
             -3, 4,-2),3,3,byrow = TRUE) #3x3 matrix

B = diag(1,4,4) #identity matrix 4x4

C = matrix(c(2, 3, 1, 5 ,
             1, 0, 3, 1 ,
             0, 2, -3, 2 ,
             0, 2, 3, 1),4,4, byrow = TRUE) #4x4 matrix

#inverse matrices for checking
solve(A)
solve(B)
solve(C)

#testing the functions
x = makeCacheMatrix(C)
cacheSolve(x)

