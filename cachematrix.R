## makeCacheMatrix() creates a matrix object that can cache its inverse using four functions: 
##1.  set the value of matrix 'x' (set)
##2.  get the value of the x (get)
##3.  set the value of the inverse of x with the solve() function (set_inverse)
##4.  get the value of the inverse of x (get_inverse)

## the output of makeCacheMatrix is a list of the four above functions so they are available for use in the R 
## parent environment (thus, subsequently available for use by the cacheSolve function). 

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y ##utilize double operator to assign same x from parent environment
                inverse_matrix <<- NULL 
        }
        get <- function() x
        set_inverse <- function() inverse_matrix <<- solve(x)
        get_inverse <- function() inverse_matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## the cacheSolve() function returns a matrix that is the inverse of 'x' by using the functions defined in the
## makeCacheMatrix() function. If there is data cached in MakeCacheMatrix, this function pulls it instead of 
## recaculating the inverse of x, thus saving time and computing power. 

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$get_inverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        my_matrix <- x$get()
        inverse_matrix <- solve(my_matrix, ...)
        x$set_inverse(inverse_matrix)
        inverse_matrix
}