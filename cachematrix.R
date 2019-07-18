## Put comments here that give an overall description of what your
## functions do
#The functions below can be used to create a matrix object that both contains 
# the original matrix as well as the inverse matrix (after it has been 
# calculated once before)



## Write a short comment describing this function
#This function initially sets the inverse_matrix to NULL
#the get function allows to return the matrix that is used as input
#the setinverse function will set the inverse_matrix to 'inverse' if called
#this inverse_matrix is cached
#The getinverse function can be used to retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# the function below first gets the inverse matrix stored in the object matrix 
# object included in the function call
# it checks whether the inverse matrix actually contains any value or is NULL
# if it contains a value it will return the stored value
# if it is still NULL, the function will get the original matrix
# after that it will calculate the inverse matrix with the solve function
# then it will set the inverse matrix value in the matrix object
# ultimately it will return the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    matrix_to_solve <- x$get()
    inverse_matrix <- solve(matrix_to_solve, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}