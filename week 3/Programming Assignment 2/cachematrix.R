# Function makeCache stores a square matrix and its inverse. 
# Function cacheSolve computes the inverse matrix of a square matrix if it 
# hasn't already been computed.


# This function creates an object whose member variables are a matrix and its
# inverse matrix. It also defines functions to set and get those variables'
# values.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Function to change the matrix's value. When this happens its inverse is
    # set to NULL.
    set <- function(mtx)
    {
        x <<- mtx
        inv <<-NULL
    }
    get <- function() x
    
    # Funtion to set the value of the inverse matrix.
    setinv <- function(inverse)
    {
        ## This test is performed to ensure the matrix passed as argument is
        ## indeed the inverse of matrix x (in case setinv is used in another 
        ## context).
        # if(solve(x)==inverse)
        # {
        # 
        #     inv <<- inverse
        # }
        # else
        # {
        #     message("The matrix submitted is not the inverse of matrix x.")
        # }
        
        inv <<- inverse
        
    }
    
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Function cacheSolve receives as argument an object such as the one created by 
# function makeCacheMatrix, and computes and sets the inverse matrix of that 
# object's matrix. If the object's inverse matrix variable has already been set,
# cacheSolve simply retrieves the cached data.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    
    # Check if the inverse matrix has already been computed and if so retrieve
    # it.
    if(!is.null(inv))
    {
        message("Getting cached data.")
        return(inv)
    }
    
    data <- x$get()
    
    # Compute and set the inverse matrix.
    inv <- solve(data, ...)
    x$setinv(inv)
    
    # Return the inverse matrix.
    inv
}
