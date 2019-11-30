## These functions calculate and cache the inverse of an input matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix. Caches first inverse calculated.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Tests the functionality of makeCacheMatrix and cacheSolve given an invertable square matrix.

test <- function(){
        mat1 <-  matrix(c(2, -3, 4, -7), nrow=2, ncol=2, byrow = TRUE)
    
        m <- makeCacheMatrix(mat1)
        print(m$get())
    
        print("Inverse")
        inv1 <- cacheSolve(m)
        print(inv1)
    
        print("Cached Inverse")
        inv2 <- cacheSolve(m) # raises cached warning
        print(inv2)
    
        m2 <- makeCacheMatrix(inv2)
    
        print("Inverse Inverse")
        inv3 <- cacheSolve(m2)
        print(inv3) #note this is the original matrix
}
