#week 3 assignment: Lexical Scoping

##create matrix so solving for inverse gets results from 
#cached value instead of solving again 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(mx) {
                x <<- mx 
                inv <<- NULL 
        }
        get <- function() x 
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}



## solve inverse of matrix
#if already solved, return cached results from above  

cacheSolve <- function(cachemat, ...) { 
        inv <- cachemat$getinv()
        if(!is.null(inv)){
                message("getting cached value!")
                return(inv)
        }
        matrx <- cachemat$get()
        inv <- solve(matrx, ...)
        cachemat$setinv(inv)
        
        inv
        
}




