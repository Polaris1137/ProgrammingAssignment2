## Let x equals as matrix
## After which, represent "w" as null
## Replace mean with solve
makeCacheMatrix <- function(x = matrix(sample(1:800,4),9,9)) {
   w <- NULL
   set <- function(y) {
        x <<- y
        w <<- NULL
        }
   get <- function() x
   setsolve <- function(solve) w <<- solve 
   getsolve <- function() w
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}    

## Likewise, replace "mean" to "solve" then "m" to "w'

cacheSolve <- function(x, ...) {
   w <- x$getsolve()
   if(!is.null(w)) {
           message("getting inversed matrix")
           return(w)
           }
   data <- x$get()
   w <- solve(data, ...)
   x$setsolve(w)
   w
}
