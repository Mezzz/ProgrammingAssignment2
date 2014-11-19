## This functions allow user to get inverse matrix from cache.

## makeCaheMatrix initialize source matrix and retruns list of sub functions.
## Use always when you need to compute inverse of new matrix.

makeCacheMatrix <- function(x = matrix()) {
        s<- NULL
        set<- function(y){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setSolve <- function(solve) s<<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function returns calculated inverse matrix created with initialize function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)){
                message("getting cached inverse")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setSolve(s)
        s
}
