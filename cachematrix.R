## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a list to
## set the value of the matrx
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m <<- solve
    getmatrix<-function() m
    list(set=set, get=get,
    setmatrix=setmatrix,
    getmatrix=getmatrix)}


## Write a short comment describing this function
## The following function calculates the inverse of the matrix created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
            message("getting cached data for inverse")
            return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
