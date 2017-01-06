## Making the inversion of a matrix may be usually costly computation 
## and it could have some benefit to catch the inverse of a matrix rather
## than compute it repeatedly. This two functions are used to cache the
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
				inv<-NULL
				set<-function(y){
					x<<-y
					inv<<-NULL
				}
				get <- function() x
				setinv<-function(inverse) inv <<- inverse
				getinv <- function() inv
				list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
				inv <- x$getinv()
				if(!is.null(inv)) {
					message("getting cached data.")
					return(inv)
				}
				data <- x$get()
				inv <- solve(data, ...)
				x$setinv(inv)
				inv
}
## Example run:
## > x<-diag(5,6)
## > j<-makeCacheMatrix(x)
## > j$get()
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]    5    0    0    0    0    0
## [2,]    0    5    0    0    0    0
## [3,]    0    0    5    0    0    0
## [4,]    0    0    0    5    0    0
## [5,]    0    0    0    0    5    0
## [6,]    0    0    0    0    0    5
##
## > cacheSolve(j)
##      [,1] [,2] [,3] [,4] [,5] [,6]
## [1,]  0.2  0.0  0.0  0.0  0.0  0.0
## [2,]  0.0  0.2  0.0  0.0  0.0  0.0
## [3,]  0.0  0.0  0.2  0.0  0.0  0.0
## [4,]  0.0  0.0  0.0  0.2  0.0  0.0
## [5,]  0.0  0.0  0.0  0.0  0.2  0.0
## [6,]  0.0  0.0  0.0  0.0  0.0  0.2
