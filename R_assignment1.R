makeMatrix <- function(x=matrix()){
        inverse <- NULL
        set <- function(matrix){
                x <<- matrix
                inverse <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(inverse){
                inverse<<-inverse
        }
        getinverse <- function(){
                inverse
        }
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        
}
cacheSolve <- function(m,...){
        x <- m$getinverse()
        if(!is.null(x)){
                message("getting cached data")
                return(x)
        }
        data <- m$get()
        x <- solve(data)%*% data
        m$setinverse(x)
        x
}
