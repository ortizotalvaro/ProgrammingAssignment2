# to use these functions:
# source('cachematrix.R)

# -----------------------
# makeCacheMatrix 
# This function creates a special "matrix" object that can cache its inverse.

# CALLING
# a matrix object should be created first:
# amatrix <- makeCacheMatrix()
# then any function of makeCacheMatrix can be called like:
# amatrix$initmatrix(matrix(1:4,2,2))
# amatrix$getmatrix()
# ----------------------------
makeCacheMatrix <- function(mymatrix=matrix()){

    list(    
        
        #Initialize my matrix with some specific values given in mym
        #mym should be of class(mym)=matrix
        initmatrix = function(mym){
            mymatrix <<- mym
            myinverse <<- NULL
        }
        ,
    
        #get mymatrix from chache
        getmatrix = function() { mymatrix }
        ,
    
        #add the inverse of mymatrix to the list (that is output of makeCacheMatrix)
        saveinverse = function(myinv) {
            myinverse <<- myinv
        }
        ,
    
        #get myinverse from cache
        getinverse = function() { myinverse }
    )        
    
}


# ----------------------------
# cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache.

# CALLING
# First initialize a matrix with the function above:
# somematrix<-makeCacheMatrix() and somematrix$initmatrix()
# Then use cacheSolve:
# cacheSolve(somematrix)
# ----------------------------
cacheSolve <- function(mym){
    
    #check if there's a value of the inverse of mym already in cache
    myinv <- mym$getinverse
    
    if(!is.null(myinverse)){
        cat('getting inverse matrix from cache')
        mymatrix <- mym$getmatrix()
        mymatrix
        myinverse
        return(myinverse)
    }
    
    #if inverse is not yet calculated, get the matrix
    mymatrix <- mym$getmatrix()
    print(dim(mymatrix))
    cat('calculating inverse matrix of', mymatrix)
    
    #calculate inverse of mymatrix
    myinverse <- solve(mymatrix)
    
    #save myinverse in cache
    mym$saveinverse(myinverse)
    
    myinverse
    #print(myinverse)
        
}
