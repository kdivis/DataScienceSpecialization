##R Programming: Assignment 2
#   -K. Divis (July 2015)
#   -Created as part of Coursera Data Science Specialization


makeVector = function(x = numeric()) {
    m = NULL
    set = function(y) {
        x <<- y
            #My understanding is this: it searches the parent environment to see if x already has a value and then replaces it with y
            #so not just defined within the function and then lost. So when call "set", the value of x is set to the input (y) and the 
            #mean is cleared out to null
        m <<- NULL
    }
    get = function() {x}
            #When call get, just return the function definition: "function() x"
    setmean = function(mean) {m <<- mean}
            #When call setmean, get function with "mean" argument and then rewriting "m" with mean ... the inputted value.
    getmean = function() {m}
            #Just returns function definition: function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
            #Weird format just provides label/factor formatting
}

cachemean = function(x, ...) {
    m = x$getmean()
        #Check the mean stored in the vector from the prior function. If it's null, then just skip to regular calc of mean and store in cache
    if(!is.null(m)) {
        #If isn't null, can pull from the cached data and don't have to recalculate
        message("getting cached data")
        return(m)
    }
    data = x$get()
    m = mean(data, ...)
    x$setmean(m)
    m
}