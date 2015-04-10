##' Basic Mean Test
##' 
##' This function computes the global mean for a set of values and returns
##' either a 0/1 indicating if a value is flagged as "bad" or a score
##' indicating how far that value is from the group mean.
##' 
##' @param y A numeric vector containing the data values.
##' @param returnType One of "score" or "flag".  If "score", a p-value is
##' returned; otherwise a binary value is returned corresponding to whether
##' or not the observation is three standard deviations from the mean.
##' @param robust Logical indicating if a robust estimate of the mean and
##' standard deviation should be used.  Defaults to TRUE, and this is
##' recommended when the data may have outliers.
##' 
##' @return A vector of the same length as the input y.  See returnType.  In
##' the case of the binary return value, a TRUE will represent a flagged value,
##' i.e. a value which is considered bad or a potential outlier.
##' 

basicMeanTest = function(y, returnType = "flag", robust = TRUE){
    
    ## Data Quality Checks
    stopifnot(is(y, "numeric"))
    stopifnot(returnType %in% c("score", "flag"))
    stopifnot(is(robust, "logical"))
    
    if(robust){
        yClean = y[!is.na(y)]
        estimator = robustbase::huberM(yClean)
        mu = estimator$mu
        sigma = estimator$s
    } else {
        mu = mean(y, na.rm = TRUE)
        sigma = sd(y, na.rm = TRUE)
    }
    
    score = (y - mu)/sigma
    if(returnType == "score")
        return(1-abs(pnorm(score)))
    else
        return(as.numeric(abs(score) > 3))
}