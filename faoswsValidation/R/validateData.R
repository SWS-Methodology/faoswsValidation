##' Validate Data
##' 
##' @param data A data.table object containing the data to be validated.
##' @param valueColumnName The column name of data which contains the value
##' to be validated.
##' @param byKey A vector of column names which should be used to specify
##' different slices of the data.  Each unique byKey will be validated
##' separately.  Moreover, for models with independent variables (such as
##' basicLmTest) time is assumed to go from 1 to the number of observations,
##' and this won't be valid if an incorrect byKey is provided.
##' @param validationModels A list of functions which should be used to
##' validate the data.  The functions should only require one argument: y.
##' 
##' @return A numerical vector of the same length as nrow(data).  The number
##' represents the number of models which identified that particular row of
##' data as a potential outlier.
##' 

validateData = function(data, valueColumnName, byKey, validationModels){

    ## Data Quality Checks
    stopifnot(valueColumnName %in% colnames(data))
    stopifnot(byKey %in% colnames(data))
    
    data[, flaggedCounts := 0]
    sapply(validationModels, function(fun){
        data[, flaggedCounts := flaggedCounts + fun(get(valueColumnName)),
              by = byKey]
    })
}