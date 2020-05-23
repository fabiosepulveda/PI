plot.pi <- function(X,...){
if (class(X)[1]=="SpatialPixelsDataFrame"){
spplot(X,"var1.pred",...)
}else{
spplot(X, c("pred.eto"),...)
}
}
getS3method("plot", "pi", optional = FALSE)