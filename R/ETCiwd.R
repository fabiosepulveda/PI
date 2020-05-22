ETCiwd <- function(data.eto, farms, rastera, date.start, date.end=date.start, stat.coord){

data.eto$date <- as.Date(data.eto$date)
data.search <- subset(data.eto, date >= date.start & date <= date.end)
data.stat <- sapply(1 : length(names(table(data.search$id.sta))), function(i) subset(data.search, id.stat == names(table(data.search$id.stat))[i]), simplify = FALSE)
cumsum.data <- sapply(1 : length(data.stat), function(i) sum(data.stat[[i]]$eto))
x <- stat.coord[,1]
y <- stat.coord[,2]
cumsum.eto <- data.frame(names(table(data.search$id.sta)),x,y,cumsum.data)
names(cumsum.eto) <- c("id.stat","x","y","eto")
coordinates(cumsum.eto) = ~ x + y

return(cumsum.eto)}
