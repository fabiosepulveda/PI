ETCiwd <- function(data.crop, data.eto, farma, rastera, cons.date, stat.coord){

data.crop$date.crop <- as.Date(data.crop$date.crop)
data.eto$date <- as.Date(data.eto$date)
cons.date <- as.Date(cons.date)
data.crop$age.crop <- (cons.date - data.crop$date.crop)+1
farms.crop.cons <- subset(data.crop, data.crop$date.crop <= cons.date)
farms.name <- names(table(farms.crop.cons$farms.crop))
farms.crop.date <- sapply(1:length(farms.name), function(i) subset(farms.crop.cons, 
                   farms.crop.cons$farms.crop == farms.name[i]), simplify = FALSE)
farms.date.name.crop <- sapply(1:length(farms.name), function(i) sapply(1:length(names(table(farms.crop.date[[i]]$name.crop))), 
       function(j) subset(farms.crop.date[[i]],farms.crop.date[[i]]$name.crop==names(table(farms.crop.date[[i]]$name.crop)[j])),
       simplify = FALSE))
farms.date.crop <- sapply(1:length(farms.name), function(i) sapply(1:length(names(table(farms.crop.date[[i]]$name.crop))), 
    function(j) farms.date.name.crop[[i]][[j]][which(farms.date.name.crop[[i]][[j]]$date.crop==max(farms.date.name.crop[[i]][[j]]$date.crop)),],
    simplify = FALSE))
ETO <- ETOiwd(data.eto=data.eto, farms=farms, rastera=rastera, date.start=cons.date, stat.coord=coord.stat)
return(ETO)}