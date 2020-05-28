ETCiwd <- function(data.crop, data.eto, farma, rastera, cons.date, stat.coord){

data.crop$date.crop <- as.Date(data.crop$date.crop)
data.eto$date <- as.Date(data.eto$date)
farms.crop.cons <- subset(data.crop, data.crop$date.crop <= cons.date)
farms.name <- names(table(farms.crop.cons$farms.crop))
farms.crop.date <- sapply(1:length(farms.name), function(i) subset(farms.crop.cons, farms.crop.cons$farms.crop == farms.name[i]), simplify = FALSE)

aa <- sapply(1:length(farms.crop.date), function(i) names(table(farms.crop.date[[i]]$name.crop)), simplify = FALSE)

ETO <- ETOiwd(data.eto=data.eto, farms=farms, rastera=rastera, date.start=cons.date, stat.coord=coord.stat)
return(ETO)}