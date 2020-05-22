
ETCiwd <- function(data.eto, farms, rastera, date.start, date.end=date.start){

data.eto$date <- as.Date(data.eto$date)
data.search <- subset(data.eto, date >= date.start & date <= date.end)

data <- NULL
for (i in 1 : length(names(table(data.search$id.sta)))){
  data[[i]] = subset(data.search, id.sta == names(table(data.search$id.sta))[i])}

return(data)}




tapply(aa$eto,names(table(aa$id.sta)),subset)


tapply(aa$eto,aa$id.sta,subset)

datos1 = subset(data.eto, id_est == "Car")
datos2 = subset(data.eto, id_est == "Rut")
datos3 = subset(datos, id_est == "Sec")
datos4 = subset(datos, id_est == "Zar")
datos5 = subset(datos, id_est == "Pai")
Estion = c("Car","Rut","Sec","Zar","Pai")






Y <- ETCiwd(data.eto=XX, farms=Farms, rastera = RasterA, date.start = "2019-12-1")