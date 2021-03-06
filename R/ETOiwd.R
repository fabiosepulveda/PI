ETOiwd <- function(data.eto, farms, rastera, date.start, date.end=date.start, stat.coord){

if (proj4string(farms) != proj4string(rastera)) stop("Your data do not have same coordinate reference system proj4string(farms) != proj4string(rastera)") 

names(farms) <- c("id_farms")
data.eto$date <- as.Date(data.eto$date)
data.search <- subset(data.eto, date >= date.start & date <= date.end)
id.stat <- names(table(data.search$id.stat))
data.stat <- sapply(1 : length(id.stat), function(i) 
  subset(data.search, id.stat == id.stat[i]), simplify = FALSE)
cumsum.data <- sapply(1 : length(data.stat), function(i) sum(data.stat[[i]]$eto))
x <- stat.coord[,1]
y <- stat.coord[,2]
cumsum.eto <- data.frame(names(table(data.search$id.sta)),x,y,cumsum.data)
names(cumsum.eto) <- c("id.stat","x","y","eto")
coordinates(cumsum.eto) <- ~ x + y
box <- bbox(rastera)
box1 <- seq(box[1,1], box[1,2],length=500)
box2 <- seq(box[2,1], box[2,2],length=500)
boxgrid <- expand.grid(box1,box2)
coordinates(boxgrid) <- ~ Var1 + Var2
gridded(boxgrid) <- TRUE
idw.eto <- idw(cumsum.eto$eto ~ 1, cumsum.eto, boxgrid)
rast.idw.eto <- raster(idw.eto)
eto.mean.farms <- extract(x=rast.idw.eto, y=farms, fun=mean, df=TRUE, na.rm=TRUE)
eto.mean.farms$poly_ID <- farms@data$id_farms
names(eto.mean.farms)[2:3] <- c("pred.eto","id_farms")
eto.farms <- merge(farms, eto.mean.farms, by="id_farms")

invisible(return(list(eto.mean.farms=eto.mean.farms,idw.eto=idw.eto,eto.farms=eto.farms)))}
