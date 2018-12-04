N = 6*24
v1 = rnorm(N, 0.1)/1000 
v2 = rnorm(N, 0.1)/1000
v3 = rnorm(N, 0.02)
hora = 0:23
min = 0:59
seg = 0:59
#horario = expand.grid(seg, min, hora)
horario = expand.grid(min, hora)
lat = c(-23.56076779)
lon = c(-46.72976164)
val1 = c(50)
for (i in 2:N) {
  lat[i] = lat[i-1]+v1[i]
  lon[i] = lon[i-1]+v2[i]
  val1[i] = val1[i-1]+v3[i]
}
inter = (max(val1) - min(val1))/5
m = min(val1)
quali = factor(cut(val1, c(m, m+inter, m+inter*2, m+inter*3, m+inter*4, m+inter*5), 
          c("Péssimo", "Ruim", "Regular","Bom", "Ótimo"), include.lowest = TRUE), ordered = T)
dat = data.frame(hora = horario$Var2,
                 min = horario$Var1,
                 seg = rep(0, N),#$horario$Var1,
                 lat = lat,
                 lon = lon,
                 val1 = round(val1,2),
                 quali = quali)
plot(dat$val1, type = "l")
plot(dat$lat~dat$lon, type = "l")

dat$quali
library(mapview)
library(ggmap)
library(ggplot2)


library(readr)
library(sp)

df <- read_csv("https://raw.githubusercontent.com/fastah/sample-data/master/FastahDatasetMapsTutorial.csv")

ggmap(b) + geom_point(data = df, 
                      aes(lon,lat,color=Operator),size=2,alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="Ping Locations", color = "Operator")


coordinates(dat) <- ~ lon + lat
proj4string(dat) <- "+init=epsg:4326"

mapview(dat)
mapview(dat, zcol = "quali", burst = TRUE, legend=T)

leaflet(data = sep[]) %>% 
  setView(lng = mean(sep$Longitude), lat = mean(sep$Latitude), zoom = 12) %>% addTiles()  %>%
  addCircleMarkers(~Longitude, ~Latitude,  color = ~Color_Assets(Percent_SEP12_Assets),  popup = ~as.character(paste(Site, Percent_SEP12_Assets , sep=", "))) %>%
  addLegend("bottomright", pal = Color_Assets, values = Percent_SEP12_Assets, title = "SEP 12 Installs")

Percent_SEP12_Assets <- factor((sep[,8] > 33) + (sep[, 8] >= 66), labels = c("Less than 33%", "Between 33% and 66%", "More than 66%"))

Color_Assets <- colorFactor(c("darkred","darkorange","darkgreen"),
                            levels = Percent_SEP12_Assets,ordered=FALSE)
