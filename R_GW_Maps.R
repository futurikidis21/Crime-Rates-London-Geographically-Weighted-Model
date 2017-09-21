install.packages("rgdal")
# For spatial data handling
library(rgdal)
install.packages("spdep")
library(spdep)
install.packages("rgeos")
library(rgeos)
# For charting
install.packages("tmap")
library(tmap)
install.packages("cartogram")
library(cartogram)
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("GGally")
library(GGally)
# For data munging
install.packages("dplyr")
library(dplyr)
# For spatial stats
install.packages("GWmodel")
library(GWmodel)
install.packages("spdep")
library(spdep)
# For regression functions
install.packages("car")
library(car)

percentage<- read.csv("db(N).csv")
percentage$id <- as.character(percentage$id)
gb_boundaries <- readOGR(dsn = "London-wards", layer = "Wardsv2")
gb_boundaries@data$WD11_CMWD1 <- as.character(gb_boundaries@data$WD11_CMWD1)
gb_boundaries@data <- inner_join(gb_boundaries@data, percentage,  by=c("WD11_CMWD1" =  "id"))
gb_boundaries@data[is.na(gb_boundaries@data)]<-0
#Geographically Weighted model  and mapping...change range to get maps
gw_ss <- gwss(gb_boundaries,var=c(colnames(gb_boundaries@data[45]),colnames(gb_boundaries@data[16:36])),kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE)
tm_shape(gw_ss$SDF,unit.size=10000000000000) +
  tm_fill(col=colnames(gw_ss$SDF@data[,c(573,574,585,586,587,588,589,590,591,593)]), title="Spearman Correlation Coefficient", style="cont",palette="Spectral", size=2,scale=2) + 
  tm_borders(col="#bdbdbd", lwd=0.01) +
  tm_facets(free.scales = FALSE,scale.factor = 10) +
  tm_layout(
    panel.labels=c('Crimes vs Population','Crimes vs Population Density','Crime vs Bad Health','Crime vs Higher Social Status','Crime vs Unemployed','Crime vs Economically Active','Crime vs Unemployed','Crime vs Long Term Unemployed','Crime vs Unqualified','Crime vs House Price'),
    panel.label.size=0.5,
    panel.label.height =1.5,
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=0.7,
    legend.text.size=0.6,
    legend.outside=TRUE)
tmap_mode("plot")
