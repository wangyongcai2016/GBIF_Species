#批量下载gbif数据
library(rgbif)
setwd('D:/GBIF_Occurences')
grid <- openxlsx::read.xlsx('ChinaModisBox.xlsx')
nrows <- nrow(grid)
for (i in 1:nrows) {
  filename <- paste('h',grid[i,2],'v',grid[i,1],'.xlsx',sep = "")
  geom <- c(grid$lon_min[i],grid$lat_min[i],grid$lon_max[i],grid$lat_max[i])
  data <- occ_search(return = "data",hasCoordinate = T, country = "CN", geometry = geom)
  write.xlsx(data,filename)
  }


