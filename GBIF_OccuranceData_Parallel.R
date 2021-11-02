#批量下载gbif数据
library(rgbif)
library(openxlsx)
library(purrr)
library(R.utils)
library(doParallel)
library(foreach)

setwd('D:/GBIF_Occurences/1Grid')
grid <- openxlsx::read.xlsx('1GridFishnetExtent.xlsx',1)
nrows <- nrow(grid)
useCores <- detectCores() - 2
cl <- makeCluster(useCores)
registerDoParallel(cl)
##
filelist <- 1:nrows %>% map(function(x) x)

foreach(i = 1:nrows,.packages = 'rgbif',.inorder = T,.combine = rbind) %dopar% {
  filename <- paste(grid$FID[i],'.xlsx',sep = "")
  if (file.exists(filename)) {
    print(sprintf("%s existed",filename))
  }
  else {
    geom <- c(grid$lon_min[i],grid$lat_min[i],grid$lon_max[i],grid$lat_max[i])
    cat(sprintf("Processing row %i/%i",i,nrows))
    tryCatch({
      data <- occ_search(return = "data",hasCoordinate = T, country = "CN", geometry = geom, limit = 100000);
      write.xlsx(data,filename)
    },warning = function(w) {
      #将警告信息打印到控制???
      print(w)
    },error = function(e) {
      #打印错误信息到控制台
      print(e)
    },finally = {
      print(filename)
    })
  }
}
stopCluster(cl)


# lst <- lapply(1:nrows, function(i) {
#   filename <- paste(grid$FID[i],'.xlsx',sep = "")
#   if (file.exists(filename)) {
#     print(sprintf("%s existed",filename))
#   }
#   else {
#     geom <- c(grid$lon_min[i],grid$lat_min[i],grid$lon_max[i],grid$lat_max[i])
#     cat(sprintf("Processing row %i/%i",i,nrows))
#     tryCatch({
#       withTimeout({
#         data <- occ_search(return = "data",hasCoordinate = T, country = "CN", geometry = geom, limit = 100000);
#         write.xlsx(data,filename)
#       },timeout = 3600)
#     },warning = function(w) {
#       #将警告信息打印到控制???
#       print(w)
#     },error = function(e) {
#       #打印错误信息到控制台
#       print(e)
#     },finally = {
#       print(filename)
#     })
#   }})
