# PWD plot mean relationships
library(terra)
cwd <- rast('data/pwd/cwd1981_2010_ave_PWDX.asc')
plot(cwd)

pInfo <- read.csv('data/pwd/plotInfo.csv')
head(pInfo)

xx <- as.points(pInfo[,c('UTM.E','UTM.N')])
