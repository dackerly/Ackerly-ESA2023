# This analysis uses David Ackerly's species-level summary statistics
# of CWD and AET niches (e.g., various kinds of niche means, optima, etc.)
# to compute community weighted climatic niche means for tree assemblages
# in California FIA plots used in Rosenblad et al. (2023) PNAS.
# Then we see which niche mean version is best predicted by macroclimate.

options(scipen=9999)

library(foreach)
library(doParallel)
detectCores() # this tells you how many are available in your current environment
cores <- 10 # ADJUST TO THE NUMBER OF PROCESSOR CORES YOU WANT TO USE FOR PARALLELIZATION

# read previously prepped subplot data
subplots <- readRDS("./data/kr_fia_data_products/current_subplots.RDS")

# zoom out to plot level
plots <- unique(subplots[c("LAT_LON", "LAT", "LON", "STATECD")])
rm(subplots)
gc()

# keep california only
plots <- subset(plots, STATECD==6 & !is.na(STATECD))

# read previously prepped tree data
data <- readRDS("./data/kr_fia_data_products/data2.RDS")
data <- subset(data, LAT_LON%in%plots$LAT_LON)
gc()

# load in david ackerly's niche means 
cwdniches <- read.csv("./data/climNicheData/cchCWD.csv")
aetniches <- read.csv("./data/climNicheData/cchAET.csv")

# restructure so that the vector for each climate variable's summary
# statistic has the climate variable in the vector name
cwdniches$climVar <- NULL
names(cwdniches)[names(cwdniches)!="name"] <- paste("cwd",
                                                names(cwdniches)[names(cwdniches)!="name"],
                                                sep="_")
aetniches$climVar <- NULL
names(aetniches)[names(aetniches)!="name"] <- paste("aet",
                                                names(aetniches)[names(aetniches)!="name"],
                                                sep="_")

data$name <- paste(data$GENUS, data$SPECIES, sep=" ")
data <- merge(data, cwdniches, by="name", all.x=TRUE, all.y=FALSE, sort=FALSE)
data <- merge(data, aetniches, by="name", all.x=TRUE, all.y=FALSE, sort=FALSE)

# calculate basal area from diameter
data$basal_area <- (pi/(4*144)) * ((data$DIA)^2)

# filter out trees that fall
# within a macroplot but not within a standard subplot. this is an
# issue in the pnw but not the rm region.
data <- subset(data, DIST<=24)

# filter out a small subset of trees subject to procedural errors
# and changes:
data <- subset(data, RECONCILECD==1 | RECONCILECD==2 | is.na(RECONCILECD))

# identify vector names in "data" for which to compute community weighted values
library(stringr)
cwdnames <- names(data)[str_starts(string=names(data), pattern="cwd")]
aetnames <- names(data)[str_starts(string=names(data), pattern="aet")]
varnames <- c(cwdnames, aetnames)

# compute community weighted niche means (and optima, etc.)
registerDoParallel(cores) # AGAIN, THE NUMBER OF CORES MAY NEED TO BE ADJUSTED
d <- foreach(i=1:nrow(plots), .combine=rbind) %dopar% {
  trees <- data[data$LAT_LON==plots[i,"LAT_LON"],]
  trees <- subset(trees, STATUSCD==1 & INVYR==max(trees$INVYR))
  row <- c()
  for(j in 1:length(varnames)){
    row[j] <- sum(trees[c(varnames[j])]*trees$basal_area)/sum(trees$basal_area)
  }
  row
}
d <- as.data.frame(d)
names(d) <- varnames
plots <- cbind(plots, d)

rm(d, data)
gc()

# some plots had no living trees in the most recent census, so exclude these
plots <- na.exclude(plots)

# load and merge on chelsa climate data for each plot
library(raster)
coordinates(plots) <- c("LON", "LAT")
crs(plots) <- CRS("+proj=longlat +datum=NAD83")

cwd <- raster("./data/gis_data/CAcwd.tiff")
plots <- spTransform(plots, crs(cwd))
plots$cwd <- extract(cwd, plots)

aet <- raster("./data/gis_data/CAaet.tiff")
plots <- spTransform(plots, crs(aet))
plots$aet <- extract(aet, plots)

tmn <- raster("./data/gis_data/CAtmn.tiff")
plots <- spTransform(plots, crs(tmn))
plots$tmn <- extract(tmn, plots)

plots <- as.data.frame(plots)

# which community cwd statistic is best predicted by cwd?
corrs <- data.frame(commstat=varnames, cwdcorr=NA, aetcorr=NA, tmncorr=NA)
for(i in 1:nrow(corrs)){
  corrs[i, "cwdcorr"] <- cor(plots$cwd, plots[varnames[i]])
  corrs[i, "aetcorr"] <- cor(plots$aet, plots[varnames[i]])
  corrs[i, "tmncorr"] <- cor(plots$tmn, plots[varnames[i]])
}

corrs
# looks like the pmn and pmd values work best

cwd_pmd_mod <- lm(cwd ~ cwd_pmd, data=plots)
cwd_pmn_mod <- lm(cwd ~ cwd_pmn, data=plots)
summary(cwd_pmd_mod)
summary(cwd_pmn_mod)
# coefficients are very close to 1

aet_pmd_mod <- lm(aet ~ aet_pmd, data=plots)
aet_pmn_mod <- lm(aet ~ aet_pmn, data=plots)
summary(aet_pmd_mod)
summary(aet_pmn_mod)
# coefficients greatly exceed 1. undesirable property?
# look at other commstats with high corrs

aet_mwm_mod <- lm(aet ~ aet_mwm, data=plots)
summary(aet_mwm_mod)
# closer, but still 1.3

aet_opt_mod <- lm(aet ~ aet_opt, data=plots)
summary(aet_opt_mod)
# now down to only 0.33
# aet is much tricker than cwd. maybe cwd is a better variable to link
# species occurrence to climate from a causal perspective.




