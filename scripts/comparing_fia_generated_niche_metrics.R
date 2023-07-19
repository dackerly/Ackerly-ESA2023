options(scipen=999)

# load in processed FIA data from Rosenblad et al. (2023) PNAS
# niche means were calculated from western continental US, not just CA
data <- readRDS("./data/kr_fia_data_products/subplot_data.RDS")
data <- subset(data, timepoint=="after" & STATECD==6 & !is.na(STATECD))

library(ggplot2)
opt_meansimple <- ggplot(data, aes(x=tmp_nicheopt_after/10, y=tmp_nichemeansimple_after/10))+
  geom_point()+
  xlab("Modeled Niche Optimum (°C)")+
  ylab("Basal Area-Weighted Mean of Presences (°C)")+
  geom_abline(slope=1, intercept=0)+
  geom_smooth(method=lm)+
  theme_bw()
opt_meansimple
ggsave("./results/kr_fia_results/opt_meansimple.png", height=5, width=5)

opt_mean <- ggplot(data, aes(x=tmp_nicheopt_after/10, y=tmp_nichemean_after/10))+
  geom_point()+
  xlab("Modeled Niche Optimum (°C)")+
  ylab("Modeled Niche Mean (°C)")+
  geom_abline(slope=1, intercept=0)+
  geom_smooth(method=lm)+
  theme_bw()
opt_mean
ggsave("./results/kr_fia_results/opt_mean.png", height=5, width=5)

