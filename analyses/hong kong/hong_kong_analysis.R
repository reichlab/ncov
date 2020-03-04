library(MMWRweek)
hk_dat <- readxl::read_xlsx("analyses/hong kong/flux_data.xlsx")

colnames(hk_dat)
plot(hk_dat[,6],type='l')

clean_hk_dat <- hk_dat[,1:3]
clean_hk_dat$ili <- hk_dat[,6]
clean_hk_dat$pp <-hk_dat[,11]
colnames(clean_hk_dat) <- c('year','week','from','ili','pp')
clean_hk_dat <-clean_hk_dat[3:323,]
clean_hk_dat$year <- as.numeric(clean_hk_dat$year)
clean_hk_dat$pp <- as.numeric(c(clean_hk_dat$pp$...11))
clean_hk_dat$ili <- as.numeric(c(clean_hk_dat$ili$...6))


clean_hk_dat$season_week <- ifelse(
  clean_hk_dat$week <= 30,
  clean_hk_dat$week + MMWRweek(MMWRweek:::start_date(clean_hk_dat$year) - 1)$MMWRweek - 30,
  clean_hk_dat$week - 30
)



clean_hk_dat$season <- ifelse(
  clean_hk_dat$week <= 39,
  paste0(clean_hk_dat$year - 1, "/", clean_hk_dat$year),
  paste0(clean_hk_dat$year, "/", clean_hk_dat$year + 1)
)
library(dplyr)
hk_baseline <- clean_hk_dat %>% dplyr::group_by(season) %>% dplyr::summarise(baseline=mean(ili[1:2]))
clean_hk_dat <- clean_hk_dat %>% left_join(hk_baseline)

library(ggplot2)
p_ilineg2_hk <- ggplot(clean_hk_dat, aes(y=(100 - pp) * (ili/baseline) / 100, x=season_week, color=season)) + 
  geom_line() +
  geom_line(data=dplyr::filter(clean_hk_dat, season=="2019/2020"), size=1) +
  ylab("(1 - proportion positive) * (wILI)") + xlab(NULL) +
  theme(legend.position = "bottom") +
  xlab("season week") 

ggsave(filename = "hk_analysis",plot = p_ilineg2_hk,device = "png")
