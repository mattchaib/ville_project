filepath <- 'raw_data//'
library(tidyverse)

dat1 <- read_csv(str_c(filepath, "cfu_results_T4.csv"))

dat1 <- select(dat1, 1:7)

dat_long <- dat1 %>% 
  gather(Replicate, cfu, 2:6, -Media)
 
dat_long <- mutate(dat_long, cfu = as.double(cfu))
a
dat_long %>%
  ggplot(aes(Treatment, cfu)) + 
  geom_point() + 
  facet_wrap(~Media) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

dat_summary <- dat_long %>%
  group_by(Treatment, Media) %>%
  summarise(mean_cfu = mean(cfu, na.rm = T), sd_cfu = sd(cfu, na.rm = T))

