filepath <- 'raw_data//'
library(tidyverse)

t4 <- read_csv(str_c(filepath, "cfu_results_T4.csv"))
t8 <- read_csv(str_c(filepath, "cfu_results_T8.csv"))

t4_dat <- select(t4, 1:7)
t8_dat <- select(t8, 1:7)

t4_long <- t4_dat %>% 
  gather(Replicate, cfu, 2:6, -Media)

t8_long <- t8_dat %>% 
  gather(Replicate, cfu, 2:6, -Media)
 
t4_long <- mutate(t4_long, cfu = as.double(cfu))
t8_long <- mutate(t8_long, cfu = as.double(cfu))

remove_master <- function(df) {
  df %>% filter(!str_detect(Treatment, "\\*"))
}

t4_long <- t4_long %>% remove_master()
t8_long <- t8_long %>% remove_master()

rm_uncountable <- function(df) {
  df %>% mutate(Treatment = ifelse(str_detect(Treatment, "\\(P\\)"), str_extract(Treatment, ".+?(?=\\W\\()"), Treatment))
}

t4_long <- t4_long %>% rm_uncountable()
t8_long <- t8_long %>% rm_uncountable()

t4_plot <- t4_long %>%
  ggplot(aes(Treatment, cfu)) + 
  geom_point() + 
  facet_wrap(~Media) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

t8_plot <- t8_long %>%
  filter(!is.na(Media)) %>%
  ggplot(aes(Treatment, cfu)) + 
  geom_point() + 
  facet_wrap(~Media) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

ggpubr::ggarrange(t4_plot, t8_plot, nrow = 2)


# log(cfu)
t4_long %>%
  ggplot(aes(Treatment, log(cfu))) + 
  geom_point() + 
  facet_wrap(~Media) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) 

t4_long %<>% mutate(t = 1)
t8_long %<>% mutate(t = 2)

bind_rows(t4_long, t8_long) %>%
  filter(str_detect(Treatment, "R4"), Media == "SMSA_Ralstonia") %>%
  ggplot(aes(t, cfu)) +
  geom_point(aes(colour = Replicate, group = interaction(Treatment, Replicate))) +
  geom_line(aes(colour = Replicate, group = interaction(Treatment, Replicate))) +
  facet_wrap(~Treatment, scales = "free_y")

bind_rows(t4_long, t8_long) %>%
  filter(str_detect(Treatment, "R4"), Media == "Agar_Pseudomonas") %>%
  ggplot(aes(t, cfu)) +
  geom_point(aes(colour = Replicate, group = interaction(Treatment, Replicate))) +
  geom_line(aes(colour = Replicate, group = interaction(Treatment, Replicate))) +
  facet_wrap(~Treatment, scales = "free_y")

bind_rows(t4_long, t8_long) %>%
  filter(str_detect(Treatment, "R4"), Media %in% c("Agar_Pseudomonas","SMSA_Ralstonia")) %>%
  ggplot(aes(t, log(cfu))) +
  geom_point(aes(colour = Media, group = interaction(Treatment, Replicate, Media))) +
  geom_line(aes(colour = Media, group = interaction(Treatment, Replicate, Media))) +
  facet_wrap(~Treatment, scales = "free_y")

dat_summary <- dat_long %>%
  group_by(Treatment, Media) %>%
  summarise(mean_cfu = mean(cfu, na.rm = T), sd_cfu = sd(cfu, na.rm = T))
