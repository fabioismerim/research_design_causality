##install.packages('causaldata')
#install.packages('modelsummary')

{
  library(tidyverse)
  library(modelsummary)
  }


df <- causaldata::Mroz %>% 
  filter(lfp == TRUE) %>% 
  mutate(earn = exp(lwg))

ggplot(df, aes(inc, y=earn)) +
  geom_point() +
  scale_x_log10() + scale_y_log10()

#conditional mean by college attendance

df %>% 
  group_by(wc) %>% 
  summarise(earn = mean(earn))

#conditional mean by bins
df %>% 
  mutate(inc_cut = cut(inc, 10)) %>% 
  group_by(inc_cut) %>% 
  summarise(earn = mean(earn))

## draw de LOESS and linear regression curves
