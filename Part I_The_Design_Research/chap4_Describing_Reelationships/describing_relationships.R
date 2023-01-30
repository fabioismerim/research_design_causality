##install.packages('causaldata')
#install.packages('modelsummary')

{
  library(tidyverse)
  library(modelsummary)
  }


df <- causaldata::Mroz %>% 
  filter(lfp == TRUE) %>% 
  mutate(earn = exp(lwg))

#scatter plot of inc by earn

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

## draw the LOESS 
ggplot(df, aes(inc, earn)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_log10() + scale_y_log10()

## plot linear regression
ggplot(df, aes(inc, earn)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_x_log10() + scale_y_log10()

## run a linear regression, by itself and including controls
model1 <- lm(formula = lwg ~ log(inc), data = df)

#k5 is number of kids under 5
model2 <- lm(lwg ~ log(inc) + wc + k5, data = df)

msummary(list(model1, model2))
