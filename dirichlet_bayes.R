# install.packages("brms")
library(tidyverse)
library(brms)
A = rbind(
  c(0.2, 0.3, 0.5),
  c(0.8, 0.1, 0.1)
)
# x = 0.97, 0.54
df = data.frame(x = rnorm(2))
df$A = A
m = brm(A ~ x, data = df, family = dirichlet())

res  = predict(m, data.frame(x = 0:3))
res_matrix = res[, 1, ]
rowSums(res_matrix)
res_df = as.data.frame(res_matrix) %>% 
  mutate(x = 0:3) %>% 
  pivot_longer(matches("[0-3]"))
ggplot(res_df) +
  geom_area(aes(x, value, fill = name))

# https://github.com/ATFutures/who3/tree/master/scenarios
# https://mastering-shiny.org/