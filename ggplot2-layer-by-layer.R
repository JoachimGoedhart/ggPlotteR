#### R code snippet to show line-by-line coding with ggplot2


library("ggplot2")

df <- read.csv("gapminder-FiveYearData.csv")

p <- ggplot(data = df) +
  aes(x=continent) +
  aes(y=lifeExp) +
  geom_jitter() +
#  geom_boxplot(fill = NA, size=1) +
  aes(color=year) +
  
  coord_flip() +
  scale_y_continuous(limits=c(10,100)) +
  
  labs(color="Year") +
  labs(x = "Continent") +
  labs(y= "Life Expectency") +
  labs(title="Title of the graph") +
  stat_summary(fun.y = median, fun.ymin=median, fun.ymax=median, geom="crossbar") +

  NULL
  


p
