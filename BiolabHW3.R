library(ggplot2)
library(gapminder)
data("gapminder")

pp<-ggplot(data=arrange(filter(gapminder,year==2007),desc(gdpPercap)),
           aes(x=gdpPercap,y=lifeExp))+
  geom_point(col="black",fill="black") +
  theme_bw()+
  labs(x="GDP per capita",y="life expectancy") +
  annotate("text",
           x=arrange(filter(gapminder,year==2007),desc(gdpPercap))$gdpPercap[2:6],
           y=arrange(filter(gapminder,year==2007),desc(gdpPercap))$lifeExp[2:6]-0.5,
           label = arrange(filter(gapminder,year==2007),desc(gdpPercap))$country[2:6],
           col = "blue",size=4)+
  annotate("text",
           x=arrange(filter(gapminder,year==2007),desc(gdpPercap))$gdpPercap[1],
           y=arrange(filter(gapminder,year==2007),desc(gdpPercap))$lifeExp[1]+0.5,
           label = arrange(filter(gapminder,year==2007),desc(gdpPercap))$country[1],
           col = "blue",size=4)

pp
ggsave(filename = "pp.png",plot = pp,width = 8,height = 8)