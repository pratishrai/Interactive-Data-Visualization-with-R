library(animint2)
library(gistr)

data(WorldBank, package="animint2")

WorldBank2005 <- subset(WorldBank, year==2005)

WorldBankBefore2005 <- subset(WorldBank, 2000 <= year & year <= 2005)

plot <- function(df, x.var){
  data.frame(df, x.var=factor(x.var, c("year", "life expectancy")))
}
(visualization <- animint(
  scatter=ggplot()+
    geom_point(aes(x=life.expectancy, y=fertility.rate, color=region),
               data=plot(WorldBank2005, "life expectancy"))+
    geom_path(aes(x=life.expectancy, y=fertility.rate, color=region,
                  group=country),
              data=plot(WorldBankBefore2005, "life expectancy"))+
    geom_line(aes(x=year, y=fertility.rate, color=region, group=country),
              data=plot(WorldBank, "year"))+
    xlab("")+
    facet_grid(. ~ x.var, scales="free")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))
))

animint(out.dir="Interactive Data Visualization")

animint2gist(visualization, description = "My Interactive Visualization")
