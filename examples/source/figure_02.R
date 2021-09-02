library(ggplot2)

library(nord)

mv.df <-arrow::read_feather("data/morphetvale.feather")
ss.names <- mv.df$substation |> unique()

this.month=11 # determines month
mv.hour.df <-mv.df |> 
  dplyr::group_by(year,month, hour) |>
  dplyr::summarise('average demand'=mean(MW, na.rm=T),
                   'minimum demand'=min(MW, na.rm=T),
                   MVA=mean(MVA, na.rm=T),
                   MVar=mean(MVar, na.rm=T)) |>
  tidyr::pivot_longer(-c(year,month, hour))

plot.mv <- ggplot(mv.hour.df |> 
         subset(month==this.month & name %in% c("minimum demand") & year %in% c(2010,2011, 2012,2014, 2017, 2018,2019)), 
       aes(hour, value, col=factor(year)))+
  facet_grid(~name)+
 sszcf::theme_sszcf_tw(base_size = 15, 
                             axis_title_size = 15,
                             axis_col = "#FF9900",
                             grid_col = "#FF9900")+
  theme(legend.position = c(.15,.3), legend.title = element_blank(),
        legend.background = element_rect(fill="white", colour="white"))+
  # facet_wrap(~month, ncol=4)+
  geom_line(size=.5)+
  geom_hline(yintercept=0,size=.3)+
  scale_colour_nord("red_mountain" )+
  labs(caption=paste(ss.names,"- month of", month.name[this.month]),
       y="MW", 
       x="hour of day")