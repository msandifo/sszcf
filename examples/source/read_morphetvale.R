source("~/Dropbox/msandifo/documents/programming/r/sunshot-zcf/sszcf/examples/mnsp_funcs.R")


mv.df <-arrow::read_feather("examples/data/morphetvale.feather")
ss.names <- mv.df$substation |> unique()
mv.hour.df <-mv.df |> 
  dplyr::group_by(year,month, hour) |>
  dplyr::summarise('average demand'=mean(MW, na.rm=T),
                   'minimum demand'=min(MW, na.rm=T),
                   MVA=mean(MVA, na.rm=T),
                   MVar=mean(MVar, na.rm=T))|>
  pivot_longer(-c(year,month, hour))

this.month=11

ggplot(mv.hour.df |> 
         subset(month==this.month & name %in% c("minimum demand")), 
       aes(hour, value, col=factor(year)))+
  facet_grid(~name)+
  hrbrthemes::theme_ipsum_tw()+
  theme(legend.position = "bottom" , legend.title = element_blank())+
  # facet_wrap(~month, ncol=4)+
  geom_line(size=.3)+
  geom_hline(yintercept=0,size=.3)+
  
  labs(caption=paste(ss.names,"-", month.name[this.month]),y="MW", x="hour of day")

ggsave("./examples/figs/mv_byhourofday_nov.png", width=7, height=5.5)

# 
# d.f |> str()
# #d.f <- map_df(ms.files, readr::read_csv , skip=3)
# 
# #stringr::str_join(d.f.names ,names(d.f) )
# #d.f
# 
# n.1 <-d.f.names[seq(4, length(d.f.names), 3)] |> rep(each=3)
# 
# n.2 <-(names(d.f)[3: length(names(d.f))] |> stringr::str_split('_'))  |> unlist()
# 
# names( d.f) <- c("date", "time", stringr::str_c(n.1, n.2[ stringr::str_detect(n.2, "M")], sep=" "))
# 
#  
# 
# d.f.1 <-d.f |> tidyr::pivot_longer(-c(date,time)) |> 
#   subset(stringr::str_detect(name,sub.filter ) & stringr::str_detect(name,"MW" ) ) |> 
#   dplyr::mutate(date.time=lubridate::dmy_hms(stringr::str_c(date, time, sep= " ")))


library(tidyverse)

ggplot(mv.ss.df |> 
         mutate(mday= lubridate::mday(Date))|>
         subset(year==2019 & month== 11 & mday>0 & mday<15)  
       , aes( Date, MW))+
  geom_line(size=.15, col="grey20")+
  
  hrbrthemes::theme_ipsum_tw()+
  theme(legend.position = "bottom" , 
        legend.title = element_blank())+
  # facet_wrap(~month, ncol=4)+
  geom_hline(yintercept=0, col="black", size=.3)+
  labs(subtitle=ss.names, 
       x=NULL)
ggsave("./examples/figs/mv.date.png", width=9, height=6)

extrafont::loadfonts()

ggplot(mv.ss.df , aes( Date, MW))+
  geom_line(size=.05, col="grey20")+
  
  hrbrthemes::theme_ipsum_tw()+
  theme(legend.position = "bottom" , 
        legend.title = element_blank())+
  # facet_wrap(~month, ncol=4)+
  geom_hline(yintercept=0, col="black", size=.3)+
  labs(subtitle=ss.names, 
       x=NULL)
ggsave("./examples/figs/mv.date.all.png", width=9, height=6)
