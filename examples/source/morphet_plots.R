library(ggplot2)


 
#' Title
#'
#' @param hour  a numeric vetcor to be converted to an hour::minutes string
#'
#' @return
#' @export
#'
#' @examples time_string(runif(20)*100)
time_string<- function(hour) {
i_time_string <- function(hour) {this.hour <- floor(hour%%24)
 this.minute <- floor((hour -floor(hour))*60)
 
 if  (this.hour<10) lead.hour="0" else lead.hour=""
 if  (this.minute<10) lead.minute="0" else lead.minute=""
 paste0(lead.hour, this.hour,":", lead.minute,this.minute)
}
purrr::map_chr(hour,i_time_string)
   
}

#------
# Best et al, 2021,  wealth deciles

source.file<- "examples/data/solar_deciles.feather"
sd.df <-arrow::read_feather(source.file)
 
ggplot(sd.df, aes(decile, households))+ 
  geom_bar(stat = "identity", width = .6, size=0, fill="orange")+
  ylim(c(0,30))+
  hrbrthemes::theme_ipsum_tw()+
  labs(caption= "data sourced from ABS 2017–18 Survey of Income and Housing\n analysis by Best et al. 2021, 'Equity and effectiveness of Australian small-scale solar schemes' ",
    y="% households", 
    x= "wealth decile")+
  scale_x_continuous(breaks= 1:10, label=round(1:10,0))+
   hrbrthemes::theme_modern_rc(base_size = 15, 
                               axis_title_size = 15)+
   theme(legend.position = c(.3,1), legend.title = element_blank(), 
         legend.direction = "horizontal")

 ggsave("examples/figs/deciles.png", width=9,height=6)
 
#----
# 

#
this.month=11 # set the month

red_mountain = c("#7D3232", "#7D4B4B", "#7D6464", "#AF967D", "#FAC87D", "#E1AF64","#C8964B","#32324B")

mv.hour.df <-mv.df |> 
  dplyr::group_by(year, month, hour) |>
  dplyr::summarise('average demand'=mean(MW, na.rm=T),
                   'minimum demand'=min(MW, na.rm=T),
                   MVA=mean(MVA, na.rm=T),
                   MVar=mean(MVar, na.rm=T)) |>
  tidyr::pivot_longer(-c(year,month, hour))

 ggplot(mv.hour.df |> 
                    subset(month==this.month & name %in% c("minimum demand") & year %in% c(2010,2011, 2012,2014, 2017, 2018,2019)), 
                  aes(hour+.25, value, col=factor(year)))+
  facet_grid(~name)+
  hrbrthemes::theme_modern_rc(base_size = 15, 
                             axis_title_size = 15)+
  theme(legend.position = c(.3,1), legend.title = element_blank(), 
        legend.direction = "horizontal")+
  # facet_wrap(~month, ncol=4)+
  geom_line(size=.5)+
  geom_hline(yintercept=0,size=.2, col="white", alpha=.35,linetype=1)+
  scale_colour_manual(values=red_mountain )+
   scale_x_continuous(breaks=seq(0,24,3), labels=time_string(seq(0,24,3))) +
   labs(#caption=paste(ss.names,"- month of", month.name[this.month]),
       y="MW", 
       x="hour of day",
       title= paste(ss.names,"- minimum load,", month.name[this.month]))

  ggsave("examples/figs/mv.hour_minium_load.png", width=9,height=6)
 
#-----
   
  mv.hour.year.df <-mv.df |> 
    dplyr::group_by(year,  hour) |>
    dplyr::summarise('average demand'=mean(MW, na.rm=T),
                     'minimum demand'=min(MW, na.rm=T),
                     MVA=mean(MVA, na.rm=T),
                     MVar=mean(MVar, na.rm=T)) |>
    tidyr::pivot_longer(-c(year,  hour))
  
  
  mv.hour.year.df.2011  <-  mv.df|> 
    dplyr::group_by(year,  hour) |>
    subset(   year %in% c(2011)) |>
    dplyr::summarise('average demand'=mean(MW, na.rm=T),
                     'minimum demand'=min(MW, na.rm=T),
                     MVA=mean(MVA, na.rm=T),
                     MVar=mean(MVar, na.rm=T)) |>
    tidyr::pivot_longer(-c(year,  hour))
  
  length(mv.hour.year.df$value)/length(mv.hour.year.df.2011$value) 
  
  ggplot( mv.hour.year.df  |>
            dplyr::mutate(value1= value/mv.hour.year.df.2011$value )|>
           subset(name %in% c("average demand") & 
                  year %in% c( 2011, 2012,2014, 2017, 2018,2019))  , 
         aes(hour+.25, 100*value1, col=factor(year)))+
    hrbrthemes::theme_modern_rc(base_size = 15, 
                                axis_title_size = 15)+
    theme(legend.position = c(.5,.15), legend.title = element_blank(), 
          legend.direction = "horizontal")+
    # facet_wrap(~month, ncol=4)+
    geom_line(size=.5)+
    geom_hline(yintercept=0,size=.2, col="white", alpha=.35,linetype=1)+
    scale_colour_manual(values=red_mountain )+
    scale_x_continuous(breaks=seq(0,24,3), labels=time_string(seq(0,24,3))) +
    labs(#caption=paste(ss.names,"- month of", month.name[this.month]),
      y="% 2011 levels", 
      x="hour of day",
      title= paste(ss.names,"- average load, full year, referenced against 2011" ))
  ggsave("examples/figs/mv.hour-av_load_less2011.png", width=9,height=6)
  
  
  ggplot(mv.hour.year.df |> 
           subset(name %in% c("average demand") & 
                    year %in% c(2010,2011, 2012,2014, 2017, 2018,2019)), 
         aes(hour+.25,  value, col=factor(year)))+
    hrbrthemes::theme_modern_rc(base_size = 15, 
                                axis_title_size = 15)+
    theme(legend.position = c(.35,.9), legend.title = element_blank(), 
          legend.direction = "horizontal")+
    # facet_wrap(~month, ncol=4)+
    geom_line(size=.5)+
    geom_hline(yintercept=0,size=.2, col="white", alpha=.35,linetype=1)+
    scale_colour_manual(values=red_mountain )+
    scale_x_continuous(breaks=seq(0,24,3), labels=time_string(seq(0,24,3))) +
    labs(#caption=paste(ss.names,"- month of", month.name[this.month]),
      y="MW", 
      x="hour of day",
      title= paste(ss.names,"- average load, full year" ))
  ggsave("examples/figs/mv.hour-av_load_year.png", width=9,height=6)
  
  
  #-----
  
  ggplot(mv.df |>
           dplyr::mutate(Date=lubridate::decimal_date(  Date)),  
         aes(  Date, MW))+
   
    hrbrthemes::theme_modern_rc(base_size = 15, 
                                axis_title_size = 15)+
    theme(legend.position = c(.3,1), legend.title = element_blank(), 
          legend.direction = "horizontal")+
    # facet_wrap(~month, ncol=4)+
    geom_line(size=.05)+
    scale_x_continuous(breaks =2011:2020)+
    geom_hline(yintercept=0,size=.2, col="white", alpha=.35,linetype=1)+
   
       labs(#caption=paste(ss.names,"- month of", month.name[this.month]),
      y="MW", 
 
      title= paste(ss.names,"- load" ))
  
  ggsave("examples/figs/mv.date_load.png", width=9,height=6)
  
  #-----
  
  

mv.df <-arrow::read_feather("examples/data/morphetvale.feather")
ss.names <- mv.df$substation |> unique()

mv.mon.df <- mv.df |>  
 # subset(hour > 10 & hour< 15) |>
  dplyr::group_by(year, month, hour) |>
  dplyr::summarise(MWm=min(MW, na.rm=T),
                   MW=mean(MW, na.rm=T),
                   
                   MVA=mean(MVA, na.rm=T),
                   MVar=mean(MVar, na.rm=T))  

time_string(0:24)

mv.mon.fac.df <-mv.mon.df |>
  dplyr::mutate(hour=floor(hour)+.5, month.lab=  month.name[month])|>
  subset(month %in% c(2,5,8, 11))
  
mv.mon.fac.df$month.lab <- factor(mv.mon.fac.df$month.lab, levels = month.name)   
 
ggplot( mv.mon.fac.df , 
        aes(hour, year, fill=MWm, label=round(MWm,1)))+
  facet_wrap(~month.lab, ncol=2  )+
  geom_tile( col="black",size=.6)+
  geom_text(data= mv.mon.fac.df  |> subset(MWm<0  ), size=4, label="-", col="white")+
  hrbrthemes::theme_modern_rc()+
  theme(strip.text.x=element_text(colour="white",),
        strip.text.y=element_text(colour="white"),
       panel.spacing.x = unit(-.5, "lines"),
       panel.spacing.y = unit(.5, "lines"))+
  scale_y_continuous(breaks=seq(2010,2020,1))+
  scale_x_continuous(breaks=seq(0,24,6), labels=time_string(seq(0,24,6))) +
  scale_fill_gradient2(  high = "firebrick1",
                         mid = "white",
                         low =  "blue" ,
                         midpoint = 5,
                         na.value = "black")+
  theme(legend.position = "bottom", legend.title = element_blank())+
  labs( x=NULL, y=NULL,
        title= paste(ss.names,"- minimum load MW"),
        caption= "")#Sunshot ZCF" ) 

ggsave("examples/figs/mv.min.load.png", width=14/1.,height=9/1.15)

ggplot( mv.mon.fac.df , 
        aes(hour, year, fill=MW, label=round(MW,1)))+
  facet_wrap(~month.lab, ncol=2  )+
  geom_tile( col="black",size=.6)+
  geom_text(data= mv.mon.fac.df  |> subset(MW<0  ), size=4, label="-", col="white")+
  hrbrthemes::theme_modern_rc()+
  theme(strip.text.x=element_text(colour="white",),
        strip.text.y=element_text(colour="white"),
        panel.spacing.x = unit(-.5, "lines"),
        panel.spacing.y = unit(.5, "lines"))+
  scale_y_continuous(breaks=seq(2010,2020,1))+
  scale_x_continuous(breaks=seq(0,24,6), labels=time_string(seq(0,24,6))) +
  scale_fill_gradient2(  high = "firebrick1",
                         mid = "white",
                         low =  "blue" ,
                         midpoint = 10,
                         na.value = "black")+
  theme(legend.position = "bottom", legend.title = element_blank())+
  labs( x=NULL, y=NULL,
        title= paste(ss.names,"- average load MW"),
        caption= "")#Sunshot ZCF" ) 

ggsave("examples/figs/mv.average.load.png", width=14/1,height=9/1.15)

