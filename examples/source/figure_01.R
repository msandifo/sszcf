source.file<- "data/solar_deciles.feather"
sd.df <-arrow::read_feather(source.file)
ggplot(sd.df, aes(decile, households))+ 
  geom_bar(stat = "identity", width = .6, size=0, fill="orange")+
  ylim(c(0,30))+
  hrbrthemes::theme_ipsum_tw()+
  labs(#caption= "data sourced from ABS 2017–18 Survey of Income and Housing",
       y="% households", 
       x= "wealth decile")+
  scale_x_continuous(breaks= 1:10, label=round(1:10,0))
