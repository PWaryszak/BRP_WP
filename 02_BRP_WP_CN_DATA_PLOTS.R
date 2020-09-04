#Plot NEW BRP DATA========
library(tidyverse)#install.packages() first if not in your local library
library(grid)
library(gridExtra)
library(sjPlot)

#LOAD and DATA:
NewDATA <- read.csv("WP_CN_DATA.csv")#
NewDATA <- NewDATA [ !is.na(NewDATA$C.percent),] #remove NA-s
NewDATA$C.percent <- ifelse(NewDATA$C.percent == 0, 0.001, NewDATA$C.percent)#convert 0 into 0.001 to run log-models
NewDATA$SliceLength.cm <- (NewDATA$DepthTo_cm - NewDATA$DepthFrom_cm) #round % to full numbers to run Poisson
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter_mm/10/2)^2)*NewDATA$SliceLength.cm  #/10 to onvert mm to cm,slice volume
NewDATA$dry_bulk_density.gcm3 <- NewDATA$Dryweight_g / NewDATA$SampleVolume.cm3

NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3 * NewDATA$C.percent/100

NewDATA$CarbonStock.Mgha <- (((NewDATA$CarbonDensity.gcm3  / 1000000 ) *100000000) * NewDATA$SliceLength.cm )
NewDATA$Core_in.mm <- (NewDATA$PipeLength_mm) - NewDATA$CompactionIn_mm 
NewDATA$Pipe_in.mm <- (NewDATA$PipeLength_mm) - NewDATA$CompactionOut_mm
NewDATA$Compaction_Correction_Value<- NewDATA$Core_in.mm/NewDATA$Pipe_in.mm
NewDATA$CarbonStock.Mgha_CORRECTED <- NewDATA$CarbonStock.Mgha * NewDATA$Compaction_Correction_Value



#Compare WP and WP3 site==========
RehabControl <- filter(NewDATA, site == "Rehab" | site=="Control") %>% #Site WP2 had only Rehab (Degraded & Control (Natural) present
  filter (DepthTo_cm <= 5) %>%
  group_by(coreID, site,WesternPort) %>%
  summarise(Stock = sum( CarbonStock.Mgha_CORRECTED, na.rm = T))


View(RehabControl)

ggplot(data = RehabControl, aes( y = Stock, x = WesternPort)) +
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2, size = 1) +
  stat_summary(fun.y = "mean", size = 3, geom = "bar", fill = "lightblue")+
  facet_grid(.~site)+
  theme_bw()+
  labs(x ="Two WP sites", y = "Carbon Stock (tonnes/hectare)")+
  theme(axis.text.x=element_text(size=12,colour = "black"),
        axis.text.y=element_text(size=12,colour = "black"),
        axis.title.y=element_text(size=16,colour = "black",face = "bold",),
        axis.title.x=element_text(size=16, face = "bold", hjust = 0.5),
        legend.position = "none",
        strip.text=element_text(size=11))


#Plot by by elvation:=====
a <- ggplot(RehabControl, aes(x = WesternPort, y = Stock, color = WesternPort)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid( .~site)+ geom_jitter( alpha = 0.4)+
  labs(x= "", y = bquote('Organic Carbon Stock  ' (Mg*~ha^-1)))+
  theme_bw() +
  ggtitle("Two Western Port sites (BRP)", subtitle = "Control [Natural], Rehab [Degraded]")+
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        strip.text=element_text(size=18),
        plot.title = element_text(size = 18, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
a

#Plot WP2 by Habitat-Type:=====
wp2 <- filter(NewDATA, WesternPort == "WP2")

aa <- ggplot(wp2, aes(x = habitat, y = CarbonStock.Mgha_CORRECTED, color = habitat)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid( .~site)+ geom_jitter( alpha = 0.4)+
  labs(x= "", y = bquote('Organic Carbon Stock  ' (Mg*~ha^-1)))+
  theme_bw() +
  ggtitle("Site WP2 (BRP)", subtitle = "Control [Natural], Rehab [Degraded]")+
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"),
        legend.position = "none",
        strip.text=element_text(size=18),
        plot.title = element_text(size = 18, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
aa

grid.arrange(a, aa)
two_plots <- arrangeGrob(a,aa, nrow=2)

ggsave(two_plots, dpi=600, width = 7, height = 9, filename = "TwoWPs.png")
       