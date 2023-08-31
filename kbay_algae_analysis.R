# Kaneohe Bay algae decline analysis
# code originally written by Mary Donovan 2018
# edited by Morgan Winston
# last updated June 20 2023

# INITIALIZATION ----------------------------------------------------------
## load required packages
library(plyr)
library(reshape2)
library(knitr)
library(kableExtra)
library(dplyr)
library(mgcv)
library(plotrix)
library(vegan)
library(tidyr)
library(ggplot2)
library(scales)
'%!in%' <- function(x,y)!('%in%'(x,y)) # the opposite of %in%

setwd("C:/Users/Morgan.Winston/Documents/GitHub/kaneohe-bay-algae-decline/data/working") # change to your wd

# LOAD DATA & MANIPULATE -------------------------------------------------
raw <- read.csv("final_kbay_08212023.csv") # load entire dataset - transect/benthic survey lvl
raw$SurveyDate <- as.Date(raw$SurveyDate_benthic)
raw$X <- NULL

fishcombo_all <- read.csv("C:/Users/Morgan.Winston/Documents/GitHub/kaneohe-bay-algae-decline/data/working/new_fish.combo_08212023.csv") # slight different fish data:
## ^ here, date value corresponds to the actual fish survey date, whereas the fish data in the combined dataset was summarized over the month to match the benthic survey date

# drop reefs with very low invasive algae cover
raw <- raw[which(raw$Reef %!in% c(12, 17, 22)),]
fishcombo_all <- fishcombo_all[which(fishcombo_all$Reef %!in% c(12, 17, 22)),]

# make a index for time 
temp <- NA 
for(i in 0:(length(unique(raw$Year))-1)){
  t <- rep(2011+i,12) # create 12 reps of each year
   temp <- c(temp,t)
}
temp <- temp[2:length(temp)] # remove the initial NA value
fulltime <- data.frame(Month=rep(NA,length(temp)),Year=rep(NA,length(temp))) # create a placeholder df with month and year columns
fulltime$Year <- temp # fill in reps of year into this column
fulltime$Month <- rep(seq(1:12),length(unique(raw$Year))) # add in months (1-12) for each year
min(raw$SurveyDate)
fulltime <- fulltime[11:nrow(fulltime),] # remove months prior to 11/2011 from data frame (no surveys were done)
fulltime$time.ind <- seq(1:nrow(fulltime)) # add index for each month/year timepoint
raw <- left_join(raw,fulltime,by=c("Month","Year")) # add index to raw data
raw$dum <- 1 # dummy var

timego <- fulltime
timego$mo_yr <- paste('1/',timego$Month,'/',timego$Year,sep="")
timego$mo_yr <- as.Date(timego$mo_yr,'%d/%m/%Y')

# what dates were reefs surveyed? just some nice code to make a table don't always need to run
# raw$Month <- as.numeric(raw$Month)
# raw$Year <- as.numeric(raw$Year)
# temp <- ddply(raw[which(raw$Method == "photoquad"),], .(Reef,Month,Year,Method), 'nrow')
# temp$mo_yr <- paste(temp$Year,temp$Month,sep="_")
# temp <- dcast(temp, Reef~mo_yr, value.var='nrow')
# temp[is.na(temp)] <- '-'
# 
# for(i in 2:36) temp[,i] <- cell_spec(temp[,i],color=ifelse(temp[,i]=='-','grey','blue'))
# cn <- data.frame('mo_yr'=colnames(temp[2:36]))
# tp <- ddply(raw, .(Month,Year), 'nrow')
# tp$mo_yr <- paste(tp$Year,tp$Month,sep="_")
# cn <- join(cn,tp[c('Month','Year','mo_yr')],by='mo_yr')
# 
# temp <- temp[,c(1:4,7:13,5,6, 14, 16:22, 15, 24,25,23, 27, 26, 29, 30, 28, 31, 33, 34, 32, 36, 35)] # order by date
# cn <- cn[with(cn,order(Year,Month)),]
# colnames(temp)[2:36] <- cn$Month
# 
# kable(temp,'html',escape=F,align='c') %>% # table showing number of transects per month (should this be @ finer resolution, some reefs surveyed multiple times/month)
#   kable_styling(bootstrap_options=c('striped','condensed'),full_width=F,position='left') %>%
#   add_header_above(c(" "=1,"2011"=2,'2012'=10,'2013'=9,'2014'=3,'2015'=2,'2016'=3, '2017'=1, '2018' = 3, '2019' = 2))

# for results:
## what was the highest % cover of E/K reached across all reefs combined? when?
# ## what reef was % cover highest at? when?
# head(raw)
# raw$MonthYr <- paste(raw$Month, raw$Year)
# baywide_alg <- raw %>% group_by(Reef, MonthYr) %>%summarise('kapp'=mean(kapp,na.rm=T))
# head(baywide_alg)
# baywide_alg[which(baywide_alg$kapp == max(baywide_alg$kapp)),]

# GENERAL TREND OF DECLINE ------------------------------------------------------------------------------
detach(package:plyr)     
library(dplyr)

# summarize E/K cover per survey type/method per survey date per reef
temp <- raw %>% group_by(Reef,Method,Habitat,time.ind) %>% summarise('kapp'=mean(kapp,na.rm=T))
temp$dum <- 1
temp <- as.data.frame(temp)
temp$Reef <- as.factor(temp$Reef)

## Figure 3 - E/K Decline (Overall) ####
setwd("C:/Users/Morgan.Winston/Documents/GitHub/kaneohe-bay-algae-decline/outputs")
tiff(file='Figure 3.tif',height=3800,width=4800,res=600, compression = "lzw")
ggplot(temp, aes(x = time.ind, y = kapp)) + 
  geom_point(size=2) +
  ylab("E/K Percent Cover (%) \n") +
  xlab("") + 
  theme_bw() +
  theme(
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "White"),
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  stat_smooth(method = "loess") +
  #scale_y_continuous(lim = c(0,40)) +
  scale_x_continuous(breaks=c(1,13,25,37,49,61,73,85,97), labels=format(timego$mo_yr[timego$time.ind %in% c(1,13,25,37,49,61,73,85,97)],'%m/%Y'))
dev.off()

# plot it by reef
ggplot(temp, aes(x = time.ind, y = kapp, color = Reef)) + 
  facet_wrap(~Reef) +
  geom_point(size=1.5) +
  ylab("E/K Percent Cover (%)") +
  xlab("") + 
  theme_bw() +
  theme(
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "White"),
    axis.title = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  stat_smooth(method = "loess") +
  scale_x_continuous(breaks=c(1,13,25,37,49,61,73,85,97), labels=format(timego$mo_yr[timego$time.ind %in% c(1,13,25,37,49,61,73,85,97)],'%m/%Y'))


# # summarize E/K cover per survey date per reef
# temp <- raw %>% group_by(Reef,time.ind) %>%summarise('kapp'=mean(kapp,na.rm=T)) # run this for reef level plotting of decline
# #temp <- raw %>% group_by(Reef,Method,Habitat,time.ind) %>%summarise('kapp'=mean(kapp,na.rm=T))
# temp$dum <- 1
# temp <- as.data.frame(temp)
# # temp$Habitat <- as.factor(temp$Habitat)
# # temp$Method <- as.factor(temp$Method)
# temp$Reef <- as.factor(temp$Reef)
# 
# timego <- fulltime
# timego$mo_yr <- paste('1/',timego$Month,'/',timego$Year,sep="")
# timego$mo_yr <- as.Date(timego$mo_yr,'%d/%m/%Y')
# 
# # summarized per survey day at each reef (so multiple transects per reef each day were summarized)
# ggplot(temp, aes(x = time.ind, y = kapp, color = Reef)) + 
#   geom_line(size=1) +
#   ylab("E/K Percent Cover (%)") +
#   xlab("") + 
#   theme_bw() +
#   theme(
#     panel.background = element_rect(colour = "white"),
#     panel.grid.major = element_line(colour = "white"),
#     panel.grid.minor = element_line(colour = "White"),
#     text = element_text(size = 20)
#   ) +
#   scale_x_continuous(breaks=c(1,13,25,37,49,61,73,85,97), labels=format(timego$mo_yr[timego$time.ind %in% c(1,13,25,37,49,61,73,85,97)],'%m/%y'))
# 


# DRIVERS OF DECLINE ####
#### check for correlations #
raw2 <- raw[!is.na(raw$H_bio),] # remove NA vals for herb biomass 

# check for correlations
full_pred <- raw2[,c("H_bio",
                     "Coral",
                     #"discharge",
                     "dhw",
                     "sst_all_mn",
                     "wind",
                     "rain_max",
                     "par")]
pred_cor <- cor(full_pred)
round(pred_cor,2)
corrplot::corrplot(pred_cor, method="circle") 
pairs(full_pred)


fvar <- raw2[,c('kapp','Habitat','Method','Reef','time.ind','SurveyDate',"Treatment", "Coral",
               "H_bio","sst_all_mn","dhw","wind","rain_max","par","dum")] # subset to what will go in the model
fvar.s <- fvar # create copy
fvar.s$Coral_p <- fvar.s$Coral/100 # proportion
for(i in c("H_bio", "sst_all_mn","wind","rain_max","par","dhw", "Coral_p")) fvar.s[i] <- scale(fvar.s[i])[,1] # scale each predictor
#fvar.s$Treatment <- as.factor(fvar.s$Treatment) # make random effects factors for gam to run properly
fvar.s$Habitat <- as.factor(fvar.s$Habitat)
fvar.s$Method <- as.factor(fvar.s$Method)
fvar.s$Reef <- as.factor(fvar.s$Reef)
fvar.s$Treatment <- as.factor(fvar.s$Treatment)

fvar.s$kapp_p <- fvar.s$kapp/100 # will run model on proportion data
kapp.gam.all <- gam((sqrt(fvar.s$kapp_p)) ~  s(time.ind, k=4) + # note square-root transformation
                      s(Habitat, bs='re', by = dum) +
                      s(Method, bs='re', by = dum) +
                      s(Reef, bs='re', by = dum) +
                      #s(Treatment, bs='re', by = dum) +
                      Treatment + # make sure it is a factor and look into what happens
                      s(H_bio, k=4) +
                      #s(Gs_p, k=4) +
                      s(Coral_p, k=4) +
                      s(dhw, k=4) +
                      s(sst_all_mn, k=4) +
                      s(wind, k=4) +
                      s(rain_max, k=4) +
                      s(par, k=4),
                    family = 'gaussian',
                    data = fvar.s,
                    correlation=corAR1(~time.ind),
                    method = "REML",
                    na.action='na.fail')
## Model Summary (Table S1) ####
summary(kapp.gam.all)

par(mfrow = c(2, 2))
gam.check(kapp.gam.all) # model checks for gam

## look at the effects of management 
newdat0 <- data.frame('time.ind'=mean(fvar.s$time.ind),
                     'Habitat'='Slope','Method'='LPI','Reef'= 10, 'dum'=0,
                     'H_bio'=mean(fvar.s$H_bio),
                     'Coral_p'=mean(fvar.s$Coral_p),
                     'dhw'=mean(fvar.s$dhw),
                     'sst_all_mn'=mean(fvar.s$sst_all_mn),
                     'par'=mean(fvar.s$par),
                     'rain_max'=mean(fvar.s$rain_max),
                     'wind'=mean(fvar.s$wind),
                     'Treatment'=0)
newdat1 <- data.frame('time.ind'=mean(fvar.s$time.ind),
                     'Habitat'='Slope','Method'='LPI','Reef'= 10,'dum'=0,
                     'H_bio'=mean(fvar.s$H_bio),
                     'Coral_p'=mean(fvar.s$Coral_p),
                     'dhw'=mean(fvar.s$dhw),
                     'sst_all_mn'=mean(fvar.s$sst_all_mn),
                     'par'=mean(fvar.s$par),
                     'rain_max'=mean(fvar.s$rain_max),
                     'wind'=mean(fvar.s$wind),
                     'Treatment'=1)
newdat <- rbind(newdat0,newdat1)

pred <- predict(kapp.gam.all,se.fit=T,newdata=newdat)
fits <- pred$fit^2; up <- (pred$fit+1*pred$se.fit)^2; down <- (pred$fit-1*pred$se.fit)^2
pred <- cbind(fits,up,down,newdat)
pred$down[2] <- 0
pred <- pred[with(pred,order(pred[,'Treatment'])),]
pred$fits_perc <- pred$fits*100
pred$up_perc <- pred$up*100
pred$down_perc <- pred$down*100

ggplot(pred, aes(x = Treatment, y = fits_perc)) +
  geom_point() +
  geom_errorbar(aes(ymin = down_perc, ymax = up_perc))

plot(c(1,2),pred$fits_perc,ylab="E/K percent cover (%)",xlab="Treatment",pch=21,col='black',cex=1.4,bg='grey85',main='',xlim=c(0.5,2.5),ylim=c(0,12),xaxt="n",cex.lab=1.3,cex.axis=1.3)
plotCI(c(1,2),pred$fits_perc,ui=pred$up_perc,li=pred$down_perc,add=T,pch=NA)
axis(1, at=c(1,2),labels=c('absent','present'))

## Figure 4: Predictor Effects ####
# for each predictor, plot the smoother and +/- 2 standard errors around the smoother 
## the y-axis is not the same as the raw data, because the smoother does not include the intercept
tiff(file='Figure 4.tif',height=3500,width=3500,res=600, compression = "lzw")
par(mfrow=c(2,3),mar=c(3,3,2,1),mgp=c(1.7,.6,0)) 
plot(kapp.gam.all,select=c(6),shade=T,xlab='Live Coral Cover',ylab='s(Live Coral Cover)',cex.lab=1.3,cex.axis=1.3)
plot(kapp.gam.all,select=c(5),shade=T,xlab='Herbivore Biomass',ylab='s(Herbivore Biomass)',cex.lab=1.3,cex.axis=1.3)
plot(kapp.gam.all,select=c(8),shade=T,xlab='Sea Surface Temperature',ylab='s(SST)',cex.lab=1.3,cex.axis=1.3)
plot(kapp.gam.all,select=c(9),shade=T,xlab='Wind Speed',ylab='s(Wind Speed)',cex.lab=1.3,cex.axis=1.3)
plot(kapp.gam.all,select=c(10),shade=T,xlab='Rainfall',ylab='s(Rainfall)',cex.lab=1.3,cex.axis=1.3)
plot(c(1,2),pred$fits_perc,ylab="Predicted E/K Percent Cover (%)",xlab="Treatment",pch=21,col='black',cex=1.4,bg='grey85',main='',xlim=c(0.5,2.5),ylim=c(0,12),xaxt="n",cex.lab=1.3,cex.axis=1.3)
plotCI(c(1,2),pred$fits_perc,ui=pred$up_perc,li=pred$down_perc,add=T,pch=NA)
axis(1, at=c(1,2),labels=c('Absent','Present'))
dev.off()

## Figure 7. Driver Boxplots ####
head(raw)
str(raw)
raw$period <- NA
for(i in c(1:nrow(raw))){
  if(raw$Year[i] %in% c(2011, 2012)){
    raw$period[i] <- "pre"
  }
  if(raw$Year[i] %in% c(2017, 2018)){
    raw$period[i] <- "post"
  }
}

temp <- raw[,c("Coral", "sst_all_mn", "wind", "rain_max", "par", "H_bio", "period")]
temp <- temp[which(!is.na(temp$H_bio)),]
colnames(temp) <- c("Live Coral Cover (%)", 
                    "Sea Surface Temperature (°C)", 
                    "Wind Speed (mph)", 
                    "Rainfall (mm)", 
                    "Light", 
                    "Herbivore Biomass (g m^-2)", 
                    "Period")
temp <-  gather(temp, var, val, `Live Coral Cover (%)`:`Herbivore Biomass (g m^-2)`, factor_key=TRUE)
temp <- temp[which(temp$Period %in% c("pre", "post")),]
temp$Period <- factor(temp$Period, levels = c("pre", "post"))
levels(temp$Period) <- c("Pre Decline: 2011-2012", "Post Decline: 2017-2018")
temp<- temp[which(temp$var != "Light"),]

tiff(file='Figure_7.tif',height=5000,width=4600,res=600, compression = "lzw")
ggplot(temp, aes(x = Period, y = val, alpha = Period, color = Period, fill = Period)) +
  facet_wrap(~ var, scales = "free") +
  geom_boxplot() +
  ylab("Value") +
  xlab("") +
  scale_alpha_manual(values = c(0.6,0.6)) + 
  scale_fill_manual(values = c("forestgreen","deepskyblue4")) +
  scale_colour_manual(values = c("forestgreen","deepskyblue4")) +
  theme_bw() +
  theme(
    strip.text = element_markdown(),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
dev.off()

# HERBIVORE TRENDS ####

### Figure 5. Herbivore Biomass ####
# herbivore biomass through time --> uses fishcombo data 
head(fishcombo_all)
fishcombo_all$SurveyDate <- as.Date(fishcombo_all$SurveyDate)

fishcombo_all$Month <- as.integer(format(fishcombo_all$SurveyDate, "%m"))
fishcombo_all$Year <- as.numeric(format(fishcombo_all$SurveyDate, "%Y"))

# summarize hbio yearly
fishcombo_all %>% group_by(Year) %>% summarise('H_bio'=mean(H_bio,na.rm=T))
  
temp <- left_join(fishcombo_all, fulltime, by=c('Month','Year')) 
temp <- temp[!is.na(temp$time.ind),]
str(temp)
temp <- temp[which(temp$Reef %!in% c(12, 17, 22)),]
summary(temp)
temp$Reef <- as.factor(temp$Reef)
head(temp)

setwd("C:/Users/Morgan.Winston/Documents/GitHub/kaneohe-bay-algae-decline/outputs")
tiff(file='Figure_5.tif',height=2000,width=2700,res=300, compression = "lzw")
ggplot(temp, aes(x = time.ind, y = H_bio)) +
  geom_point(size=2) +
  ylab(expression(paste("Herbivore Biomass (g ", m^-2,")"))) +
  xlab("") + 
  theme_bw() +
  theme(
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "White"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  stat_smooth(method = "loess") +
  #scale_y_continuous(limits = c(-1,41)) +
  scale_x_continuous(breaks=c(1,13,25,37,49,61,73,85,97), labels=format(timego$mo_yr[timego$time.ind %in% c(1,13,25,37,49,61,73,85,97)],'%m/%Y'))
dev.off()

# herbivore community composition
detach(package:plyr)  
library(dplyr)
herbs <- raw %>% group_by(Month,Year,Reef) %>% summarise(Hgd=median(Hgd,na.rm=T),Hbrow=median(Hbrow,na.rm=T),Hscex=median(Hscex,na.rm=T),kapp=median(kapp,na.rm=T)) %>% ungroup() ## do something better for the kapp numbers?
herbs <- herbs[!is.na(herbs$Hgd),]
herbs$sum <- rowSums(herbs[4:6])
herbs <- herbs[!herbs$sum==0,]
herbs.f <- log(herbs[4:6]+1)
herbs.mds <- suppressMessages(metaMDS(herbs.f,trace=0))

## Figure 6. Herbivore Community ####
tiff(file='Figure 6.tif',height=1500,width=2700,res=300, compression = "lzw")
par(mar=c(0, 0, 0, 0))
plot(herbs.mds,xlim=c(-2,2.1),xaxt='n',yaxt='n',ylab='',xlab='')
points(herbs.mds, cex=herbs$kapp/10, pch=19, col='darkgreen')
reg.arrow <- scores(herbs.mds,choices=1:3,display="sp",scaling=2)
arrows(0,0,reg.arrow[,1],reg.arrow[,2],length=0,lty=1,cex=3,lwd=1.5,col="black")
text(-0.85,0.4,'grazers',cex=1.2)
text(-1.8,0.5,'browsers',cex=1.2)
text(0,-0.3,'scrapers',cex=1.2)
text(1.9,.9,expression(italic('E/K cover')),cex=1)
text(1.7,0.6,expression(italic('Less')),cex=1)
text(2.1,0.6,expression(italic('More')),cex=1)
draw.circle(1.7,0.75,0.02, col = "darkgreen", border = "darkgreen")
draw.circle(2.1,0.75,0.08, col = "darkgreen", border = "darkgreen")
arrows(1.75,0.75,2,0.75, length = 0.1)
dev.off()


# SUPPLEMENTAL FIGURES ------------------------------------------------------

## Figure S1: Survey Effort Per Reef ####
surveys <- aggregate(list(n = raw$Transect), by = list(date = raw$SurveyDate, Reef = raw$Reef, Method = raw$Method, Treatment = raw$Treatment), function(x) length(unique(x)))
surveys$Reef <- as.factor(surveys$Reef)
surveys$Method <- as.factor(surveys$Method)
surveys$date <- as.Date(surveys$date)
surveys$Treatment <- as.factor(surveys$Treatment)

surveys$t_date <- NA
surveys$t_date <- as.Date(surveys$t_date)
for(i in c(1:nrow(surveys))){
  if(surveys$Reef[i] == 9){
    surveys$t_date[i] <- "2019-05-01"
  }
  if(surveys$Reef[i] == 23){
    surveys$t_date[i] <- "2019-03-01"
  }
  if(surveys$Reef[i] == 28){
    surveys$t_date[i] <- "2017-09-01"
  }
  if(surveys$Reef[i] == 10){
    surveys$t_date[i] <- "2014-10-01"
  }
  if(surveys$Reef[i] == 14){
    surveys$t_date[i] <- "2015-10-01"
  }
  if(surveys$Reef[i] == 16){
    surveys$t_date[i] <- "2015-07-01"
  }
  if(surveys$Reef[i] == 19){
    surveys$t_date[i] <- "2014-10-01"
  }
  if(surveys$Reef[i] == 26){
    surveys$t_date[i] <- "2011-01-01"
  }
  if(surveys$Reef[i] == 27){
    surveys$t_date[i] <- "2012-03-01"
  }
  if(surveys$Reef[i] == 29){
    surveys$t_date[i] <- "2012-08-01"
  }
}

surveys$date <- as.POSIXct(surveys$date, "%Y-%m-%d")
surveys$t_date <- as.POSIXct(surveys$t_date, "%Y-%m-%d")
surveys$Method <- as.character(surveys$Method)
surveys[which(surveys$Method == "photoquad"),]$Method <- "Photoquadrat"
surveys$Method <- as.factor(surveys$Method)

setwd("C:/Users/Morgan.Winston/Documents/GitHub/kaneohe-bay-algae-decline/outputs")
tiff(file='Figure A1_new.tif',height=5000,width=4800,res=300, compression = "lzw")
ggplot(surveys, aes(x=date, y=n, color = Method)) + 
  geom_jitter(size = 3) +
  geom_vline(data = surveys, mapping = aes(xintercept = t_date), show.legend = F) +
  scale_linetype(name = " ") +
  facet_wrap(~ Reef, ncol = 2) +
  xlab("") +
  ylab("Transects Conducted (#) \n") +
  scale_x_datetime(limits = c(min(surveys[which(!is.na(surveys$t_date)),]$t_date), 
                              max(surveys$date)), 
                   date_breaks = "12 months", labels = date_format("%m/%Y")) +
  #xlim(c(min(surveys[which(!is.na(surveys$t_date)),]$t_date), max(surveys$date))) +
  theme_bw() +
  theme(
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "White"),
    legend.position = "bottom",
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
dev.off()


## Figure S2: E/K Decline By Reef ####
temp <- raw
raw$MonthYr <- format(as.Date(raw$SurveyDate), "%Y-%m")
reef_sum <- data.frame(aggregate(list(kapp = raw$kapp), by = list(Reef = raw$Reef, date = raw$MonthYr), mean))
reef_sum2 <- data.frame(aggregate(list(kapp_se = raw$kapp), by = list(Reef = raw$Reef, date = raw$MonthYr), std.error))
reefs <- merge(reef_sum, reef_sum2)

reefs$Reef <- as.factor(reefs$Reef)
reefs$date <- format(as.Date(paste(reefs$date, "-01", sep = "")), "%Y-%m-%d")
reefs$date <- as.Date(reefs$date)

setwd("C:/Users/Morgan.Winston/Documents/GitHub/kaneohe-bay-algae-decline/outputs")
tiff(file='Figure A2_new.tif',height=5000,width=4800,res=300, compression = "lzw")

ggplot(reefs, aes(x = date, y = kapp, color = Reef)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=kapp-kapp_se, ymax=kapp+kapp_se), width=.2,
                position=position_dodge(0.05)) +
  geom_line(group = 1) + 
  facet_wrap(~ Reef, ncol = 2) +
  theme_bw() +
  theme(
    panel.background = element_rect(colour = "white"),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "White"),
    legend.position = "none",
    strip.text = element_text(size = 22),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  xlab("") +
  ylab("E/K Percent Cover (%) \n") +
  scale_x_date(breaks = "12 months", labels = date_format(format = "%m/%Y"))

dev.off()

