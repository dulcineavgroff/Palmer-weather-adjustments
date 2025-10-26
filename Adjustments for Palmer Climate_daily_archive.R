# Adjustments for Palmer Climate -- version 3.0
library(ggplot2)
library(plotly)
#library(tidyverse)
library(lubridate)
library(plyr)
library(dplyr)
library(scales)

# 22-mo overlap PALMOS and PAWS comparison #####    
# create dates and variables
{
  
  setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/ERA5 adjustment data for Palmer")
  palmos <- read.csv("table_214.csv",header=TRUE)
  # makre air pressure numeric
  palmos$Air.Pressure..mbar. <- as.numeric(palmos$Air.Pressure..mbar.)
  # make wind speed and wind direction numeric to calc u and v wind components
  palmos$WS.avg.2min..m.s. <- as.numeric(palmos$WS.avg.2min..m.s.)
  palmos$WD.avg.2min..º. <- as.numeric(palmos$WD.avg.2min..º.)
  # calculate u and v wind components 
  palmos$v <- -palmos$WS.avg.2min..m.s. * cos(palmos$WD.avg.2min..º. *pi/180)
  palmos$u <- -palmos$WS.avg.2min..m.s. * sin(palmos$WD.avg.2min..º. *pi/180)
  # format date and time
  palmos$dates <- ymd(palmos$Date)
  palmos$year <- format(as.Date(palmos$dates, format="Y%/%m/%d"),"%Y")
  palmos$month <- format(as.Date(palmos$dates, format="Y%/%m/%d"),"%m")
  palmos$day <- format(as.Date(palmos$dates, fromat="Y%/%m/%d"),"%d")
  palmos$year <- as.numeric(palmos$year,na.rm=T)
  palmos$month <- as.numeric(palmos$month,na.rm=T)
  palmos$day <- as.numeric(palmos$day,na.rm=T)
 # palmos$Time <- hms(palmos$Time)
  
  # format columns to match paws
  palmos <- palmos[,c(30,2,3,5,9,10,16,29,28,31,32,33,14)]
  colnames(palmos)[1:13] <- c("dates","Time","WS_avg_2min", "WD_avg_2min","Air_temp","Rel_humidity","Precip_melted","u","v","year","month","day","Pressure")
  
  # add april to september 2017 palmos data (discovered separately from orignal overlap period dataset)
  setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/2017")  
  palmos_2017 <- read.csv("palmos_2017_1min.csv", header=TRUE)
  palmos_2017 <- palmos_2017[,c(9,1,3,4,5,6,7,14,13,10,11,12,8)]
  colnames(palmos_2017)[1:13] <- c("dates","Time","WS_avg_2min", "WD_avg_2min","Air_temp","Rel_humidity","Precip_melted","u","v","year","month","day","Pressure")
  
  #combine palmos datasets for overlap period
  comb.palmos <- rbind.data.frame(palmos, palmos_2017)
  
  # remove rows that have NA in the day column (removes erroneous rows with all NAs)
  comb.palmos <- comb.palmos[!is.na(comb.palmos$day), ]
  
  # calculate daily values 
  # note that precip is calc'd different for paws and palmos (sum vs max)
  temp.palmos <- comb.palmos %>%
    group_by(year,month,day) %>%
    summarise(Air.Temp = mean(Air_temp, na.rm=TRUE), 
              WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              Rel.Humidity = mean(Rel_humidity, na.rm=TRUE),
              precip.melted = max(Precip_melted, na.rm=TRUE),
              Pressure = mean(Pressure, na.rm=TRUE))
  rm(palmos)
  rm(palmos_2017)
  
  # trim to only overlapping period 11- sept - 2015 to 30 September 2017
  temp.palmos <- temp.palmos[c(4998:5742),]
  
  
  # -------------------------------------------------  #
  setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate")
  paws <- read.csv("PAWS Data_2015-2017.csv", header=T)
  
  paws$WS.Avg.2min <- as.numeric(paws$WS.Avg.2min,na.rm=T)
  paws$WD.Avg.2min <- as.numeric(paws$WD.Avg.2min,na.rm=T)
  
  paws$v <- -paws$WS.Avg.2min * cos(paws$WD.Avg.2min*pi/180)
  paws$u <- -paws$WS.Avg.2min * sin(paws$WD.Avg.2min*pi/180)
  
  paws$dates <- mdy_hms(paws$SampleDateTime)
  paws$year <- format(as.Date(paws$dates, format="Y%/%m/%d"),"%Y")
  paws$month <- format(as.Date(paws$dates, format="Y%/%m/%d"),"%m")
  paws$day <- format(as.Date(paws$dates, format="Y%/%m/%d"),"%d")
  paws$year <- as.numeric(paws$year,na.rm=T)
  paws$month <- as.numeric(paws$month,na.rm=T)
  paws$day <- as.numeric(paws$day,na.rm=T)
  
  paws$Rel.Humidity <- as.numeric(paws$Rel.Humidity)
  # paws$WS_avg_2min <- as.numeric(paws$WS_avg_2min)
  # paws$Air.Temp <- as.numeric(paws$Air.Temp)
  
  temp.paws <- paws %>%
    group_by(year,month,day) %>%
    summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
              WS_avg_2min = mean(WS.Avg.2min, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
              precip.melted = sum(Rainfall, na.rm=TRUE),
              Pressure = mean(Air.Pressure, na.rm=TRUE)) 
  
  # makes a new date column with only yyyymmdd, removes time
  daily.paws <- temp.paws %>%
    mutate(date = make_date(year, month, day))
 
   daily.paws <- daily.paws[c(1:750),] # trim the PAWS data to sept 30, 2017
  
  daily.palmos <- temp.palmos %>%
    mutate(date = make_date(year, month, day))
}
  
# remove rows/days in paws that are missing in the palmos dataset
  daily.paws <- daily.paws[-c(52:54, 126,174,545,546),] # palmos is missing Nov 1, 2, 3, Jan 14; rm MAR 2nd too
  daily.palmos <- daily.palmos[-c(170,479,746),] # removes march 2, 2016 which is missing in paws, so need to remove from palmos
  # 05 JAN 2017 is missing in PAWS; rm from PalMOS and # 9-10 Mar 2017 is missing in PalMOS; rm from PAWS
  #write.csv(daily.paws, "daily paws overlap 2015-2017.csv")
  #write.csv(daily.palmos, "daily palmos overlap 2015-2017.csv")
  # check that all days match between the datasets
  daily.paws$day - daily.palmos$day # should all be zero

## Pressure #####
  # calculate mean sea level pressure for each station
  Tv <- 271.55 + 1 # == mean ann Temp (-1.6ºC) in Kelvin, plus 1 degree for virtual temperature
  daily.paws$Pressure <- daily.paws$Pressure + (40 / (29.27 * Tv)) # mslp = station pressure-hPa + (elevation-m / 9.2)
  daily.palmos$Pressure <- daily.palmos$Pressure + (6 / (29.27 * Tv))
  
  # daily.paws$Pressure <- daily.paws$Pressure + (30 / 9.2) # mslp = station pressure-hPa + (elevation-m / 9.2)
  # daily.palmos$Pressure <- daily.palmos$Pressure + (8 / 9.2)
  
  # plot(daily.paws$Pressure, daily.palmos$Pressure, xlab="PAWS Air Pressure (hPa)",cex=1.5,ylab="PALMOS Air Pressure (hPa)",xlim=c(975,1005), ylim=c(975,1005))
  # abline(a=0, b=1, lty=3, lwd=1)
  
  meandiff <- mean(daily.paws$Pressure) - mean(daily.palmos$Pressure)
  meandiff # old wrong result === -3.0 mBar or hpa
  # mslp result is -0.5635337
  
  
  # text(981,1002, substitute(paste(bold("mean monthly\nair pressure"))))
  # text(995,977, "Pearson's r = 0.999\nmean difference = 3.0 hPa")
  # 
  ### Fig 2a daily pressure 1:1 plot #####
  # 992 x 466
  # function for figure labels a, b, c
  {
    line2user <- function(line, side) {
      lh <- par('cin')[2] * par('cex') * par('lheight')
      x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
      y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
      switch(side,
             `1` = grconvertY(-line * y_off, 'npc', 'user'),
             `2` = grconvertX(-line * x_off, 'npc', 'user'),
             `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
             `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
             stop("Side must be 1, 2, 3, or 4", call.=FALSE))
    }
    
    addfiglab <- function(lab, xl = par()$mar[2], yl = par()$mar[3]) {
      
      text(x = line2user(xl, 2), y = line2user(yl, 3), 
           lab, xpd = NA, font = 2, cex = 1.5, adj = c(0, 1))
      
    }
  }
  
  par(mfrow=c(2,3))
  par(mar=c(4,6,0.5,0.5))
  
  {
    plot(daily.palmos$Pressure, daily.paws$Pressure,
         xlab=expression(bold("PalMOS SLP (hPa)")), ylab=expression(bold("PAWS SLP (hPa)")),
         xlim=c(950,1025), ylim=c(950,1025), col="transparent",
         cex.lab=1.5, cex.axis=1.5
    )
    abline(v=c(950,960,970,980,990,1000,1010,1020), h=c(950,960,970,980,990,1000,1010,1020), lty = 9, col = "grey33", lwd = 0.75)
    abline(a=0,b=1, lty = 9, col="grey33")
    text(970,1015,expression(bold("PalMOS—PAWS\nSpearman's ρ = 0.999\nn = 743")))
    #text(970,1010,expression(bold("PalMOS—PAWS\nSpearman's ρ = 0.999\nn = 743\nCF = 0.999")))
    points(daily.palmos$Pressure, daily.paws$Pressure, col="black")
    #points(daily.palmos$Pressure*0.999, daily.paws$Pressure, col="blue")
    #points(daily.palmos$Pressure-2.954838, daily.paws$Pressure, col="pink3")
    
    lsf <- lm(daily.paws$Pressure ~ daily.palmos$Pressure) # lm(y ~ x)
    summary(lsf)
   # rmse
    rmse <- sqrt(mean(lsf$residuals^2))
    round(rmse,1)
    
    cor.test(daily.palmos$Pressure, daily.paws$Pressure, method="pearson")
  }
  addfiglab("(a)")
  
  ### Fig 2b monthly air pressure PalMOS-PAWS overlap #####
  { 
    mo_palmos <- daily.palmos %>%
      group_by(month) %>%
      summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
                WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
                u = mean(u, na.rm=TRUE),
                v = mean(v, na.rm=TRUE),
                Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
                precip.melted = sum(precip.melted, na.rm=TRUE),
                Pressure = mean(Pressure, na.rm=TRUE))
    
    mo_paws <- daily.paws %>%
      group_by(month) %>%
      summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
                WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
                u = mean(u, na.rm=TRUE),
                v = mean(v, na.rm=TRUE),
                Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
                precip.melted = sum(precip.melted, na.rm=TRUE),
                Pressure = mean(Pressure, na.rm=TRUE))
    
    plot(mo_palmos$month, mo_palmos$Pressure, 
         ylim=c(978,1000),
         col=alpha("#ca0020",0.9), pch=0, lwd=3,cex=1.5, cex.lab=1.5, cex.axis=1.5,
         xlab=expression(bold("month")), 
         ylab=expression(bold("SLP (hPa)")),
    ) 
    points(mo_paws$month, mo_paws$Pressure, col=alpha("#0571b0",0.7),pch=1,lwd=3, cex=1.5)
    
    legend("topleft", legend=c(expression(bold("PalMOS")), expression(bold("PAWS"))), bg="transparent", bty="n",
           pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
  }
  addfiglab("(b)")
  
  #### Fig 2c mean monthly differences#####
  {
    diff <- mo_paws$Pressure - mo_palmos$Pressure
    
    plot(diff, 
         ylim=c(-2.7,-3.2),
         col=alpha("black",0.9), pch=15, lwd=3,cex=2.5, cex.lab=1.5, cex.axis=1.5,
         xlab=expression(bold("month")), 
         ylab=expression(bold("SLP (hPa)")))
    legend(1,-3.2, legend=c(expression(bold("PAWS minus PalMOS\nmonthly mean difference"))), bg="transparent", bty="n",
           pch=c(15), pt.cex = 2, pt.lwd=3, col="black")
  }
  addfiglab("(c)")
  
  
## Temperature #####
  ### stats for Table S1#####
  wilcox.test(daily.palmos$Air.Temp[1:719], daily.paws$Air.Temp[1:719], paired=T)
  cor.test(daily.palmos$Air.Temp[1:719], daily.paws$Air.Temp[1:719], method="spearman")
  
  ### Fig 2a daily temperature 1:1 plot #####
  # 992 x 466
  # function for figure labels a, b, c
  {
    line2user <- function(line, side) {
      lh <- par('cin')[2] * par('cex') * par('lheight')
      x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
      y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
      switch(side,
             `1` = grconvertY(-line * y_off, 'npc', 'user'),
             `2` = grconvertX(-line * x_off, 'npc', 'user'),
             `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
             `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
             stop("Side must be 1, 2, 3, or 4", call.=FALSE))
    }
    
    addfiglab <- function(lab, xl = par()$mar[2], yl = par()$mar[3]) {
      
      text(x = line2user(xl, 2), y = line2user(yl, 3), 
           lab, xpd = NA, font = 2, cex = 1.5, adj = c(0, 1))
      
    }
  }
  
  par(mfrow=c(2,3))
  par(mar=c(4,6,0.5,0.5))
  
  {
  plot(daily.palmos$Air.Temp[1:719], daily.paws$Air.Temp[1:719],
       xlab=expression(bold("PalMOS air temperature (ºC)")), ylab=expression(bold("PAWS air temperature (ºC)")),
       xlim=c(-17,7), ylim=c(-17,7), col="transparent",
     cex.lab=1.5, cex.axis=1.5
     )
abline(v=c(-15,-10,-5,0,5), h=c(-15,-10,-5,0,5), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(-11.5,3,expression(bold("PalMOS—PAWS\nSpearman's ρ = 0.989\nn = 719\nCF = 0.987")))

#text(-13.9,1.75, expression(bold("CF = 0.987")))
points(daily.palmos$Air.Temp[1:719], daily.paws$Air.Temp[1:719], col="black")

lsf <- lm(daily.paws$Air.Temp[1:719] ~ daily.palmos$Air.Temp[1:719]) # lm(y ~ x)
summary(lsf)
# rmse
rmse <- sqrt(mean(lsf$residuals^2))
round(rmse,1)


}
  addfiglab("(a)")
  
  ### Fig 2b monthly air temperature PalMOS-PAWS overlap #####
  { 
    mo_palmos <- daily.palmos %>%
      group_by(month) %>%
      summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
                WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
                u = mean(u, na.rm=TRUE),
                v = mean(v, na.rm=TRUE),
                Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
                precip.melted = sum(precip.melted, na.rm=TRUE))
    
    mo_paws <- daily.paws %>%
      group_by(month) %>%
      summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
                WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
                u = mean(u, na.rm=TRUE),
                v = mean(v, na.rm=TRUE),
                Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
                precip.melted = sum(precip.melted, na.rm=TRUE))
    
    plot(mo_palmos$month, mo_palmos$Air.Temp, 
         ylim=c(-7,5),
         col=alpha("#ca0020",0.9), pch=0, lwd=3,cex=1.5, cex.lab=1.5, cex.axis=1.5,
         xlab=expression(bold("month")), 
         ylab=expression(bold("air temperature (ºC)")),
    ) 
    points(mo_paws$month, mo_paws$Air.Temp, col=alpha("#0571b0",0.7),pch=1,lwd=3, cex=1.5)
    
    legend("topleft", legend=c(expression(bold("PalMOS")), expression(bold("PAWS"))), bg="transparent", bty="n",
           pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
  }
  addfiglab("(b)")
  
  #### Fig 2c mean monthly differences#####
 {
  diff <- mo_paws$Air.Temp - mo_palmos$Air.Temp
 
  plot(diff, 
       ylim=c(-1,0.5),
       col=alpha("black",0.9), pch=15, lwd=3,cex=2.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("air temperature (ºC)")))
  legend(1,0.45, legend=c(expression(bold("PAWS minus PalMOS\nmonthly mean difference"))), bg="transparent", bty="n",
         pch=c(15), pt.cex = 2, pt.lwd=3, col="black")
 }
addfiglab("(c)")
  


  ### Fig SX Temperature residuals #####
  #par(mar=c(4,4.5,0.5,0.5))
  {
    lsf <- lm(daily.paws$Air.Temp[1:719] ~ daily.palmos$Air.Temp[1:719]) # lm(y ~ x)
    summary(lsf)
    
    residuals <- residuals(lsf)
    #plot(daily.palmos$Air.Temp[1:719], residuals, main = "Residual Plot", xlab = "Predictor Variable", ylab = "Residuals") 
    plot(fitted(lsf), residuals, cex.axis=1.5, cex.lab=1.5, cex=1.5,
         xlab = expression(bold("Fitted PalMOS air temperature (ºC)")), ylab = expression(bold("Residuals")))
    abline(h = 0, col="red", lwd=2) 
  }
  addfiglab("(a)")
  
  
  ## Relative humidity #####
    daily.paws$day - daily.palmos$day # should all be zero
    
    # May 07, 2016 through May 20, 2016 n= 12 days that need to be removed
    # could be through may 19, 2016 in palmos, need to check out RAW data
    rh.daily.palmos <- daily.palmos[1:719,]
    rh.daily.paws <- daily.paws[1:719,]
    
    rh.daily.palmos <- rh.daily.palmos[-c(235:248),]
    rh.daily.paws <- rh.daily.paws[-c(235:248),] #removes May 7 through May 20 2016 (which are missing in palmos)
    # n= 705 # number of days in overlapping dataset for rel humidity

    # Used to identify extended consecutive days where the daily mean humidity is zero or 100%    
        # hundo <- rh.daily.palmos[rh.daily.palmos$Rel.Humidity==100,]
        # hundo <- rh.daily.paws[rh.daily.paws$Rel.Humidity==100,]
        # zero.rh <- rh.daily.palmos[rh.daily.palmos$Rel.Humidity==0,]
        # zero.rh <- rh.daily.paws[rh.daily.paws$Rel.Humidity==0,]

### stats for Table S1 #####
    cor.test(rh.daily.palmos$Rel.Humidity, rh.daily.paws$Rel.Humidity, method="spearman") # this ends Sept 7, 2017
    wilcox.test(rh.daily.palmos$Rel.Humidity, rh.daily.paws$Rel.Humidity,paired=TRUE)
       
    ### Fig. 3a daily rh 1:1 plot #####
    par(mfrow=c(2,3))
    par(mar=c(4,6,0.5,0.5))
{
      plot(rh.daily.palmos$Rel.Humidity, rh.daily.paws$Rel.Humidity, 
         xlab=expression(bold("PalMOS rel. humidity (%)")), ylab=expression(bold("PAWS rel. humidity (%)")),     
         #main="Relative humidity", 
         xlim=c(48,100), ylim=c(48,100),
         cex.lab=1.5, cex.axis=1.5, cex=1.5) 
    abline(v=c(50,60,70,80,90,100), h=c(50,60,70,80,90,100), lty = 9, col = "grey33", lwd = 0.75)
    abline(a=0,b=1, lty = 9, col="grey33")
    text(61,90,expression(bold("PalMOS-PAWS\nSpearman's ρ = 0.794\nn = 705\nCF = 0.855")))
    #text(53,88,expression(bold("CF = 0.855")))
    lsf <- lm(rh.daily.paws$Rel.Humidity ~ rh.daily.palmos$Rel.Humidity) # lm(y ~ x)
    summary(lsf)
    # rmse
    rmse <- sqrt(mean(lsf$residuals^2))
    round(rmse,1)
    
    }  
    addfiglab("(a)") 
    ### Fig 3b monthly rh PalMOS-PAWs overlap #####

{
      mo_rh.daily.palmos <- rh.daily.palmos %>%
      group_by(month) %>%
      summarize(Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE))
    
    mo_rh.daily.paws <- rh.daily.paws %>%
      group_by(month) %>%
      summarize(Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE))
    
    plot(mo_rh.daily.palmos$month, mo_rh.daily.palmos$Rel.Humidity,pch=0, lwd=3,
         ylim=c(68,100), col="#ca0020",
         cex.lab=1.5, cex.axis=1.5, cex=1.5,
         xlab=expression(bold("month")),
         ylab=expression(bold("rel. humidity (%)")))
    points(mo_rh.daily.paws$month, mo_rh.daily.paws$Rel.Humidity, col="#0571b0", lwd=3,cex=1.5)
    legend("topleft", legend=c(expression(bold("PalMOS")), expression(bold("PAWS"))), bg="transparent", bty="n",
           pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
    
    } 
    addfiglab("(b)")

### Fig 3c mean monthly differencs #####
{
   diff_rh <- mo_rh.daily.paws$Rel.Humidity - mo_rh.daily.palmos$Rel.Humidity

  plot(diff_rh, pch=15, lwd=3,
       ylim=c(-12,0), col="black",
       cex.lab=1.5, cex.axis=1.5, cex=2.5,
       xlab=expression(bold("month")),
       ylab=expression(bold("rel. humidity (%)")))
  abline(h=-5.811006,col="darkgrey",lty=3,lwd=4)
  
  legend(1,-0.5, legend=c(expression(bold("PAWS minus PalMOS\nmonthly mean difference"))), 
         bg="transparent", bty="n",
         pch=15, pt.cex = 2, col="black")
  }

addfiglab("(c)")

mean(diff_rh)


### Fig SX RH residuals #####
{
  lsf <- lm(rh.daily.paws$Rel.Humidity ~ rh.daily.palmos$Rel.Humidity) # lm(y ~ x)
  summary(lsf)
  residuals <- residuals(lsf)
  #plot(rh.daily.palmos$Rel.Humidity, residuals, main = "Residual Plot", xlab = "Predictor Variable", ylab = "Residuals") 
  
  plot(fitted(lsf), residuals, cex.lab=1.5, cex.axis=1.5, cex=1.5,
       xlab = expression(bold("Fitted PalMOS relative humidity (%)")), ylab = expression(bold("Residuals")))
  abline(h = 0, col="red", lwd=2)
}
addfiglab("(b)")


## Wind speed #####
  ### remove zeros in palmos and paws overlapping wind speed #####      
  {
    # removes zeros in paws:   
    #.    = 9 days             begins on 7th at 10:28am, 2016-05-08, 9,10,14,15,17,18,19,27
    #.    = 3 days             2016-07-11, 29,30
    #.    = 1 day               2017-05-12
    paws_wind <- paws
    paws_wind$ymd <- ymd(format(paws_wind$date, "%y%m%d"))
    # paws_wind$ymd <- paws_wind$dates
    # daily zeros - code to check exactly which days need to be removed
    # paws_zeros <- daily.paws[daily.paws$WS_avg_2min == 0,]
    # check out raw data zeros
    # raw_paws_zeros <- paws[paws$WS.Avg.2min == 0,]
    # raw_paws_zeros <- raw_paws_zeros[!is.na(raw_paws_zeros$day),]
    # check out individual days with zeros
    # apes<- raw_paws_zeros[raw_paws_zeros$ymd == "2016-05-07",]
    
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-07",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-08",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-09",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-10",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-12",] #5
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-13",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-14",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-15",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-16",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-17",] #10
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-18",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-19",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-20",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-26",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-27",] #15
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-28",]
    
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-10",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-11",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-12",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-13",] #20
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-15",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-29",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-30",]
    
    paws_wind <- paws_wind[paws_wind$ymd != "2017-05-11",]
    paws_wind <- paws_wind[paws_wind$ymd != "2017-05-12",] #25
    paws_wind <- paws_wind[paws_wind$ymd != "2017-05-13",] #26
    
    # removed because windspeed was zero in palmos
    paws_wind <- paws_wind[paws_wind$ymd != "2016-05-11",]
    paws_wind <- paws_wind[paws_wind$ymd != "2016-07-28",]
    
  
    
    
# removes zeros in palmos: 
    #.    = 9 days             2016-05-11, 12, 13,14,15,16,17,18,19
    #.              begins at 2016-05-10 @ 16H 7M 58S
    #.    = 2 days             2016-07-29, 30
    
    palmos_zeros <- daily.palmos[daily.palmos$WS_avg_2min == 0,]
    
    raw_palmos_zeros <- comb.palmos[comb.palmos$WS_avg_2min == 0,]
    raw_palmos_zeros <- raw_palmos_zeros[!is.na(raw_palmos_zeros$day),]
    
    
    #apes<- raw_palmos_zeros[raw_palmos_zeros$dates == "2016-07-30",]
    palmos_wind <- comb.palmos
    
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-10",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-11",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-12",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-13",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-14",] #5
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-15",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-16",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-17",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-18",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-19",]#10
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-20",]
    
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-28",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-29",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-30",]
    
    # removals because they were removed in the paws overlap
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-07",]#15
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-08",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-09",]
    
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-26",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-27",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-05-28",]#20
    
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-10",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-11",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-12",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-13",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2016-07-15",]#25
    
    palmos_wind <- palmos_wind[palmos_wind$date != "2017-05-11",]
    palmos_wind <- palmos_wind[palmos_wind$date != "2017-05-12",] 
    palmos_wind <- palmos_wind[palmos_wind$date != "2017-05-13",] #26
    
  } 

  daily.palmos.wind <- palmos_wind %>%
    group_by(year,month,day) %>%
    summarise(WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE))
  
  daily.palmos.wind <- daily.palmos.wind[c(4998:5714),]
  
  daily.paws.wind <-  paws_wind %>%
    group_by(year,month,day) %>%
    summarise(WS_avg_2min = mean(WS.Avg.2min, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE))
          
  daily.paws.wind <- daily.paws.wind[c(1:722),] # trim the PAWS data to sept 30, 2017
  
  # remove rows/days in paws that are missing in the palmos dataset
  daily.paws.wind <- daily.paws.wind[-c(52:54,126,520,521),]#, 126,174,545,546),] # palmos is missing Nov 1, 2, 3, Jan 14; rm MAR 2nd too
  daily.palmos.wind <- daily.palmos.wind[-c(454),] #170,479,746),] # removes march 2, 2016 which is missing in paws, so need to remove from palmos
  # 05 JAN 2017 is missing in PAWS; rm from PalMOS
  # 9-10 Mar 2017 is missing in PalMOS; rm from PAWS
  
  # check that all days match between the datasets
  daily.paws.wind$day - daily.palmos.wind$day # should all be zero
 
  # makes a new date column with only yyyymmdd, removes time
  daily.paws.wind <- daily.paws.wind %>%
    mutate(date = make_date(year, month, day))
  
  daily.palmos.wind <- daily.palmos.wind %>%
    mutate(date = make_date(year, month, day))
  
#### stats for Table S1 #####
#correlation between paws-palmos wind speed
  cor.test(daily.palmos.wind$WS_avg_2min, daily.paws.wind$WS_avg_2min,method="spearman")
  wilcox.test(daily.palmos.wind$WS_avg_2min, daily.paws.wind$WS_avg_2min, paired=T)

#### Fig 4a daily wind speed 1:1 plot #####
  par(mar=c(4,6,0.5,0.5))
{
  plot(daily.palmos.wind$WS_avg_2min, daily.paws.wind$WS_avg_2min, col="transparent",
       xlab=expression(bold("PalMOS wind speed (m/s)")), ylab=expression(bold("PAWS wind speed (m/s)")),     
       #main=expression(bold("daily wind speed")), cex.main=1.5,
       xlim=c(0,22), ylim=c(0,22),
       cex.lab=1.5, cex.axis=1.5
       ) 
  abline(v=c(0,5,10,15,20), h=c(0,5,10,15,20), lty = 9, col = "grey33", lwd = 0.75)
  abline(a=0,b=1, lty = 9, col="grey33")
  text(6,18,expression(bold("PalMOS—PAWS\nSpearman's ρ = 0.991\nn = 716\nCF = 1.136")))
  #text(4,16, expression(bold("CF = 1.136")))
  points(daily.palmos.wind$WS_avg_2min, daily.paws.wind$WS_avg_2min, col="black")
  
  lsf <- lm(daily.paws.wind$WS_avg_2min ~ daily.palmos.wind$WS_avg_2min) # lm(y ~ x)
  summary(lsf)
  # rmse
  rmse <- sqrt(mean(lsf$residuals^2))
  round(rmse,1)
  
  } 
  addfiglab("(a)")

  

  
#### Fig SX wind speed residuals #####
{ 
  lsf <- lm(daily.paws.wind$WS_avg_2min ~ daily.palmos.wind$WS_avg_2min) # lm(y ~ x)
  summary(lsf)
  
  residuals <- residuals(lsf)
  #plot(daily.palmos.wind$WS_avg_2min, residuals, main = "Residual Plot", xlab = "Predictor Variable", ylab = "Residuals") 
  plot(fitted(lsf), residuals, 
       cex.axis=1.5, cex.lab=1.5, cex=1.5,
       xlab = expression(bold("Fitted PalMOS wind speed (m/s)")), ylab = expression(bold("Residuals")))
  abline(h = 0, col="red", lwd=2)
}
  addfiglab("(c)")  
  
  ### wind rose paws raw palmos comparison #####

  # library(openair)
  # head(paws_wind[1074840,])
  # tail(palmos_wind[4754000,])
  # 
  # tail(palmos[4241646:4643126,])
  # palmos_wind <- palmos_wind[4241646:4754106,] # 11 sep 2015 to 30 Sep 2017
  # paws_wind <-  paws_wind[1:1167116,]
  # 
  # windRose(palmos_wind, ws ="WS_avg_2min", wd="WD_avg_2min", 
  #          #ws2=paws$WS.Avg.2min, wd2 = paws$WD.Avg.2min,
  #          hemisphere="southern",paddle=F, breaks = c(0, 5, 10, 15, 20),
  #          main="2-min PalMOS: 11 Sep 2015 - 30 Sep 2017", max.freq = 20)
  # windRose(paws_wind, ws ="WS.Avg.2min", wd="WD.Avg.2min", 
  #          hemisphere="southern",paddle=F, breaks = c(0, 5, 10, 15, 20),
  #          main="1-min PAWS: 11 Sep 2015 - 30 Sep 2017", max.freq = 20)

## Wind direction #####
  library(plotly)
  theme_set(theme_bw()) 
  # https://plotly.com/r/polar-chart/
  # palmos wind by month (raw data)
  palmos_bymonth_mo <- palmos_wind %>%
    group_by(month) %>%
    summarize(rep_ws=mean(WS_avg_2min, na.rm=TRUE),
              u=mean(u, na.rm=TRUE),
              v=mean(v, na.rm=TRUE))

  # https://www.eol.ucar.edu/content/wind-direction-quick-reference
  wind_dir_trig_to = atan2(palmos_bymonth_mo$v, palmos_bymonth_mo$u) 
  wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi #
    
  wind_dir_trig_from_degrees = 270 - wind_dir_trig_to_degrees 
  palmos_bymonth_mo$wd <- wind_dir_trig_from_degrees
 

### Fig 4c the polar scatter plot by month #####
  {  
  fig1 <- plot_ly(palmos_bymonth_mo,
                  type = 'scatterpolar',
                  r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="1"],
                  theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="1"],
                  mode = 'markers',
                  name = "Jan",
                  #color= ~palmos_bymonth_mo$month, # this makes crazy plot axes
                  #colors = colors_map,
                  marker = list(
                    color = "#3B4552", #colors = "#8090c7",
                    opacity = 1.0,
                    symbol = 'square',
                    size = 20, rotation = 90
                  ))
  {
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="2"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="2"],
                              mode = 'markers',
                              name = "Feb",
                              marker = list(
                                color = "#C0C0C0", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
  
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="3"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="3"],
                              mode = 'markers',
                              name = "Mar",
                              marker = list(
                                color = "darkorange", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))

  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="4"],
                            theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="4"],
                            mode = 'markers',
                            name = "Apr",
                            marker = list(
                              color = "#F4BA45", #colors = "#8090c7",
                              opacity = 1.0,
                              symbol = 'square',
                              size = 20, rotation = 90
                              ))
                            
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="5"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="5"],
                              mode = 'markers',
                              name = "May",
                              marker = list(
                                color = "#F1E084", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="6"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="6"],
                              mode = 'markers',
                              name = "Jun",
                              marker = list(
                                color = "#040378", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="7"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="7"],
                              mode = 'markers',
                              name = "Jul",
                              marker = list(
                                color = "#3232BA", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))

  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="8"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="8"],
                              mode = 'markers',
                              name = "Aug",
                              marker = list(
                                color = "#458AC9", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="9"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="9"],
                              mode = 'markers',
                              name = "Sep",
                              marker = list(
                                color = "#72559D", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
  
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="10"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="10"],
                              mode = 'markers',
                              name = "Oct",
                              marker = list(
                                color = "#9368D1", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
 
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="11"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="11"],
                              mode = 'markers',
                              name = "Nov",
                              marker = list(
                                color = "#9592DB", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
 
  fig1 <- fig1 %>% add_trace( r = palmos_bymonth_mo$rep_ws[palmos_bymonth_mo$month=="12"],
                              theta = palmos_bymonth_mo$wd[palmos_bymonth_mo$month=="12"],
                              mode = 'markers',
                              name = "Dec",
                              marker = list(
                                color = "#000000", #colors = "#8090c7",
                                opacity = 1.0,
                                symbol = 'square',
                                size = 20, rotation = 90
                              ))
  
  }
  
    
  # # layout
  # fig1 <- fig1 %>% layout(fig1, 
  #          polar = list(
  #            angularaxis = list(
  #              tickfont = list(
  #                size = 12),
  #              rotation = 90,
  #              direction = 'clockwise'
  #            ),
  #            radialaxis = list(
  #              side = 'counterclockwise',
  #              showline = T,
  #              linewidth = 2,
  #              tickwidth = 2,
  #              gridcolor = '#FB8072“',
  #              gridwidth = 1,
  #              range=c(0,9)
  #            )), showlegend = TRUE)
  # 
  # fig1
 }
  

# paws by month
  paws_bymonth_mo <- paws_wind %>%
    group_by(month) %>%
    summarize(rep_ws=mean(WS.Avg.2min, na.rm=TRUE),
              u=mean(u, na.rm=TRUE),
              v=mean(v, na.rm=TRUE))
  
  
  wind_dir_trig_to = atan2(paws_bymonth_mo$v, paws_bymonth_mo$u) 
  wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi 
  
  wind_dir_trig_from_degrees = 270 - wind_dir_trig_to_degrees 
  paws_bymonth_mo$wd <- wind_dir_trig_from_degrees
  
  
### Fig 4c PAWS polar scatter plot by month ##### 
  {  
    # fig1 <- plot_ly(paws_bymonth_mo,
    #                 type = 'scatterpolar',
    #                 r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="1"],
    #                 theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="1"],
    #                 mode = 'markers',
    #                 name = "Jan",
    #                 #color= ~paws_bymonth_mo$month, # this makes crazy plot axes
    #                 #colors = colors_map,
    #                 marker = list(
    #                   color = "black", #colors = "#8090c7",
    #                   opacity = 0.45,
    #                   symbol = 'square',
    #                   size = 20, rotation = 90
    #                 ))
    
      
    {
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="1"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="1"],
                                  mode = 'markers',
                                  name = "Jan",
                                  marker = list(
                                    color = "#3B4552", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ), showlegend=T)
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="2"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="2"],
                                  mode = 'markers',
                                  name = "Feb",
                                  marker = list(
                                    color = "#C0C0C0", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ), showlegend=T)
      
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="3"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="3"],
                                  mode = 'markers',
                                  name = "Mar",
                                  marker = list(
                                    color = "darkorange", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="4"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="4"],
                                  mode = 'markers',
                                  name = "Apr",
                                  marker = list(
                                    color = "#F4BA45", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="5"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="5"],
                                  mode = 'markers',
                                  name = "May",
                                  marker = list(
                                    color = "#F1E084", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="6"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="6"],
                                  mode = 'markers',
                                  name = "Jun",
                                  marker = list(
                                    color = "#040378", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
        
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="7"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="7"],
                                  mode = 'markers',
                                  name = "Jul",
                                  marker = list(
                                    color = "#3232BA", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="8"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="8"],
                                  mode = 'markers',
                                  name = "Aug",
                                  marker = list(
                                    color = "#458AC9", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="9"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="9"],
                                  mode = 'markers',
                                  name = "Sep",
                                  marker = list(
                                    color = "#72559D", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
        
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="10"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="10"],
                                  mode = 'markers',
                                  name = "Oct",
                                  marker = list(
                                    color = "#9368D1", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="11"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="11"],
                                  mode = 'markers',
                                  name = "Nov",
                                  marker = list(
                                    color = "#9592DB", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
      
      fig1 <- fig1 %>% add_trace( r = paws_bymonth_mo$rep_ws[paws_bymonth_mo$month=="12"],
                                  theta = paws_bymonth_mo$wd[paws_bymonth_mo$month=="12"],
                                  mode = 'markers',
                                  name = "Dec",
                                  marker = list(
                                    color = "#000000", #colors = "#8090c7",
                                    opacity = 1.0,
                                    symbol = 'circle',
                                    size = 20, rotation = 90
                                  ))
      
    }
    
    # layout
    fig1 <- fig1 %>% layout(fig1, 
                            polar = list(
                              angularaxis = list(
                                tickfont = list(
                                  size = 18, 
                                  color = "black"),
                                rotation = 90,
                                direction = 'clockwise'
                              ),
                              radialaxis = list(
                                tickfont = list(
                                  size = 18, 
                                  color = "black"),
                                side = 'counterclockwise',
                                showline = T,
                                linewidth = 2,
                                tickwidth = 2,
                                gridcolor = '#FB8072“',
                                gridwidth = 1,
                                range=c(0,9)
                              )), showlegend = TRUE)
    
    fig1
  }

### Fig 4b mean difference in wind speed #####
{
diff_ws <- paws_bymonth_mo$rep_ws - palmos_bymonth_mo$rep_ws
  
 
  plot(diff_ws, 
       ylim=c(-1,3.5),
       col=alpha("black",0.9), pch=15, lwd=3,cex=2.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("wind speed (m/s)")))
  legend(1,3.3, legend=c(expression(bold("PAWS minus PalMOS\nmonthly mean difference"))), bg="transparent", bty="n",
         pch=c(15), pt.cex = 2, pt.lwd=3, col="black")
  }
addfiglab("(b)")


#####princax analysis of principal variance #####
#### dataframes for matlab princax function #####
# w <- cbind.data.frame(daily.palmos.wind$u, daily.palmos.wind$v)
#   write.csv(w, "palmos_w.csv")
   
# w <- cbind.data.frame(daily.paws.wind$u, daily.paws.wind$v)
#  write.csv(w, "paws_w.csv")
  

  
  
  
## Precipitation #####  
  # values > 200 mm in one day are deemed erroneous; Turner et al "The dominant role
  # of extreme precipitation in Antarctica..." showed that 87mm is max on West AP
  
  # values>200 are replaced with NA
  # pal_adj[, 9][pal_adj[, 9] >= 200] <- NA

  
  ### remove very high precip values from palmos dataset #####
  {  # dates: 2015-11-25, 2015-11-27, 2015-12-22, 2016-8-8
    # trim down to 08-SEP-2016
    # 
    daily.paws_precip <- daily.paws[1:359,]
    daily.palmos_precip  <- daily.palmos[1:359,]
    daily.palmos_precip$precip.melted
    
    daily.palmos_precip <- subset(daily.palmos_precip, precip.melted <200)
    # same dates need to be removed from the paws precip comparison
    # removed dates in paws that are the four extremely large precip values in palmos
    daily.paws_precip <- daily.paws_precip[-c(73,75,100,328),]
    daily.paws_precip$day - daily.palmos_precip$day
    
  }
  ### stats for Table S1 #####
  # compare palmos and paws precip statistical tests
  cor.test(daily.palmos_precip$precip.melted, daily.paws_precip$precip.melted, method="spearman")
  wilcox.test(daily.palmos_precip$precip.melted, daily.paws_precip$precip.melted, paired=T)
  

  ### calc annual catch ratio of overlapping precip melted ####
  sum(daily.palmos_precip$precip.melted, na.rm=TRUE)
  sum(daily.paws_precip$precip.melted, na.rm=TRUE)
  catch.ratio <- sum(daily.paws_precip$precip.melted, na.rm=TRUE) / sum(daily.palmos_precip$precip.melted, na.rm=TRUE)# 345.8mm / 678.434mm
  catch.ratio # result paws / palmos = 2.046628
  
  ### 12-mo catch ratios ##### 
  {
    par(mfrow=c(3,4))
    # sept 2015 == 1.3 *(only 2015) or     2.4 -> (n=28, 2015 & 2016)
    {
      sep_2015.palmos <- subset(daily.palmos_precip, year=="2015" & month=="9")
      sep_2015.paws <- subset(daily.paws_precip, year=="2015" & month=="9")
      
      cor.test(sep_2015.palmos$precip.melted, sep_2015.paws$precip.melted) #palmos vs paws
      #shapiro.test(sep_2015.palmos$precip.melted) 
      #t.test(sep_2015.palmos$precip.melted, sep_2015.paws$precip.melted) # palmos and paws are not different
      plot(sep_2015.palmos$precip.melted, sep_2015.paws$precip.melted,
           #xlim=c(0,40),ylim=c(0,40)
      ) #palmos vs paws
      plot(sep_2015.paws$precip.melted, type="l")
      lines(sep_2015.palmos$precip.melted, type="l", col="blue")
      sep_2015_catch <-      sum(sep_2015.paws$precip.melted) / 
        (sum(sep_2015.palmos$precip.melted))
    }
    sep_2015_catch
    
    # oct 2015 == 1.4
    {
      oct_2015.palmos <- subset(daily.palmos_precip, year =="2015" & month=="10") 
      oct_2015.paws <- subset(daily.paws_precip, year =="2015" & month=="10")
      
      cor.test(oct_2015.palmos$precip.melted, oct_2015.paws$precip.melted) #palmos vs paws
      #shapiro.test(oct_2015.paws$precip.melted)
      #t.test(oct_2015.palmos$precip.melted, oct_2015.paws$precip.melted) # palmos and paws are not different
      plot(oct_2015.palmos$precip.melted, oct_2015.paws$precip.melted,
           xlim=c(0,30),ylim=c(0,30)) #palmos vs paws
      
      oct_2015_catch <-      (sum(oct_2015.paws$precip.melted)) / 
        sum(oct_2015.palmos$precip.melted)    
    }
    oct_2015_catch
    
    # nov 2015 == 1.4
    {
      # remove Nov 1,2,3 from ERA5 to match the missing palmos/paws datasets n=27, not n=30
      
      nov_2015.palmos <- subset(daily.palmos_precip, year =="2015" & month=="11" & precip.melted<200) 
      nov_2015.paws <- subset(daily.paws_precip, year =="2015" & month=="11" & date!="2015-11-27")
      nov_2015.paws <- subset(nov_2015.paws, date!="2015-11-25")
      
      plot(nov_2015.palmos$precip.melted, nov_2015.paws$precip.melted,
           xlim=c(0,30),ylim=c(0,30)) #palmos vs paws
      cor.test(nov_2015.palmos$precip.melted, nov_2015.paws$precip.melted) #palmos vs paws
      #shapiro.test(nov_2015.paws$precip.melted) 
      #t.test(nov_2015.palmos$precip.melted, nov_2015.paws$precip.melted) # palmos and paws are not different
      
      nov_2015_catch <-      sum(nov_2015.paws$precip.melted) / (sum(nov_2015.palmos$precip.melted))
    }
    nov_2015_catch
    
    # dec 2015 2.0
    {
      dec_2015.palmos <- subset(daily.palmos_precip, year =="2015" & month=="12" & precip.melted<200) 
      dec_2015.paws <- subset(daily.paws_precip, year =="2015" & month=="12"  & date!="2015-12-22")
      
      plot(dec_2015.palmos$precip.melted, dec_2015.paws$precip.melted) #palmos vs paws
      cor.test(dec_2015.palmos$precip.melted, dec_2015.paws$precip.melted) #palmos vs paws
      #t.test(dec_2015.palmos$precip.melted, dec_2015.paws$precip.melted) # palmos and paws are not different
      plot(dec_2015.paws$precip.melted, type="l") #palmos vs paws
      lines(dec_2015.palmos$precip.melted, type="l", col="blue") #palmos vs paws
      
      dec_2015_catch <- sum(dec_2015.paws$precip.melted) / sum(dec_2015.palmos$precip.melted)
    }
    dec_2015_catch
    
    # Jan 2016 == 2.0
    {
      jan_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="1") 
      jan_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="1")
      
      cor.test(jan_2016.palmos$precip.melted, jan_2016.paws$precip.melted) #palmos vs paws
      #t.test(jan_2016.palmos$precip.melted, jan_2016.paws$precip.melted) # palmos and paws are not different
      plot(jan_2016.paws$precip.melted, type="l") #palmos vs paws
      lines(jan_2016.palmos$precip.melted, type="l", col="blue") 
      jan_2016_catch <- sum(jan_2016.paws$precip.melted) / sum(jan_2016.palmos$precip.melted)
      
    }
    jan_2016_catch
    
    # Feb 2016 == 2.4
    {
      feb_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="2") 
      feb_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="2")
      
      cor.test(feb_2016.palmos$precip.melted, feb_2016.paws$precip.melted) #palmos vs paws
      #t.test(feb_2016.palmos$precip.melted, feb_2016.paws$precip.melted) # palmos and paws are not different
      plot(feb_2016.palmos$precip.melted, feb_2016.paws$precip.melted) #palmos vs paws
      plot(feb_2016.paws$precip.melted, type="l") 
      lines(feb_2016.palmos$precip.melted, type="l", col="blue") 
      
      feb_2016_catch <- sum(feb_2016.paws$precip.melted) / sum(feb_2016.palmos$precip.melted)
      
    }
    feb_2016_catch
    
    # Mar 2016 == 2.6
    {
      mar_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="3") 
      mar_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="3")
      
      cor.test(mar_2016.palmos$precip.melted, mar_2016.paws$precip.melted) #palmos vs paws
      #t.test(mar_2016.palmos$precip.melted, mar_2016.paws$precip.melted) # palmos and paws are not different
      plot(mar_2016.palmos$precip.melted, mar_2016.paws$precip.melted) #palmos vs paws
      plot(mar_2016.paws$precip.melted, type="l") 
      lines(mar_2016.palmos$precip.melted, type="l", col="blue") 
      
      mar_2016_catch <- sum(mar_2016.paws$precip.melted) / sum(mar_2016.palmos$precip.melted)
      
    }
    mar_2016_catch
    
    # Apr 2016 == 1.2
    {
      apr_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="4") 
      apr_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="4")
      
      cor.test(apr_2016.palmos$precip.melted, apr_2016.paws$precip.melted) #palmos vs paws
      #t.test(apr_2016.palmos$precip.melted, apr_2016.paws$precip.melted) # palmos and paws are not different
      plot(apr_2016.palmos$precip.melted, apr_2016.paws$precip.melted) #palmos vs paws
      plot(apr_2016.paws$precip.melted, type = "l")
      lines(apr_2016.palmos$precip.melted, type = "l", col="blue")
      
      apr_2016_catch <- sum(apr_2016.paws$precip.melted) / sum(apr_2016.palmos$precip.melted)
      
    }
    apr_2016_catch
    
    # May 2016 == 1.8
    {
      may_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="5") 
      may_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="5")
      
      cor.test(may_2016.palmos$precip.melted, may_2016.paws$precip.melted) #palmos vs paws
      #t.test(may_2016.palmos$precip.melted, may_2016.paws$precip.melted) # palmos and paws are not different
      plot(may_2016.palmos$precip.melted, may_2016.paws$precip.melted) #palmos vs paws
      plot(may_2016.paws$precip.melted, type="l") 
      lines(may_2016.palmos$precip.melted, type="l", col="blue") 
      
      may_2016_catch <- sum(may_2016.paws$precip.melted) / sum(may_2016.palmos$precip.melted)
      
    }
    may_2016_catch
    
    # Jun 2016 == 2.7
    {
      jun_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="6") 
      jun_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="6")
      
      cor.test(jun_2016.palmos$precip.melted, jun_2016.paws$precip.melted) #palmos vs paws
      # t.test(jun_2016.palmos$precip.melted, jun_2016.paws$precip.melted) # palmos and paws are not different
      plot(jun_2016.palmos$precip.melted, jun_2016.paws$precip.melted) #palmos vs paws
      plot(jun_2016.paws$precip.melted, type="l") #palmos vs paws
      lines(jun_2016.palmos$precip.melted, type="l", col="blue") #palmos vs paws
      
      jun_2016_catch <- sum(jun_2016.paws$precip.melted) / sum(jun_2016.palmos$precip.melted)
      
    }
    jun_2016_catch
    
    # Jul 2016 == 4.9
    {
      jul_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="7") 
      jul_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="7")
      
      cor.test(jul_2016.palmos$precip.melted, jul_2016.paws$precip.melted) #palmos vs paws
      #t.test(jul_2016.palmos$precip.melted, jul_2016.paws$precip.melted) # palmos and paws are  different
      plot(jul_2016.palmos$precip.melted, jul_2016.paws$precip.melted) #palmos vs paws
      plot(jul_2016.paws$precip.melted, type="l")
      lines(jul_2016.palmos$precip.melted, type="l", col="blue")
      
      jul_2016_catch <- sum(jul_2016.paws$precip.melted) / sum(jul_2016.palmos$precip.melted)
      
    }
    jul_2016_catch
    
    # Aug 2016 == 3.8
    {
      aug_2016.palmos <- subset(daily.palmos_precip, year =="2016" & month=="8" & precip.melted<200) 
      aug_2016.paws <- subset(daily.paws_precip, year =="2016" & month=="8" & date!="2016-08-08")
      
      cor.test(aug_2016.palmos$precip.melted, aug_2016.paws$precip.melted) #palmos vs paws
      #t.test(aug_2016.palmos$precip.melted, aug_2016.paws$precip.melted) # palmos and paws are not different
      plot(aug_2016.palmos$precip.melted, aug_2016.paws$precip.melted) #palmos vs paws
      plot(aug_2016.paws$precip.melted, type="l") 
      lines(aug_2016.palmos$precip.melted, type="l", col="blue") 
      
      aug_2016_catch <- sum(aug_2016.paws$precip.melted) / sum(aug_2016.palmos$precip.melted)
      
    }
    aug_2016_catch
    
  } 
  
  ### seasonal catch ratios #####
  {
    # JJA
    seas_2015.palmos1 <- subset(daily.palmos_precip,  month=="6")
    seas_2015.palmos2 <- subset(daily.palmos_precip,  month=="7")
    seas_2015.palmos3 <- subset(daily.palmos_precip,  month=="8")
    seas_palmos_cr <- rbind.data.frame(seas_2015.palmos1,seas_2015.palmos2,seas_2015.palmos3)
    
    seas_2015.paws1 <- subset(daily.paws_precip, month=="6")
    seas_2015.paws2 <- subset(daily.paws_precip, month=="7")
    seas_2015.paws3 <- subset(daily.paws_precip, month=="8")
    seas_paws_cr <-  rbind.data.frame(seas_2015.paws1, seas_2015.paws2, seas_2015.paws3)
    
    jja_cr <- sum(seas_paws_cr$precip.melted) / sum(seas_palmos_cr$precip.melted)
    jja_cr # 3.3
    
    
    # SON
    seas_2015.palmos1 <- subset(daily.palmos_precip,  month=="9")
    seas_2015.palmos2 <- subset(daily.palmos_precip,  month=="10")
    seas_2015.palmos3 <- subset(daily.palmos_precip,  month=="11")
    seas_palmos_cr <- rbind.data.frame(seas_2015.palmos1,seas_2015.palmos2,seas_2015.palmos3)
    
    seas_2015.paws1 <- subset(daily.paws_precip, month=="9")
    seas_2015.paws2 <- subset(daily.paws_precip, month=="10")
    seas_2015.paws3 <- subset(daily.paws_precip, month=="11")
    seas_paws_cr <-  rbind.data.frame(seas_2015.paws1, seas_2015.paws2, seas_2015.paws3)
    
    son_cr <- sum(seas_paws_cr$precip.melted) / sum(seas_palmos_cr$precip.melted)
    son_cr # 1.7
    
    
    # DJF
    seas_2015.palmos1 <- subset(daily.palmos_precip,  month=="12")
    seas_2015.palmos2 <- subset(daily.palmos_precip,  month=="1")
    seas_2015.palmos3 <- subset(daily.palmos_precip,  month=="2")
    seas_palmos_cr <- rbind.data.frame(seas_2015.palmos1,seas_2015.palmos2,seas_2015.palmos3)
    
    seas_2015.paws1 <- subset(daily.paws_precip, month=="12")
    seas_2015.paws2 <- subset(daily.paws_precip, month=="1")
    seas_2015.paws3 <- subset(daily.paws_precip, month=="2")
    seas_paws_cr <-  rbind.data.frame(seas_2015.paws1, seas_2015.paws2, seas_2015.paws3)
    
    djf_cr <- sum(seas_paws_cr$precip.melted) / sum(seas_palmos_cr$precip.melted)
    djf_cr # 2.2
    
    
    # MAM
    seas_2015.palmos1 <- subset(daily.palmos_precip,  month=="3")
    seas_2015.palmos2 <- subset(daily.palmos_precip,  month=="4")
    seas_2015.palmos3 <- subset(daily.palmos_precip,  month=="5")
    seas_palmos_cr <- rbind.data.frame(seas_2015.palmos1,seas_2015.palmos2,seas_2015.palmos3)
    
    seas_2015.paws1 <- subset(daily.paws_precip, month=="3")
    seas_2015.paws2 <- subset(daily.paws_precip, month=="4")
    seas_2015.paws3 <- subset(daily.paws_precip, month=="5")
    seas_paws_cr <-  rbind.data.frame(seas_2015.paws1, seas_2015.paws2, seas_2015.paws3)
    
    mam_cr <- sum(seas_paws_cr$precip.melted) / sum(seas_palmos_cr$precip.melted)
    mam_cr # 2.0
    
  }
  
  # JJA = 3.3
  # SON = 1.6
  # DJF = 2.2
  # MAM = 2.0
  
  ### Fig 5a daily precip 1:1 plot #####
  par(mfrow=c(2,3))
  {
    plot(daily.palmos_precip$precip.melted, daily.paws_precip$precip.melted, 
         xlim=c(0,32), ylim=c(0,32),
         xlab=expression(bold("PalMOS precipitation (mm)")), 
         ylab=expression(bold("PAWS precipitation (mm)")), 
         cex.lab=1.5, cex.axis=1.5, cex=1.5
    )
    
    abline(v=c(0,5,10,15,20,25,30), h=c(0,5,10,15,20,25,30), lty = 9, col = "grey33", lwd = 0.75)
    abline(a=0,b=1, lty = 9, col="grey33")
    text(25,2,expression(bold("PalMOS—PAWS\nSpearman's ρ = 0.834\nn = 355")))
    #text(20.3,0,expression(bold("CF = 1.7")))
    lsf <- lm(daily.paws_precip$precip.melted ~ daily.palmos_precip$precip.melted) # lm(y ~ x)
    summary(lsf)
    # rmse
    rmse <- sqrt(mean(lsf$residuals^2))
    round(rmse,1)
    
    
    }
  addfiglab("(a)")
  
  ### Fig 5b Catch ratios annual & monthly  plotted #####
  #par(mfrow=c(2,2))
  
  {
    ann_catch_ratio <- sum(daily.paws_precip$precip.melted, na.rm=T) / sum(daily.palmos_precip$precip.melted, na.rm=T)
    month_ann_catch_ratio <- c(jan_2016_catch,
                               feb_2016_catch,mar_2016_catch,apr_2016_catch,
                               may_2016_catch,jun_2016_catch,jul_2016_catch,
                               aug_2016_catch, sep_2015_catch,oct_2015_catch,
                               nov_2015_catch,dec_2015_catch)  
    
    mean(month_ann_catch_ratio) # 2.0
    x <- (c(1:12))
    all.catch.ratios <- cbind.data.frame(x, month_ann_catch_ratio)
    all.catch.ratios$dates <- ym(all.catch.ratios$x)
    
    plot(x, all.catch.ratios$month_ann_catch_ratio,
         cex.lab=1.5,cex=2,pch=16, cex.axis=1.5,
         col="transparent",
         ylim=c(0,5.9), xlab=expression(bold("month")), 
         ylab=expression(bold("precipitation catch ratio\n(PAWS:PalMOS)")))
    abline(h=ann_catch_ratio,col="darkgrey",lty=3,lwd=4)
    points(x, all.catch.ratios$month_ann_catch_ratio,pch=16,cex=2)
    legend("topleft", legend=c(expression(bold("total precipitation catch ratio"))),
           lty=3, lwd=4, col="darkgrey", bty="n")
    
  }
  addfiglab("(b)")
  

#  Setup Adjusted PalMOS dataset #####
setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/ERA5 adjustment data for Palmer")
pal <- read.csv("table_214.csv",header=TRUE)
head(pal,2)

# pal$Time <- hms(pal$Time)
 pal$dates <- ymd(pal$Date)
# Combine date and time and convert to POSIXct
#pal$datetime <- as.POSIXct(paste(pal$dates, pal$Time), format = "%Y-%m-%d %H:%M:%S")

pal$datetime <- paste(pal$Date, pal$Time)
pal$datetime <- as.POSIXct(pal$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")

pal$year <- format(as.Date(pal$dates, format="Y%/%m/%d"),"%Y")
pal$month <- format(as.Date(pal$dates, format="Y%/%m/%d"),"%m")
pal$day <- format(as.Date(pal$dates, fromat="Y%/%m/%d"),"%d")
pal$year <- as.numeric(pal$year,na.rm=T)
pal$month <- as.numeric(pal$month,na.rm=T)
pal$day <- as.numeric(pal$day,na.rm=T)

#Gap filling 2010 PalMOS Temp/RH ##### 
# the purpose of this step is to replace erroneous (instrument error) values in 2010

### Compare MAWS-PalMOS overlap #####
{
  setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/MAWS Data")
  maws <- read.csv("MAWS 1-minute rh_Temp 2006-2010.csv",header=TRUE)
  
  maws$year <- as.numeric(maws$year,na.rm=T)
  maws$month <- as.numeric(maws$month,na.rm=T)
  maws$day <- as.numeric(maws$day,na.rm=T)
  
  maws$Air_temp <- as.numeric(maws$Air_temp)
  maws$Rel_humidity <- as.numeric(maws$Rel_humidity) 
  maws$dates <- as.Date(maws$dates)
  
  # trim MAWS rhTemp ###
  
  # air temp starts: 01 jan 2007
  # air temp ends: 12 October 2010
  maws_temp <- maws[55435:2176388,] 
  
  # rel hum starts: 06 September 2007
  # rel hum ends: 12 october 2010
  maws_rh <- maws[415388:2176388,]
  
  # convert MAWS 1-minute to daily values ###
  maws_daily_temp <- maws_temp %>%
    group_by(year,month,day) %>%
    summarise(Air.Temp = mean(Air_temp, na.rm=TRUE)) %>%
    mutate(date = make_date(year, month, day))
  
  maws_daily_rh <- maws_rh %>%
    group_by(year,month,day) %>%
    summarise(Rel.Humidity = mean(Rel_humidity, na.rm=TRUE)) %>%
    mutate(date = make_date(year, month, day))
  
  maws_daily_temp <- maws_daily_temp[1:1245,]
  maws_daily_rh <- maws_daily_rh[1:1096,]
  
  
  # find matching PalMOS time frames
  # MAWS air temp starts: 01 jan 2007; # MAWS air temp ends: 31 December 2010 [needs to be trimmed]
  # palmos air temp starts: 01 jan 2007; # palmos air temps ends: 29 may 2010 
  #    [erroneous values begin 30 may 2010]
  pal_temp <- pal[1975143:2862882,]
  
  # MAWS rel hum starts: 06 September 2007; # MAWS rel hum ends: 31 December 2010 [needs to be trimmed]
  # palmos rh is erroneous beginning 06 September 2010
  # palmos rel hum starts: 06 Sept 2007 # palmos needs to end 05 Sept 2010
  pal_rh <- pal[2152866:2934048,]
  
  
  pal_temp_daily <- pal_temp %>%
    group_by(year,month,day) %>%
    summarise(Air.Temp = mean(Air.Temperature..C., na.rm=TRUE)) %>%
    mutate(date = make_date(year, month, day))
  
  pal_rh_daily <- pal_rh %>%
    group_by(year,month,day) %>%
    summarise(Rel.Humidity = mean(Relative.Humidity...., na.rm=TRUE)) %>%
    mutate(date = make_date(year, month, day))
  
  # do rel humidity
  maws_daily_rh$day - pal_rh_daily$day
  # palmos is missihg: 2008-04-24, 2008-06-06, 2008-06-07, 2008-07-14, 
  #                     2008-12-17, 2009-03-02, 2009-03-12, 
  # remove missing from MAWS
  maws_daily_rh <- maws_daily_rh[-c(232,275,276,310,313,469,538,544,554),]
  # MAWS is missing: 11 jul 2008, 2009-02-24, 14 jul 2008
  # remove missing from PalMOS
  pal_rh_daily <- pal_rh_daily[-c(307,533),] #461
  
  
  # next do temperature
  
  maws_daily_temp$day - pal_temp_daily$day
  
  # palmos is missihg:  2008-04-24, 2008-06-06, 2008-06-07, 2008-07-14, 2008-12-17, 2009-03-02
  # remove missing from MAWS
  maws_daily_temp <- maws_daily_temp[-c(480,523,524,561,717,792,802),]
  # MAWS is missing no rows
  
  ## Table XX stats #####
  cor.test(pal_temp_daily$Air.Temp, maws_daily_temp$Air.Temp, method="spearman")
  
  ## Fig S4a 1:1 MAWS-PalMOS overlapping temperature #####
  par(mfrow=c(2,3))
  par(mar=c(4,6,0.5,0.5))
  {
    plot(pal_temp_daily$Air.Temp, maws_daily_temp$Air.Temp,
         xlim=c(-17,8), ylim=c(-17,8), cex.axis=1.5, cex=1.5, cex.lab=1.5,
         xlab=expression(bold("PalMOS air temperature (ºC)")),
         ylab=expression(bold("MAWS air temperature (ºC)"))
    )
    
    abline(v=c(-15,-10,-5,0,5), h=c(-15,-10,-5,0,5), lty = 9, col = "grey33", lwd = 0.75)
    abline(a=0,b=1, lty = 9, col="grey33")
    text(-10,3,expression(bold("MAWS-PalMOS\nSpearman's ρ = 0.990\nn = 1238\nCF = 0.985")))
    #text(-11.7,3.7,expression(bold("CF = 0.985")))
    lsf <- lm(maws_daily_temp$Air.Temp ~ pal_temp_daily$Air.Temp) # lm(y ~ x)
    summary(lsf)
    
  }
  addfiglab("(a)")
  

  ### Fig S4b monthly temperature MAWS-Palmos overlap #####
  {
    mo_temp.daily.pal <- pal_temp_daily %>%
      group_by(month) %>%
      summarize(Air.Temp = mean(Air.Temp, na.rm=TRUE))
    
    mo_temp.daily.maws <- maws_daily_temp %>%
      group_by(month) %>%
      summarize(Air.Temp = mean(Air.Temp, na.rm=TRUE))
    
    plot(mo_temp.daily.maws$month, mo_temp.daily.maws$Air.Temp,pch=0, lwd=3,
         ylim=c(-7,5), col="#ca0020",
         cex.lab=1.5, cex.axis=1.5, cex=1.5,
         xlab=expression(bold("month")),
         ylab=expression(bold("Air temperature (ºC)")))
    points(mo_temp.daily.pal$month, mo_temp.daily.pal$Air.Temp, col="#0571b0", lwd=3,cex=1.5)
    legend("topleft", legend=c(expression(bold("MAWS")), expression(bold("PalMOS"))), bg="transparent", bty="n",
           pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
    
  } 
  addfiglab("(b)")
  
  ### Fig S4c monthly temperature MAWS-Palmos overlap #####
  {
    
    mo_temp.daily.pal <- pal_temp_daily %>%
      group_by(month) %>%
      summarize(Air.Temp = mean(Air.Temp, na.rm=TRUE))
    
    mo_temp.daily.maws <- maws_daily_temp %>%
      group_by(month) %>%
      summarize(Air.Temp = mean(Air.Temp, na.rm=TRUE))
    
diff_maws_t <- mo_temp.daily.pal$Air.Temp - mo_temp.daily.maws$Air.Temp
    
    plot(diff_maws_t, pch=15, lwd=3,
         ylim=c(0,0.425), col="black",
         cex.lab=1.5, cex.axis=1.5, cex=2.5,
         xlab=expression(bold("month")),
         ylab=expression(bold("Air temperature (ºC)")))
    
    legend(1,0.41, legend=c(expression(bold("PalMOS minus MAWS\nmonthly mean difference"))), bg="transparent", bty="n",
           pch=15, pt.cex = 1.5, col="black")
    
  } 
  addfiglab("(c)")
  
  
  
  
  
  
  
  
  #### Fig Sx temperature residuals maws-palmos #####
  {
    lsf <- lm(maws_daily_temp$Air.Temp ~ pal_temp_daily$Air.Temp) # lm(y ~ x)
    summary(lsf)
    residuals <- residuals(lsf)
    
    plot(fitted(lsf), residuals, cex.lab=1.5, cex.axis=1.5, cex=1.5,
         xlab = expression(bold("Fitted MAWS air temperature (ºC)")), ylab = expression(bold("Residuals")))
    abline(h = 0, col="red", lwd=2)
  }
  addfiglab("(x)")
  
  
  # Relative humidity #####
  ## Table xx stats #####
  cor.test(pal_rh_daily$Rel.Humidity, maws_daily_rh$Rel.Humidity, method="spearman")

## Fig S4d 1:1 MAWS-PalMOS overlapping rel humidity #####
  par(mfrow=c(2,3))
  par(mar=c(4,6,0.5,0.5))
  
  {
    plot(pal_rh_daily$Rel.Humidity, maws_daily_rh$Rel.Humidity,
         xlim=c(40,100), ylim=c(40,100), cex.axis=1.5, cex=1.5, cex.lab=1.5,
         xlab=expression(bold("PalMOS relative humidity (%)")),
         ylab=expression(bold("MAWS relative humidity (%)"))
    )
    
    abline(v=c(40,50,60,70,80,90,100), h=c(40,50,60,70,80,90,100), lty = 9, col = "grey33", lwd = 0.75)
    abline(a=0,b=1, lty = 9, col="grey33")
    text(55,89,expression(bold("MAWS-PalMOS\nSpearman's ρ = 0.946\nn = 1087\nCF = 1.0721")))
    #text(48.7,87,expression(bold("CF = 1.0721")))
    lsf <- lm(maws_daily_rh$Rel.Humidity ~ pal_rh_daily$Rel.Humidity) # lm(y ~ x)
    summary(lsf)
    
  }
  addfiglab("(d)")
  
  ## Fig S4e monthly temperature MAWS-Palmos overlap #####
  {
    mo_rh.daily.pal <- pal_rh_daily %>%
      group_by(month) %>%
      summarize(Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE))
    
    mo_rh.daily.maws <- maws_daily_rh %>%
      group_by(month) %>%
      summarize(Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE))
    
    plot(mo_rh.daily.maws$month, mo_rh.daily.maws$Rel.Humidity,pch=0, lwd=3,
         ylim=c(72,90), col="#ca0020",
         cex.lab=1.5, cex.axis=1.5, cex=1.5,
         xlab=expression(bold("month")),
         ylab=expression(bold("Relative humidity (%)")))
    points(mo_rh.daily.pal$month, mo_rh.daily.pal$Rel.Humidity, col="#0571b0", lwd=3,cex=1.5)
    legend("topleft", legend=c(expression(bold("MAWS")), expression(bold("PalMOS"))), bg="transparent", bty="n",
           pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
    
  } 
  addfiglab("(e)")
  
  ## Fig S4f mean monthly difference #####
 { 
  diff_mawsrh <- mo_rh.daily.pal$Rel.Humidity - mo_rh.daily.maws$Rel.Humidity
  plot(diff_mawsrh, pch=15, lwd=3,
       ylim=c(-4,-1), col="black",
       cex.lab=1.5, cex.axis=1.5, cex=2.5,
       xlab=expression(bold("month")),
       ylab=expression(bold("Relative humidity (%)")))
  legend(1,-1.1, legend=c(expression(bold("PalMOS minus MAWS\nmonthly mean difference"))), bg="transparent", bty="n",
         pch=15, pt.cex = 1.5, col="black")
 }
  addfiglab("(f)")
  
  
  ### Fig Sx temperature residuals maws-palmos #####
  {
    lsf <- lm(maws_daily_rh$Rel.Humidity ~ pal_rh_daily$Rel.Humidity) # lm(y ~ x)
    summary(lsf)
    residuals <- residuals(lsf)
    
    plot(fitted(lsf), residuals, cex.lab=1.5, cex.axis=1.5, cex=1.5,
         xlab = expression(bold("Fitted MAWS relative humidity (%)")), ylab = expression(bold("Residuals")))
    abline(h = 0, col="red", lwd=2)
  }
  addfiglab("(x)")
  
  #
  
}




#convert maws time to hms
setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/MAWS Data")
maws <- read.csv("MAWS 1-minute rh_Temp 2006-2010.csv",header=TRUE)
head(maws)
#maws$time <- hms(maws$time)
maws$year <- as.numeric(maws$year,na.rm=T)
maws$month <- as.numeric(maws$month,na.rm=T)
maws$day <- as.numeric(maws$day,na.rm=T)

maws$Air_temp <- as.numeric(maws$Air_temp)
maws$Rel_humidity <- as.numeric(maws$Rel_humidity) 
#maws$dates <- as.Date(maws$dates)

maws$datetime <- paste(maws$dates, maws$time)
maws$datetime <- as.POSIXct(maws$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")

# rel humidity (col=10) in pal from (rows) 06 Sep 2010 to 12 Oct 2010
#pal <- pal[2934049:2960649,]

# 26601 in pal 2-min
# 26871 in maws 2-min raw data to fill in gap

# 1-min MAWS rh     n= 37 days
#palmos rh is erroneous beginning 06 September 2010 ending 12 oct 2010
maws_rh <- maws[2005640:2059382,]
# subsample 1-min MAWS to exact two minutes to be replaced in palmos
maws_rh <- maws_rh %>% filter(row_number() %% 2 != 1) ## Delete odd-rows  # df %>% filter(row_number() %% 3 != 1) ## Delete every 3rd row starting from 1
# this checks how many rows for each date
table(maws_rh$dates)
as.data.frame(table(maws_rh$dates))
## apply Rel humidity CF adjustment on MAWS to fill in missing PalMOS #####
# multiply 1-minute data by the CF
maws_rh$adj_rh <- maws_rh$Rel_humidity * 1.0721 # CF for rel humidity
# aply 2nd CF from palmos-paws adjustment so the data can be inserted easily below
#maws_rh$adj_rh <- maws_rh$adj_rh * 0.855

maws_rh <- na.omit(maws_rh)
### panel plot for relative humidity 12-mo overlap
{
  par(mfrow=c(3,4))
  # sept 2015 == -5.8 sd 0.3
  {
    sep_2015.palmos <- subset(daily.palmos, month=="9")
    sep_2015.palmos <- sep_2015.palmos[1:56,]
    
    sep_2015.paws <- subset(daily.paws,  month=="9")
    sep_2015.paws <- sep_2015.paws[1:56,]
    
    cor.test(sep_2015.palmos$Rel.Humidity, sep_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(sep_2015.palmos$Rel.Humidity, sep_2015.paws$Rel.Humidity, paired=TRUE) # palmos and paws are not different
    #plot(sep_2015.palmos$Rel.Humidity, sep_2015.paws$Rel.Humidity)
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    sep_2015_mean_station_diff <- mean(sep_2015.paws$Rel.Humidity) - mean(sep_2015.palmos$Rel.Humidity)
    round(sep_2015_mean_station_diff, 1) # Palmos is more humid than PAWS (closer to ocean)
    sep_2015_sd_station_diff <- sd(sep_2015.paws$Rel.Humidity) - sd(sep_2015.palmos$Rel.Humidity)
    round(sep_2015_sd_station_diff, 1) 
    
    plot(sep_2015.palmos$Rel.Humidity, type="l", main="Sep-2015,2019",
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    # adjusted sep 2015
    lines(sep_2015.palmos$Rel.Humidity + sep_2015_mean_station_diff, col="red",lty=3,lwd=2)
    lines(sep_2015.paws$Rel.Humidity, col="blue")
    legend("bottomright", legend=c("PalMOS","adj. PalMOS","PAWS"), col=c("red","red","blue"), lty=c(1,3,1), bty="n",cex=0.8)
  }
  sep_2015_mean_station_diff
  
  # oct 2015 == -4.1 sd 0.1
  {
    oct_2015.palmos <- subset(daily.palmos, month=="10") 
    oct_2015.paws <- subset(daily.paws, month=="10")
    
    cor.test(oct_2015.palmos$Rel.Humidity, oct_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(oct_2015.palmos$Rel.Humidity, oct_2015.paws$Rel.Humidity) # palmos and paws are not different
    # plot(oct_2015.palmos$Rel.Humidity, oct_2015.paws$Rel.Humidity)
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    oct_2015_mean_station_diff <- mean(oct_2015.paws$Rel.Humidity) - mean(oct_2015.palmos$Rel.Humidity)
    round(oct_2015_mean_station_diff, 1) 
    oct_2015_sd_station_diff <- sd(oct_2015.paws$Rel.Humidity) - sd(oct_2015.palmos$Rel.Humidity)
    round(oct_2015_sd_station_diff, 1) 
    
    plot(oct_2015.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Oct-2015,2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(oct_2015.palmos$Rel.Humidity+oct_2015_mean_station_diff, col="red",lty=3)
    lines(oct_2015.paws$Rel.Humidity, col="blue")
  }
  oct_2015_mean_station_diff
  
  # nov 2015 == -3.6 sd 1.2
  {
    # remove Nov 1,2,3 from ERA5 to match the missing palmos/paws datasets n=27, not n=30
    #    nov_2015 <- nov_2015[c(4:30),]
    nov_2015.palmos <- subset(daily.palmos, month=="11") 
    nov_2015.paws <- subset(daily.paws, month=="11")
    
    cor.test(nov_2015.palmos$Rel.Humidity, nov_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(nov_2015.palmos$Rel.Humidity, nov_2015.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    nov_2015_mean_station_diff <- mean(nov_2015.paws$Rel.Humidity) - mean(nov_2015.palmos$Rel.Humidity)
    round(nov_2015_mean_station_diff, 1) 
    nov_2015_sd_station_diff <- sd(nov_2015.paws$Rel.Humidity) - sd(nov_2015.palmos$Rel.Humidity) 
    round(nov_2015_sd_station_diff, 1) 
    
    plot(nov_2015.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Nov-2015"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(nov_2015.paws$Rel.Humidity, col="blue")
    lines(nov_2015.palmos$Rel.Humidity+nov_2015_mean_station_diff, col="red",lty=3)
    
  }
  nov_2015_mean_station_diff
  
  # dec 2015 == -3.0 sd 0.2
  {
    dec_2015.palmos <- subset(daily.palmos, month=="12") 
    dec_2015.paws <- subset(daily.paws, month=="12")
    
    cor.test(dec_2015.palmos$Rel.Humidity, dec_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(dec_2015.palmos$Rel.Humidity, dec_2015.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    dec_2015_mean_station_diff <- mean(dec_2015.paws$Rel.Humidity) - mean(dec_2015.palmos$Rel.Humidity)
    round(dec_2015_mean_station_diff, 1) 
    dec_2015_sd_station_diff <- sd(dec_2015.paws$Rel.Humidity) - sd(dec_2015.palmos$Rel.Humidity)
    round(dec_2015_sd_station_diff, 1) 
    
    plot(dec_2015.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Dec-2015, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(dec_2015.paws$Rel.Humidity, col="blue")
    lines(dec_2015.palmos$Rel.Humidity + dec_2015_mean_station_diff, col="red",lty=3)
  }
  dec_2015_mean_station_diff
  
  # Jan 2016 == -4.3 sd -1.3
  {
    jan_2016.palmos <- subset(daily.palmos, month=="1") 
    jan_2016.paws <- subset(daily.paws, month=="1")
    
    cor.test(jan_2016.palmos$Rel.Humidity, jan_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(jan_2016.palmos$Rel.Humidity, jan_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    jan_2016_mean_station_diff <- mean(jan_2016.paws$Rel.Humidity) - mean(jan_2016.palmos$Rel.Humidity)
    round(jan_2016_mean_station_diff, 1) 
    jan_2016_sd_station_diff <- sd(jan_2016.paws$Rel.Humidity) - sd(jan_2016.palmos$Rel.Humidity)
    round(jan_2016_sd_station_diff, 1) 
    
    plot(jan_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Jan-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative Humidity (%)")
    lines(jan_2016.paws$Rel.Humidity, col="blue")
    lines(jan_2016.palmos$Rel.Humidity + jan_2016_mean_station_diff, col="red",lty=3)
    
  }
  jan_2016_mean_station_diff
  
  # Feb 2016 == -5.4 sd 0.1
  {
    feb_2016.palmos <- subset(daily.palmos, month=="2") 
    feb_2016.paws <- subset(daily.paws, month=="2")
    
    cor.test(feb_2016.palmos$Rel.Humidity, feb_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(feb_2016.palmos$Rel.Humidity, feb_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    feb_2016_mean_station_diff <- mean(feb_2016.paws$Rel.Humidity) - mean(feb_2016.palmos$Rel.Humidity)
    round(feb_2016_mean_station_diff, 1) 
    feb_2016_sd_station_diff <- sd(feb_2016.paws$Rel.Humidity) - sd(feb_2016.palmos$Rel.Humidity)
    round(feb_2016_sd_station_diff, 1) 
    
    plot(feb_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Feb-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Temperature (ºC)")
    lines(feb_2016.paws$Rel.Humidity, col="blue")
    lines(feb_2016.palmos$Rel.Humidity + feb_2016_mean_station_diff, col="red",lty=3)
    
  }
  feb_2016_mean_station_diff
  
  # Mar 2016 == -5.9 sd 0.7
  {
    mar_2016.palmos <- subset(daily.palmos,  month=="3") 
    mar_2016.paws <- subset(daily.paws,  month=="3")
    
    cor.test(mar_2016.palmos$Rel.Humidity, mar_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(mar_2016.palmos$Rel.Humidity, mar_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    mar_2016_mean_station_diff <- mean(mar_2016.paws$Rel.Humidity) -  mean(mar_2016.palmos$Rel.Humidity)
    round(mar_2016_mean_station_diff, 1) 
    mar_2016_sd_station_diff <- sd(mar_2016.paws$Rel.Humidity) - sd(mar_2016.palmos$Rel.Humidity)
    round(mar_2016_sd_station_diff, 1) 
    
    plot(mar_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Mar-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(mar_2016.paws$Rel.Humidity, col="blue")
    lines(mar_2016.palmos$Rel.Humidity + mar_2016_mean_station_diff, col="red", lty=3)
  }
  mar_2016_mean_station_diff
  
  # Apr 2016 == -6.5 sd 0.0
  {
    apr_2016.palmos <- subset(daily.palmos, month=="4") 
    apr_2016.paws <- subset(daily.paws, month=="4")
    
    cor.test(apr_2016.palmos$Rel.Humidity, apr_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(apr_2016.palmos$Rel.Humidity, apr_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    apr_2016_mean_station_diff <- mean(apr_2016.paws$Rel.Humidity) - mean(apr_2016.palmos$Rel.Humidity)
    round(apr_2016_mean_station_diff, 1) 
    apr_2016_sd_station_diff <- sd(apr_2016.paws$Rel.Humidity) - sd(apr_2016.palmos$Rel.Humidity)
    round(apr_2016_sd_station_diff, 1) 
    
    plot(apr_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Apr-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Temperature (ºC)")
    lines(apr_2016.paws$Rel.Humidity , col="blue")
    lines(apr_2016.palmos$Rel.Humidity + apr_2016_mean_station_diff, col="red",lty=3)
  }
  apr_2016_mean_station_diff
  
  # May 2016 == -5.6 sd 2.3 # this WILL CHANGE when new 2010 data replaced!!!!!
  {
    may_2016.palmos <- subset(daily.palmos, month=="5") 
    may_2016.paws <- subset(daily.paws, month=="5")
    
    cor.test(may_2016.palmos$Rel.Humidity, may_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(may_2016.palmos$Rel.Humidity, may_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    may_2016_mean_station_diff <- mean(may_2016.paws$Rel.Humidity) - mean(may_2016.palmos$Rel.Humidity)
    round(may_2016_mean_station_diff, 1) 
    may_2016_sd_station_diff <- sd(may_2016.paws$Rel.Humidity) - sd(may_2016.palmos$Rel.Humidity)
    round(may_2016_sd_station_diff, 1) 
    
    plot(may_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("May-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(may_2016.paws$Rel.Humidity, col="blue")
    lines(may_2016.palmos$Rel.Humidity + may_2016_mean_station_diff, col="red",lty=3)
    
  }
  may_2016_mean_station_diff
  
  # Jun 2016 == -7.4 sd 1.6
  {
    jun_2016.palmos <- subset(daily.palmos, month=="6") 
    jun_2016.paws <- subset(daily.paws, month=="6")
    
    cor.test(jun_2016.palmos$Rel.Humidity, jun_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(jun_2016.palmos$Rel.Humidity, jun_2016.paws$Rel.Humidity) # palmos and paws are different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    jun_2016_mean_station_diff <- mean(jun_2016.paws$Rel.Humidity) - mean(jun_2016.palmos$Rel.Humidity)
    round(jun_2016_mean_station_diff, 1) 
    jun_2016_sd_station_diff <- sd(jun_2016.paws$Rel.Humidity) - sd(jun_2016.palmos$Rel.Humidity)
    round(jun_2016_sd_station_diff, 1) 
    
    plot(jun_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Jun-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(jun_2016.paws$Rel.Humidity, col="blue")
    lines(jun_2016.palmos$Rel.Humidity + jun_2016_mean_station_diff, col="red",lty=3)
    
  }
  jun_2016_mean_station_diff
  
  # Jul 2016 == -6.8 sd 1.0
  {
    jul_2016.palmos <- subset(daily.palmos, month=="7") 
    jul_2016.paws <- subset(daily.paws, month=="7")
    
    cor.test(jul_2016.palmos$Rel.Humidity, jul_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(jul_2016.palmos$Rel.Humidity, jul_2016.paws$Rel.Humidity) # palmos and paws are  different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    jul_2016_mean_station_diff <- mean(jul_2016.paws$Rel.Humidity) - mean(jul_2016.palmos$Rel.Humidity)
    round(jul_2016_mean_station_diff, 1) 
    jul_2016_sd_station_diff <- sd(jul_2016.paws$Rel.Humidity) - sd(jul_2016.palmos$Rel.Humidity)
    round(jul_2016_sd_station_diff, 1) 
    
    plot(jul_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Jul-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Temperature (ºC)")
    lines(jul_2016.paws$Rel.Humidity, col="blue")
    lines(jul_2016.palmos$Rel.Humidity + jul_2016_mean_station_diff, col="red",lty=3)
  }
  jul_2016_mean_station_diff
  
  # Aug 2016 == -10.2 sd 1.0
  {
    aug_2016.palmos <- subset(daily.palmos, month=="8") 
    aug_2016.paws <- subset(daily.paws, month=="8")
    
    cor.test(aug_2016.palmos$Rel.Humidity, aug_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(aug_2016.palmos$Rel.Humidity, aug_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    aug_2016_mean_station_diff <- mean(aug_2016.paws$Rel.Humidity) - mean(aug_2016.palmos$Rel.Humidity)
    round(aug_2016_mean_station_diff, 1) # PAWS is colder (higher up on a hill) PALMOS is warmer (closer to ocean)
    aug_2016_sd_station_diff <- sd(aug_2016.paws$Rel.Humidity) - sd(aug_2016.palmos$Rel.Humidity)
    round(aug_2016_sd_station_diff, 1) # PAWS is colder (higher up on a hill) PALMOS is warmer (closer to ocean)
    
    plot(aug_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Aug-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(aug_2016.paws$Rel.Humidity, col="blue")
    lines(aug_2016.palmos$Rel.Humidity + aug_2016_mean_station_diff, col="red",lty=3)    
    
    
  }
  aug_2016_mean_station_diff
  
}

# apply to MAWS raw data
maws_rh$adj_rh[maws_rh$month==12]  <- maws_rh$Rel_humidity[maws_rh$month==12] + dec_2015_mean_station_diff
maws_rh$adj_rh[maws_rh$month==11]  <- maws_rh$Rel_humidity[maws_rh$month==11] + nov_2015_mean_station_diff
maws_rh$adj_rh[maws_rh$month==10] <- maws_rh$Rel_humidity[maws_rh$month==10] + oct_2015_mean_station_diff
maws_rh$adj_rh[maws_rh$month==9] <- maws_rh$Rel_humidity[maws_rh$month==9] + sep_2015_mean_station_diff
maws_rh$adj_rh[maws_rh$month==8] <- maws_rh$Rel_humidity[maws_rh$month==8] + aug_2016_mean_station_diff
maws_rh$adj_rh[maws_rh$month==7] <- maws_rh$Rel_humidity[maws_rh$month==7] + jul_2016_mean_station_diff
maws_rh$adj_rh[maws_rh$month==6] <- maws_rh$Rel_humidity[maws_rh$month==6] + jun_2016_mean_station_diff
maws_rh$adj_rh[maws_rh$month==5] <- maws_rh$Rel_humidity[maws_rh$month==5] + may_2016_mean_station_diff
maws_rh$adj_rh[maws_rh$month==4] <- maws_rh$Rel_humidity[maws_rh$month==4] + apr_2016_mean_station_diff
maws_rh$adj_rh[maws_rh$month==3] <- maws_rh$Rel_humidity[maws_rh$month==3] + mar_2016_mean_station_diff
maws_rh$adj_rh[maws_rh$month==2] <- maws_rh$Rel_humidity[maws_rh$month==2] + feb_2016_mean_station_diff
maws_rh$adj_rh[maws_rh$month==1] <- maws_rh$Rel_humidity[maws_rh$month==1] + jan_2016_mean_station_diff


###################### # ################################# #
# 1-min MAWS temperature 
#palmos temperature is erroneous beginning 30 may 2010 ending 12 oct 2010
maws_temp <- maws[1861729:2059382,]
# subsample 1-min MAWS to exact two minutes to be replaced in palmos
maws_temp <- maws_temp %>% filter(row_number() %% 2 != 1) ## Delete odd-rows  # df %>% filter(row_number() %% 3 != 1) ## Delete every 3rd row starting from 1
# this checks how many rows for each date
table(maws_temp$dates)
as.data.frame(table(maws_temp$dates))

## apply Temperature CF adjustment on MAWS to fill in missing PalMOS #####
# multiply 1-minute data by the CF
maws_temp$adj_temp <- maws_temp$Air_temp * 0.985 # CF for temperature
# apply 2nd CF from palmos-paws adjustment
maws_temp$adj_temp  <- maws_temp$adj_temp * 0.987

# Use which() to get the row numbers where the logical vector is TRUE
logical_vector <- maws$dates=="2010-10-12"
row_numbers <- which(logical_vector)
tail(row_numbers)

# calc dailys from 1-min adjusted temperature and rel humidity MAWS #####
# hourly
# maws_rh$hour <- as.POSIXlt(maws_rh$datetime)$hour
# 
# hourly_adj_rh_maws <- maws_rh %>%
#   group_by(year,month,day,hour) %>%
#   summarise(adj_rh = mean(adj_rh, na.rm=TRUE)) %>%
#   mutate(date = make_datetime(year, month, day, hour))
# 
# maws_temp$hour <- as.POSIXlt(maws_temp$datetime)$hour
# 
# hourly_adj_temp_maws <- maws_temp %>%
#   group_by(year,month,day,hour) %>%
#   summarise(adj_temp = mean(adj_temp, na.rm=TRUE)) %>%
#   mutate(date = make_datetime(year, month, day, hour))

# daily
daily_adj_rh_maws <- maws_rh %>%
  group_by(year,month,day) %>%
  summarise(adj_rh = mean(adj_rh, na.rm=TRUE)) %>%
  mutate(date = make_date(year, month, day))

daily_adj_temp_maws <- maws_temp %>%
  group_by(year,month,day) %>%
  summarise(adj_temp = mean(adj_temp, na.rm=TRUE)) %>%
  mutate(date = make_date(year, month, day))

# result:
# temp gap filling is 137 days
# rh gap filling is 37 days

# Now, use these adjusted datasets to replace daily PalMOS #####
# but first apply palmos CFs to all variables 1-min
# then replace with the maws adjusted datasets once palmos==daily values

# *************************************** #####
#  apply CFs - correction factors #####
pal$adj_ws <- pal$WS.avg.2min..m.s. * 1.136

pal$adj_wd <- pal$WD.avg.2min..º. - 22.7

pal$adj_wd <- ifelse(pal$adj_wd < 0, pal$adj_wd + 360, pal$adj_wd)

pal$adj_t  <- pal$Air.Temperature..C. * 0.987
pal$adj_t[pal$adj_t > 12] <- NA

#pal$adj_pres <- pal$Air.Pressure..mbar. - 2.954838 # this is incorrect to compare surface pressure
pal$Air.Pressure..mbar. <- pal$Air.Pressure..mbar. + (6 / (29.27 * Tv))
#pal$Air.Pressure..mbar. <- pal$Air.Pressure..mbar. + (8 / 9.2) # converts surf pressure to mslp based on elevation
#pal$adj_pres <- pal$Air.Pressure..mbar. * 0.999 # lsf slope as CF for pressure
pal$adj_pres <- pal$Air.Pressure..mbar. + -2.938757 # goal is to add monthly mean difference to palmos to adjust it to be similar to PAWS


pal$adj_pres[pal$adj_pres > 2000] <- NA # remove erroneous pressure values
pal$adj_pres[pal$adj_pres < 800] <- NA
#see <- subset(pal, adj_pres < 800)

#pal$adj_rh <- pal$Relative.Humidity.... + -5.811006
#pal$adj_rh <- pal$Relative.Humidity.... * 0.855
# applied monthly mean rh correction method (mean difference of daily mean rh for each month 2015+2019)

### panel plot for relative humidity 12-mo overlap
{
  par(mfrow=c(3,4))
  # sept 2015 == -5.8 sd 0.3
  {
    sep_2015.palmos <- subset(daily.palmos, month=="9")
    sep_2015.palmos <- sep_2015.palmos[1:56,]
    
    sep_2015.paws <- subset(daily.paws,  month=="9")
    sep_2015.paws <- sep_2015.paws[1:56,]
    
    cor.test(sep_2015.palmos$Rel.Humidity, sep_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(sep_2015.palmos$Rel.Humidity, sep_2015.paws$Rel.Humidity, paired=TRUE) # palmos and paws are not different
    #plot(sep_2015.palmos$Rel.Humidity, sep_2015.paws$Rel.Humidity)
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    sep_2015_mean_station_diff <- mean(sep_2015.paws$Rel.Humidity) - mean(sep_2015.palmos$Rel.Humidity)
    round(sep_2015_mean_station_diff, 1) # Palmos is more humid than PAWS (closer to ocean)
    sep_2015_sd_station_diff <- sd(sep_2015.paws$Rel.Humidity) - sd(sep_2015.palmos$Rel.Humidity)
    round(sep_2015_sd_station_diff, 1) 
    
    plot(sep_2015.palmos$Rel.Humidity, type="l", main="Sep-2015,2019",
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    # adjusted sep 2015
    lines(sep_2015.palmos$Rel.Humidity + sep_2015_mean_station_diff, col="red",lty=3,lwd=2)
    lines(sep_2015.paws$Rel.Humidity, col="blue")
    legend("bottomright", legend=c("PalMOS","adj. PalMOS","PAWS"), col=c("red","red","blue"), lty=c(1,3,1), bty="n",cex=0.8)
  }
  sep_2015_mean_station_diff
  
  # oct 2015 == -4.1 sd 0.1
  {
    oct_2015.palmos <- subset(daily.palmos, month=="10") 
    oct_2015.paws <- subset(daily.paws, month=="10")
    
    cor.test(oct_2015.palmos$Rel.Humidity, oct_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(oct_2015.palmos$Rel.Humidity, oct_2015.paws$Rel.Humidity) # palmos and paws are not different
    # plot(oct_2015.palmos$Rel.Humidity, oct_2015.paws$Rel.Humidity)
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    oct_2015_mean_station_diff <- mean(oct_2015.paws$Rel.Humidity) - mean(oct_2015.palmos$Rel.Humidity)
    round(oct_2015_mean_station_diff, 1) 
    oct_2015_sd_station_diff <- sd(oct_2015.paws$Rel.Humidity) - sd(oct_2015.palmos$Rel.Humidity)
    round(oct_2015_sd_station_diff, 1) 
    
    plot(oct_2015.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Oct-2015,2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(oct_2015.palmos$Rel.Humidity+oct_2015_mean_station_diff, col="red",lty=3)
    lines(oct_2015.paws$Rel.Humidity, col="blue")
  }
  oct_2015_mean_station_diff
  
  # nov 2015 == -3.6 sd 1.2
  {
    # remove Nov 1,2,3 from ERA5 to match the missing palmos/paws datasets n=27, not n=30
    #    nov_2015 <- nov_2015[c(4:30),]
    nov_2015.palmos <- subset(daily.palmos, month=="11") 
    nov_2015.paws <- subset(daily.paws, month=="11")
    
    cor.test(nov_2015.palmos$Rel.Humidity, nov_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(nov_2015.palmos$Rel.Humidity, nov_2015.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    nov_2015_mean_station_diff <- mean(nov_2015.paws$Rel.Humidity) - mean(nov_2015.palmos$Rel.Humidity)
    round(nov_2015_mean_station_diff, 1) 
    nov_2015_sd_station_diff <- sd(nov_2015.paws$Rel.Humidity) - sd(nov_2015.palmos$Rel.Humidity) 
    round(nov_2015_sd_station_diff, 1) 
    
    plot(nov_2015.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Nov-2015"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(nov_2015.paws$Rel.Humidity, col="blue")
    lines(nov_2015.palmos$Rel.Humidity+nov_2015_mean_station_diff, col="red",lty=3)
    
  }
  nov_2015_mean_station_diff
  
  # dec 2015 == -3.0 sd 0.2
  {
    dec_2015.palmos <- subset(daily.palmos, month=="12") 
    dec_2015.paws <- subset(daily.paws, month=="12")
    
    cor.test(dec_2015.palmos$Rel.Humidity, dec_2015.paws$Rel.Humidity) #palmos vs paws
    t.test(dec_2015.palmos$Rel.Humidity, dec_2015.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    dec_2015_mean_station_diff <- mean(dec_2015.paws$Rel.Humidity) - mean(dec_2015.palmos$Rel.Humidity)
    round(dec_2015_mean_station_diff, 1) 
    dec_2015_sd_station_diff <- sd(dec_2015.paws$Rel.Humidity) - sd(dec_2015.palmos$Rel.Humidity)
    round(dec_2015_sd_station_diff, 1) 
    
    plot(dec_2015.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Dec-2015, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(dec_2015.paws$Rel.Humidity, col="blue")
    lines(dec_2015.palmos$Rel.Humidity + dec_2015_mean_station_diff, col="red",lty=3)
  }
  dec_2015_mean_station_diff
  
  # Jan 2016 == -4.3 sd -1.3
  {
    jan_2016.palmos <- subset(daily.palmos, month=="1") 
    jan_2016.paws <- subset(daily.paws, month=="1")
    
    cor.test(jan_2016.palmos$Rel.Humidity, jan_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(jan_2016.palmos$Rel.Humidity, jan_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    jan_2016_mean_station_diff <- mean(jan_2016.paws$Rel.Humidity) - mean(jan_2016.palmos$Rel.Humidity)
    round(jan_2016_mean_station_diff, 1) 
    jan_2016_sd_station_diff <- sd(jan_2016.paws$Rel.Humidity) - sd(jan_2016.palmos$Rel.Humidity)
    round(jan_2016_sd_station_diff, 1) 
    
    plot(jan_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Jan-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative Humidity (%)")
    lines(jan_2016.paws$Rel.Humidity, col="blue")
    lines(jan_2016.palmos$Rel.Humidity + jan_2016_mean_station_diff, col="red",lty=3)
    
  }
  jan_2016_mean_station_diff
  
  # Feb 2016 == -5.4 sd 0.1
  {
    feb_2016.palmos <- subset(daily.palmos, month=="2") 
    feb_2016.paws <- subset(daily.paws, month=="2")
    
    cor.test(feb_2016.palmos$Rel.Humidity, feb_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(feb_2016.palmos$Rel.Humidity, feb_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    feb_2016_mean_station_diff <- mean(feb_2016.paws$Rel.Humidity) - mean(feb_2016.palmos$Rel.Humidity)
    round(feb_2016_mean_station_diff, 1) 
    feb_2016_sd_station_diff <- sd(feb_2016.paws$Rel.Humidity) - sd(feb_2016.palmos$Rel.Humidity)
    round(feb_2016_sd_station_diff, 1) 
    
    plot(feb_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Feb-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Temperature (ºC)")
    lines(feb_2016.paws$Rel.Humidity, col="blue")
    lines(feb_2016.palmos$Rel.Humidity + feb_2016_mean_station_diff, col="red",lty=3)
    
  }
  feb_2016_mean_station_diff
  
  # Mar 2016 == -5.9 sd 0.7
  {
    mar_2016.palmos <- subset(daily.palmos,  month=="3") 
    mar_2016.paws <- subset(daily.paws,  month=="3")
    
    cor.test(mar_2016.palmos$Rel.Humidity, mar_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(mar_2016.palmos$Rel.Humidity, mar_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    mar_2016_mean_station_diff <- mean(mar_2016.paws$Rel.Humidity) -  mean(mar_2016.palmos$Rel.Humidity)
    round(mar_2016_mean_station_diff, 1) 
    mar_2016_sd_station_diff <- sd(mar_2016.paws$Rel.Humidity) - sd(mar_2016.palmos$Rel.Humidity)
    round(mar_2016_sd_station_diff, 1) 
    
    plot(mar_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Mar-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(mar_2016.paws$Rel.Humidity, col="blue")
    lines(mar_2016.palmos$Rel.Humidity + mar_2016_mean_station_diff, col="red", lty=3)
  }
  mar_2016_mean_station_diff
  
  # Apr 2016 == -6.5 sd 0.0
  {
    apr_2016.palmos <- subset(daily.palmos, month=="4") 
    apr_2016.paws <- subset(daily.paws, month=="4")
    
    cor.test(apr_2016.palmos$Rel.Humidity, apr_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(apr_2016.palmos$Rel.Humidity, apr_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    apr_2016_mean_station_diff <- mean(apr_2016.paws$Rel.Humidity) - mean(apr_2016.palmos$Rel.Humidity)
    round(apr_2016_mean_station_diff, 1) 
    apr_2016_sd_station_diff <- sd(apr_2016.paws$Rel.Humidity) - sd(apr_2016.palmos$Rel.Humidity)
    round(apr_2016_sd_station_diff, 1) 
    
    plot(apr_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Apr-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Temperature (ºC)")
    lines(apr_2016.paws$Rel.Humidity , col="blue")
    lines(apr_2016.palmos$Rel.Humidity + apr_2016_mean_station_diff, col="red",lty=3)
  }
  apr_2016_mean_station_diff
  
  # May 2016 == -5.6 sd 2.3 # this WILL CHANGE when new 2010 data replaced!!!!!
  {
    may_2016.palmos <- subset(daily.palmos, month=="5") 
    may_2016.paws <- subset(daily.paws, month=="5")
    
    cor.test(may_2016.palmos$Rel.Humidity, may_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(may_2016.palmos$Rel.Humidity, may_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    may_2016_mean_station_diff <- mean(may_2016.paws$Rel.Humidity) - mean(may_2016.palmos$Rel.Humidity)
    round(may_2016_mean_station_diff, 1) 
    may_2016_sd_station_diff <- sd(may_2016.paws$Rel.Humidity) - sd(may_2016.palmos$Rel.Humidity)
    round(may_2016_sd_station_diff, 1) 
    
    plot(may_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("May-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(may_2016.paws$Rel.Humidity, col="blue")
    lines(may_2016.palmos$Rel.Humidity + may_2016_mean_station_diff, col="red",lty=3)
    
  }
  may_2016_mean_station_diff
  
  # Jun 2016 == -7.4 sd 1.6
  {
    jun_2016.palmos <- subset(daily.palmos, month=="6") 
    jun_2016.paws <- subset(daily.paws, month=="6")
    
    cor.test(jun_2016.palmos$Rel.Humidity, jun_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(jun_2016.palmos$Rel.Humidity, jun_2016.paws$Rel.Humidity) # palmos and paws are different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    jun_2016_mean_station_diff <- mean(jun_2016.paws$Rel.Humidity) - mean(jun_2016.palmos$Rel.Humidity)
    round(jun_2016_mean_station_diff, 1) 
    jun_2016_sd_station_diff <- sd(jun_2016.paws$Rel.Humidity) - sd(jun_2016.palmos$Rel.Humidity)
    round(jun_2016_sd_station_diff, 1) 
    
    plot(jun_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Jun-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(jun_2016.paws$Rel.Humidity, col="blue")
    lines(jun_2016.palmos$Rel.Humidity + jun_2016_mean_station_diff, col="red",lty=3)
    
  }
  jun_2016_mean_station_diff
  
  # Jul 2016 == -6.8 sd 1.0
  {
    jul_2016.palmos <- subset(daily.palmos, month=="7") 
    jul_2016.paws <- subset(daily.paws, month=="7")
    
    cor.test(jul_2016.palmos$Rel.Humidity, jul_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(jul_2016.palmos$Rel.Humidity, jul_2016.paws$Rel.Humidity) # palmos and paws are  different
    
    # calculate mean differences between palmos, and paws for each month in overlapping dataset
    jul_2016_mean_station_diff <- mean(jul_2016.paws$Rel.Humidity) - mean(jul_2016.palmos$Rel.Humidity)
    round(jul_2016_mean_station_diff, 1) 
    jul_2016_sd_station_diff <- sd(jul_2016.paws$Rel.Humidity) - sd(jul_2016.palmos$Rel.Humidity)
    round(jul_2016_sd_station_diff, 1) 
    
    plot(jul_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Jul-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Temperature (ºC)")
    lines(jul_2016.paws$Rel.Humidity, col="blue")
    lines(jul_2016.palmos$Rel.Humidity + jul_2016_mean_station_diff, col="red",lty=3)
  }
  jul_2016_mean_station_diff
  
  # Aug 2016 == -10.2 sd 1.0
  {
    aug_2016.palmos <- subset(daily.palmos, month=="8") 
    aug_2016.paws <- subset(daily.paws, month=="8")
    
    cor.test(aug_2016.palmos$Rel.Humidity, aug_2016.paws$Rel.Humidity) #palmos vs paws
    t.test(aug_2016.palmos$Rel.Humidity, aug_2016.paws$Rel.Humidity) # palmos and paws are not different
    
    # calculate mean differences between era5, palmos, and paws for each month in overlapping dataset
    aug_2016_mean_station_diff <- mean(aug_2016.paws$Rel.Humidity) - mean(aug_2016.palmos$Rel.Humidity)
    round(aug_2016_mean_station_diff, 1) # PAWS is colder (higher up on a hill) PALMOS is warmer (closer to ocean)
    aug_2016_sd_station_diff <- sd(aug_2016.paws$Rel.Humidity) - sd(aug_2016.palmos$Rel.Humidity)
    round(aug_2016_sd_station_diff, 1) # PAWS is colder (higher up on a hill) PALMOS is warmer (closer to ocean)
    
    plot(aug_2016.palmos$Rel.Humidity, type="l", main=expression(bold(paste("Aug-2016, 2019"))),
         ylim=c(50,100), col="red", xlab="", ylab="Relative humidity (%)")
    lines(aug_2016.paws$Rel.Humidity, col="blue")
    lines(aug_2016.palmos$Rel.Humidity + aug_2016_mean_station_diff, col="red",lty=3)    
    
    
  }
  aug_2016_mean_station_diff
  
}

# apply to 2-minute data
pal$adj_rh <- NA

pal$adj_rh[pal$month==12]  <- pal$Relative.Humidity....[pal$month==12] + dec_2015_mean_station_diff
pal$adj_rh[pal$month==11]  <- pal$Relative.Humidity....[pal$month==11] + nov_2015_mean_station_diff
pal$adj_rh[pal$month==10] <- pal$Relative.Humidity....[pal$month==10] + oct_2015_mean_station_diff
pal$adj_rh[pal$month==9] <- pal$Relative.Humidity....[pal$month==9] + sep_2015_mean_station_diff
pal$adj_rh[pal$month==8] <- pal$Relative.Humidity....[pal$month==8] + aug_2016_mean_station_diff
pal$adj_rh[pal$month==7] <- pal$Relative.Humidity....[pal$month==7] + jul_2016_mean_station_diff
pal$adj_rh[pal$month==6] <- pal$Relative.Humidity....[pal$month==6] + jun_2016_mean_station_diff
pal$adj_rh[pal$month==5] <- pal$Relative.Humidity....[pal$month==5] + may_2016_mean_station_diff
pal$adj_rh[pal$month==4] <- pal$Relative.Humidity....[pal$month==4] + apr_2016_mean_station_diff
pal$adj_rh[pal$month==3] <- pal$Relative.Humidity....[pal$month==3] + mar_2016_mean_station_diff
pal$adj_rh[pal$month==2] <- pal$Relative.Humidity....[pal$month==2] + feb_2016_mean_station_diff
pal$adj_rh[pal$month==1] <- pal$Relative.Humidity....[pal$month==1] + jan_2016_mean_station_diff

pal$adj_rh

pal$adj_v <- -pal$adj_ws * cos(pal$adj_wd*pi/180)
pal$adj_u <- -pal$adj_ws * sin(pal$adj_wd*pi/180)

pal$Melted.precipitation..mm.[pal$Melted.precipitation..mm. > 40] <- NA
pal$adj_pm <- pal$Melted.precipitation..mm. * 2.0 # 
# *************************************** #####

#  HOURLY PalMOS data to be used in GRAND COMBINATION section #####
# head(pal) # 2001-11-30 00:00:58
# tail(pal) # 2017-03-31 23:59:58
# 
# #1 create column with hour
# pal$hour <- as.POSIXlt(pal$datetime)$hour
# 
# # hourly
{
# pal_adj_not_for_overlap <- pal %>%
#   group_by(year,month,day,hour) %>%
#   summarise(adj_t = mean(adj_t, na.rm=TRUE), 
#             adj_ws = mean(adj_ws, na.rm=TRUE),
#             adj_u = mean(adj_u, na.rm=TRUE),
#             adj_v = mean(adj_v, na.rm=TRUE),
#             adj_rh = mean(adj_rh),
#             adj_pm = max(adj_pm),
#              adj_pres = mean(adj_pres, na.rm=TRUE)) %>%
#  mutate(date = make_datetime(year, month, day, hour))
# 
# # calculate hourly AMOUNT of precip from total max in palmos
# result_df <- pal_adj_not_for_overlap %>%
#   mutate(day = as_date(day)) %>% # Create a grouping variable for each day
#   group_by(day) %>% # Group the data by the 24-hour period
#   mutate(
#     hourly_precip = adj_pm - lag(adj_pm, default = first(adj_pm))
#   ) #%>%
# #  ungroup() %>% # Remove grouping
# #  select(-day) # Clean up by removing the day column
# 
# # the result
# result_df$hourly_precip[result_df$hourly_precip < 0] <- 0
# 
# pal_adj_not_for_overlap$hourly_precip <- result_df$hourly_precip
# 
# 
# # forces the hourly dataset to begin:2001-12-01 and end 2015-09-30, add paws 01 October 2015
# pal_adj_not_for_overlap <- pal_adj_not_for_overlap[-c(1:24,120063:132993),]
# 
# # palmos temperature is erroneous beginning 30 may 2010 ending 12 oct 2010
# 
# # check days are same
# df1 <- as.data.frame(pal_adj_not_for_overlap[c(73460:76723),])
# df2 <- as.data.frame(hourly_adj_temp_maws[c(1:3253),])
# 
# min_time <- min(min(df1$date), min(df2$date, na.rm=TRUE))
# max_time <- max(max(df1$date), max(df2$date, na.rm=TRUE))
# full_time_series <- data.frame(date = seq.POSIXt(min_time, max_time, by = "hour"))
# 
# # 3. Join the Datasets:
# # Perform a full_join of original datasets with this 
# # full_time_series dataframe. This ensures that all hourly timestamps 
# # are present, and missing values from the original datasets will be as NA.
# merged_data <- full_join(full_time_series, df1, by = "date") %>%
#   full_join(df2, by = "date")
# merged_data <- merged_data[-3265,]
# head(merged_data)
# colnames(merged_data)
# 
# # palmos temperature is erroneous beginning 30 may 2010 ending 12 oct 2010
# pal_adj_not_for_overlap[c(73460:76723),5] = merged_data[c(1:3264),18]
# 
# 
# # palmos rel humidity is erroneous beginning 06 September 2010 ending 12 oct 2010
# pal_adj_not_for_overlap[c(75836:76723),9] = hourly_adj_rh_maws[c(1:888),5]
# # check days are same
# pal_adj_not_for_overlap$day[c(75836:76723)] - hourly_adj_rh_maws$day[c(1:888)]
# tail(pal_adj_not_for_overlap)
# 
}

# daily
#  PalMOS data to be used in GRAND COMBINATION section #####
pal_adj_not_for_overlap <- pal %>%
  group_by(year,month,day) %>%
  summarise(adj_t = mean(adj_t, na.rm=TRUE), 
            adj_ws = mean(adj_ws, na.rm=TRUE),
            adj_u = mean(adj_u, na.rm=TRUE),
            adj_v = mean(adj_v, na.rm=TRUE),
            adj_rh = mean(adj_rh),
            adj_pm = max(adj_pm),
            adj_pres = mean(adj_pres, na.rm=TRUE)) %>%
  mutate(date = make_date(year, month, day))


# forces the daily dataset to begin:2001-12-01 and end 2015-09-30, add paws 01 October 2015
pal_adj_not_for_overlap <- pal_adj_not_for_overlap[-c(1,5018:5559),]

# palmos temperature is erroneous beginning 30 may 2010 ending 12 oct 2010
pal_adj_not_for_overlap[c(3072:3207),4] = daily_adj_temp_maws[c(1:136),4]
# check days are same
pal_adj_not_for_overlap$day[c(3072:3207)] - daily_adj_temp_maws$day[c(1:136)]


# palmos rel humidity is erroneous beginning 06 September 2010 ending 12 oct 2010
pal_adj_not_for_overlap[c(3171:3207),8] = daily_adj_rh_maws[c(1:37),4]
# check days are same
pal_adj_not_for_overlap$day[c(3171:3207)] - daily_adj_rh_maws$day[c(1:37)]

# palmos missing 2010 rel humidity and temperature values replaced with adjusted maws data



### Calc. 4 manual observations #####
# average of 10 minutes, prior to 06UTC, 12UTC, 18UTC, 24UTC

library(stringr)

pal$Time <- hms(pal$Time)

UTC24 <- pal %>%
  filter(str_detect(Time, "23H 5"))
UTC24 <- UTC24 %>%
  filter( is.na(Time) | !str_detect(Time, '23H 5M'))

UTC18 <- pal %>%
  filter(str_detect(Time, "17H 5"))
UTC18 <- UTC18 %>%
  filter( is.na(Time) | !str_detect(Time, '17H 5M'))

UTC12 <- pal %>%
  filter(str_detect(Time, "11H 5"))
UTC12 <- UTC12 %>%
  filter( is.na(Time) | !str_detect(Time, '11H 5M'))

UTC06 <- pal %>%
  filter(str_detect(Time, "5H 5"))
UTC06 <- UTC06 %>%
  filter( is.na(Time) | !str_detect(Time, '5H 5M'))
UTC06 <- UTC06 %>%
  filter( is.na(Time) | !str_detect(Time, '15H'))

# organize / order dataframe by date and time
pal_subset_to_manual <- rbind(UTC06, UTC12, UTC18, UTC24)
pal_subset_to_manual <- pal_subset_to_manual[order(as.Date(pal_subset_to_manual$dates)),]

##################################################################### #
##################################################################### #
##################################################################### #

pal_adj <- pal_subset_to_manual %>%
  group_by(year,month,day) %>%
  summarise(Air.Temp = mean(adj_t, na.rm=TRUE), 
            WS_avg_2min = mean(adj_ws, na.rm=TRUE),
            u = mean(adj_u, na.rm=TRUE),
            v = mean(adj_v, na.rm=TRUE),
            Rel.Humidity = mean(adj_rh, na.rm=TRUE),
            precip.melted = max(adj_pm),
            Pressure = mean(adj_pres, na.rm=TRUE)) %>%
  mutate(date = make_date(year, month, day))

# forces the dataset to end 2015-09-30,      will add paws 01 October 2015
pal_adj <- pal_adj[-c(5017:5559),]
tail(pal_adj)

head(pal_adj)
# ########################################################### #
# ########################################################### #
# this is used to compare with manual observations
# ########################################################### #
daily.pal_overlap <-  pal_adj[c(2:657),] # should end 2003-09-30
tail(daily.pal_overlap)

# ########################################################### #
# ########################################################### #
# ########################################################### #

# 2-yr overlap 4/day-manual and adj PalMOS comparison #####

# file 1: daily avgs 1989-12 Dec 2003
# November 30 2001 is when PalMOS started
# Compare Dec 2001 to Dec 2003 (or Nov 2003 since ends dec 12) manual vs. PalMOS
# PalMOS 
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-pal.214.3
# file: 2-minute PALMOS automatic weather station meteorological measurements 
#       (precipitation, radiation, cloud base, temperature, etc.) from Palmer 
#       Station Antarctica, 2001 - March 2017.

# file: manual obsvs in this file are from 1989-2003 (12 Dec): 
# file name: Daily weather averages for Palmer Station, Antarctica (1989-2024)

# https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-pal&identifier=28&revision=10
# edi portal says the overlap btw manual and palmos was from june to dec 2003. 
# 
setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/ERA5 adjustment data for Palmer/knb-lter-pal.28.10")
man <- read.csv("PalmerStation_Daily_Weather.csv",header=TRUE) # ends april 30 2024; begins april 01 1989
# add the u and v wind components # uses wind peak direction variable; lots of NAs
man$Wind.Average..knots._ms <- man$Wind.Average..knots. * 0.514444
man$Precipitation.Melted..mm. <- as.numeric(man$Precipitation.Melted..mm., na.rm=T)

man$v <- -man$Wind.Average..knots. * cos(man$Wind.Peak.Direction*pi/180)
man$v <- man$v * 0.514444
man$u <- -man$Wind.Average..knots. * sin(man$Wind.Peak.Direction*pi/180)
man$u <- man$u * 0.514444

# trace <- subset(man, Precipitation.Melted..mm. %in% c("T"))

# Gap filling manual Tmean #####
########################################################## #
########################################################## #
## adjust manual obs for Tmin & Tmax only & Tmean is missing #####
{
  man_adj <- man
  
  tail(man_adj) # 2024-04-30
  head(man_adj) # 1989-04-01
  
  man_adj$dates <- ymd(man_adj$Date)
  man_adj$year <- format(as.Date(man_adj$dates, format="Y%/%m/%d"),"%Y")
  man_adj$month <- format(as.Date(man_adj$dates, format="Y%/%m/%d"),"%m")
  man_adj$day <- format(as.Date(man_adj$dates, fromat="Y%/%m/%d"),"%d")
  
  man_adj$year <- as.numeric(man_adj$year,na.rm=T)
  man_adj$month <- as.numeric(man_adj$month,na.rm=T)
  man_adj$day <- as.numeric(man_adj$day,na.rm=T)
  
  
  ### Tmean calculation difference #####
  #subset to 09/1996 to 08/2003
  man_adj$hilow_T <- rowMeans(cbind(man_adj$Temperature.High..C.,man_adj$Temperature.Low..C.), na.rm=TRUE)
  Tdiff <- man_adj[c(2711:5266),]
  
  # mean of max and min = (Tmax+Tmin)/2 == hilow_T
  Tdiff_hilow_T <- Tdiff %>%
    group_by(month) %>%
    summarise(mean_hilow_T=mean(hilow_T, na.rm=TRUE))
  # sd of max and min = (Tmax+Tmin)/2 == hilow_T
  sd_Tdiff_hilow_T <- Tdiff %>%
    group_by(month) %>%
    summarise(sd_hilow_T=sd(hilow_T, na.rm=TRUE))
  # mean of manual 4x daily observations
  Tdiff_Tmean <- Tdiff %>%
    group_by(month) %>%
    summarise(Temperature.Average..C.=mean(Temperature.Average..C., na.rm=TRUE))
  # sd of manual 4x daily observations
  sd_Tdiff_Tmean <- Tdiff %>%
    group_by(month) %>%
    summarise(sd_Temperature.Average..C.=sd(Temperature.Average..C., na.rm=TRUE))
  
  monthly_Tdiff <- as.data.frame(cbind(Tdiff_Tmean$month, Tdiff_Tmean$Temperature.Average..C., sd_Tdiff_Tmean$sd_Temperature.Average..C., Tdiff_hilow_T$mean_hilow_T, sd_Tdiff_hilow_T$sd_hilow_T))
  monthly_Tdiff$monthly_Tdiff <- round((Tdiff_Tmean$Temperature.Average..C.)-(Tdiff_hilow_T$mean_hilow_T),1)
  monthly_Tdiff$sd_monthly_Tdiff <- round((sd_Tdiff_Tmean$sd_Temperature.Average..C.) - (sd_Tdiff_hilow_T$sd_hilow_T),1)
  names(monthly_Tdiff)[1:7] <-c("month", "Temperature.Average..C.","sd_Temperature.Average..C.", "hilow_T", "sd_hilow_T", "monthly_Tdiff", "sd_monthly_Tdiff")
  str(Tdiff)
}

### Table S1 results #####
cor.test(Tdiff$Temperature.Average..C., Tdiff$hilow_T,method="spearman")

### Fig S3a 1:1 Tmin/max Tmean comparison #####
par(mfrow=c(2,3))
par(mar=c(4,6,0.5,0.5)) 
{
  plot(Tdiff$Temperature.Average..C., Tdiff$hilow_T,
       xlim=c(-21,7), ylim=c(-21,7),
       cex=1.5, cex.lab=1.5, cex.axis=1.5, col="transparent",
       xlab=expression(bold("archived daily temperature (ºC)")),
       ylab=expression(bold("calculated daily temperature (ºC)")))
  
  text(-12.5,0, expression(bold("manual observations\n01-Sep-1996 to 31-Aug-2003\nSpearman's ρ = 0.980\nn = 2556\nCF = 1.008")))
  #text(-17.1,1, expression(bold("CF = 1.008")))
  lsf <- lm(Tdiff$hilow_T ~ Tdiff$Temperature.Average..C.)
  summary(lsf)
  
  abline(v=c(-20,-15,-10,-5,0,5), h=c(-20,-15,-10,-5,0,5), lty = 9, col = "grey33", lwd = 0.75)
  abline(a=0,b=1, lty = 9, col="grey33")
  
  points(Tdiff$Temperature.Average..C., Tdiff$hilow_T, cex=1.5)
  lsf <- lm(Tdiff$hilow_T ~ Tdiff$Temperature.Average..C.)
  summary(lsf)
  
}                
addfiglab("(a)")

### Fig S3b monthly_Tdiff #####
{
  plot(monthly_Tdiff$month, monthly_Tdiff$hilow_T, type="p", col=alpha("#ca0020",0.9), lwd=3, pch=0,
       ylim=c(-7,5),
       cex.lab=1.5, cex.axis=1.5, cex=1.5, xlab=expression(bold("month")), ylab=expression(bold("air temperature (ºC)")))
  points(monthly_Tdiff$month, monthly_Tdiff$Temperature.Average..C., type="p", col=alpha("#0571b0",0.9), lwd=3, cex=1.5)
  legend("topleft", legend=c(expression(bold("mean from max and min temperature")), 
                         expression(bold("archived mean"))),
         cex=1, pt.cex = 1.5, pt.lwd=3, bty="n", bg="transparent", pch=c(0,1),col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.9)))
  
}
addfiglab("(b)")  

### Fig S3c monthly mean differnce #####
{
diff_mant <- monthly_Tdiff$hilow_T - monthly_Tdiff$Temperature.Average..C.
plot(diff_mant, lwd=3, pch=15, col="black",
     ylim=c(-0.3,0.7),
     cex.lab=1.5, cex.axis=1.5, cex=2.5, xlab=expression(bold("month")), ylab=expression(bold("air temperature (ºC)")))
legend(1,0.65, legend=c(expression(bold("calculated mean minus archived mean\nmonthly mean difference"))),
       cex=1, pt.cex = 1.5, bty="n", bg="transparent", pch=15, col="black")
}
addfiglab("(c)")  




### Fig SX residuals Tdiff manual ####
{
  lsf <- lm(Tdiff$hilow_T ~ Tdiff$Temperature.Average..C.) # lm(y ~ x)
  summary(lsf)
  residuals <- residuals(lsf)
  plot(fitted(lsf), residuals, cex.lab=1.5, cex.axis=1.5, cex=1.5,
       xlab = expression(bold("Fitted reported air temperature (%)")), ylab = expression(bold("Residuals")))
  abline(h = 0, col="red", lwd=2)
}
addfiglab("(c)")  




# adjust manual Pressure obs   ##### 
### Pressure mean calculation difference #####
#subset to 09/1996 to 08/2003
{
man_adj$hilow_P <- rowMeans(cbind(man_adj$Pressure.High..mbar.,man_adj$Pressure.Low..mbar.), na.rm=TRUE)
Pdiff <- man_adj[c(2711:5266),]
# remove erroneous low values in manual dataset
Pdiff <- subset(Pdiff, Pressure.Average..mbar. >900)

# mean of max and min = (Pmax+Pmin)/2 == hilow_P
Pdiff_hilow_P <- Pdiff %>%
  group_by(month) %>%
  summarise(mean_hilow_P=mean(hilow_P, na.rm=TRUE))
# sd of max and min = (Pmax+Pmin)/2 == hilow_P
sd_Pdiff_hilow_P <- Pdiff %>%
  group_by(month) %>%
  summarise(sd_hilow_P=sd(hilow_P, na.rm=TRUE))
# mean of manual 4x daily observations
Pdiff_Pmean <- Pdiff %>%
  group_by(month) %>%
  summarise(Pressure.Average..mbar.=mean(Pressure.Average..mbar., na.rm=TRUE))
# sd of manual 4x daily observations
sd_Pdiff_Pmean <- Pdiff %>%
  group_by(month) %>%
  summarise(sd_Pressure.Average..mbar.=sd(Pressure.Average..mbar., na.rm=TRUE))

monthly_Pdiff <- as.data.frame(cbind(Pdiff_Pmean$month, Pdiff_Pmean$Pressure.Average..mbar., sd_Pdiff_Pmean$sd_Pressure.Average..mbar., Pdiff_hilow_P$mean_hilow_P, sd_Pdiff_hilow_P$sd_hilow_P))
monthly_Pdiff$monthly_Pdiff <- round((Pdiff_Pmean$Pressure.Average..mbar.)-(Pdiff_hilow_P$mean_hilow_P),1)
monthly_Pdiff$sd_monthly_Pdiff <- round((sd_Pdiff_Pmean$sd_Pressure.Average..mbar.) - (sd_Pdiff_hilow_P$sd_hilow_P),1)
names(monthly_Pdiff)[1:7] <-c("month", "Pressure.Average..mbar.","sd_Pressure.Average..mbar.", "hilow_P", "sd_hilow_P", "monthly_Pdiff", "sd_monthly_Pdiff")
}


### Table S1 results #####
cor.test(Pdiff$Pressure.Average..mbar., Pdiff$hilow_P,method="spearman")

### Fig S3a 1:1 Pmin/Pmax Pressure mean comparison #####
#par(mfrow=c(2,3))
par(mar=c(4,6,0.5,0.5)) 
{
  plot(Pdiff$Pressure.Average..mbar., Pdiff$hilow_P,
       xlim=c(940,1040), ylim=c(940,1040),
       cex=1.5, cex.lab=1.5, cex.axis=1.5, col="transparent",
       xlab=expression(bold("archived daily pressure (hPa)")),
       ylab=expression(bold("calculated daily pressure (hPa)")))
  
  text(970,1015, expression(bold("manual observations\n01-Sep-1996 to 31-Aug-2003\nSpearman's ρ = 0.991\nn = 2556")))
  #text(970,1015, expression(bold("manual observations\n01-Sep-1996 to 31-Aug-2003\nSpearman's ρ = 0.991\nn = 2556\nCF = 0.971")))
  #text(-17.1,1, expression(bold("CF = 1.008")))
  lsf <- lm(Pdiff$hilow_P ~ Pdiff$Pressure.Average..mbar.)
  summary(lsf)
  
  abline(v=c(930,940,950,960,970,980,990,1000,1010,1020,1030,1040), 
         h=c(930,940,950,960,970,980,990,1000,1010,1020,1030,1040), lty = 9, col = "grey33", lwd = 0.75)
  abline(a=0,b=1, lty = 9, col="grey33")
  
  points(Pdiff$Pressure.Average..mbar., Pdiff$hilow_P, cex=1.5)
  
  lsf <- lm(Pdiff$hilow_P ~ Pdiff$Pressure.Average..mbar.)
  summary(lsf)
  # rmse
  rmse <- sqrt(mean(lsf$residuals^2))
  round(rmse,1)
  
  
}                
addfiglab("(d)")

### Fig S3b monthly_Pdiff #####
{
  plot(monthly_Pdiff$month, monthly_Pdiff$hilow_P, type="p", col=alpha("#ca0020",0.9), lwd=3, pch=0,
       ylim=c(980,1000),
       cex.lab=1.5, cex.axis=1.5, cex=1.5, xlab=expression(bold("month")), ylab=expression(bold("air pressure (hPa)")))
  points(monthly_Pdiff$month, monthly_Pdiff$Pressure.Average..mbar., type="p", col=alpha("#0571b0",0.9), lwd=3, cex=1.5)
  legend("topleft", legend=c(expression(bold("mean from max and min pressure")), 
                             expression(bold("archived mean"))),
         cex=1, pt.cex = 1.5, pt.lwd=3, bty="n", bg="transparent", pch=c(0,1),col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.9)))
  
}
addfiglab("(e)")  

### Fig S3c monthly mean difference #####
{
  diff_manp1 <- mean(monthly_Pdiff$Pressure.Average..mbar.) - mean(monthly_Pdiff$hilow_P)
  diff_manp1
  
  diff_manp <- (monthly_Pdiff$Pressure.Average..mbar.) - (monthly_Pdiff$hilow_P)
  plot(diff_manp, lwd=3, pch=15, col="black",
       ylim=c(-0.3,0.3),
       cex.lab=1.5, cex.axis=1.5, cex=2.5, xlab=expression(bold("month")), ylab=expression(bold("air pressure (hPa)")))
  legend(1,0.26, legend=c(expression(bold("calculated mean minus archived mean\nmonthly mean difference"))),
         cex=1, pt.cex = 1.5, bty="n", bg="transparent", pch=15, col="black")
}
addfiglab("(f)")  




## Trim down to overlapping period & format #####
man_overlap <- man[c(4628:5296),]

man_overlap$dates <- ymd(man_overlap$Date)
man_overlap$year <- format(as.Date(man_overlap$dates, format="Y%/%m/%d"),"%Y")
man_overlap$month <- format(as.Date(man_overlap$dates, format="Y%/%m/%d"),"%m")
man_overlap$day <- format(as.Date(man_overlap$dates, fromat="Y%/%m/%d"),"%d")

man_overlap$year <- as.numeric(man_overlap$year,na.rm=T)
man_overlap$month <- as.numeric(man_overlap$month,na.rm=T)
man_overlap$day <- as.numeric(man_overlap$day,na.rm=T)

# makes a new date column with only yyyymmdd, removes time
daily.man_overlap <- man_overlap %>%
  mutate(date = make_date(year, month, day))

# missing Jan 26,27, 28, 29, 30, 31 in pal_overlap
# missing Feb 1,2,3 in pal_overlap
# missing July 12, 13, 16, 17
## remove missing dates from manual observation dataset#####
daily.man_overlap <- daily.man_overlap[-c(57:65, 224,225,228,229),]
                  # missing bc of erroneous precip values
                  # 2002-02-18, 
                  # daily.man_overlap <- daily.man_overlap[-c(72),]
                  # # remove last row in daily.pal (different dataframe)
                  # daily.pal_overlap <- daily.pal_overlap[-c(749),]

# Test to check the datasets align
daily.pal_overlap$day - daily.man_overlap$day




# Pressure #####
### stats for Table SX #####
# convert to mean sea level pressure mslp
# palmos was converted to mslpon Line 2091
daily.man_overlap$Pressure.Average..mbar. <- daily.man_overlap$Pressure.Average..mbar. + (8 / (29.27 * Tv))
#daily.man_overlap$Pressure.Average..mbar. <- daily.man_overlap$Pressure.Average..mbar. + (8 / 9.2) #convert to mslp

cor.test(daily.man_overlap$Pressure.Average..mbar., daily.pal_overlap$Pressure, method="spearman")
wilcox.test(daily.man_overlap$Pressure.Average..mbar., daily.pal_overlap$Pressure, paired=TRUE)

par(mfrow=c(2,3))
par(mar=c(4,6,0.5,0.5))
### Fig 2c 1:1 pressure manual-adj Palmos #####
{
  plot(daily.man_overlap$Pressure.Average..mbar., daily.pal_overlap$Pressure, 
       xlab=expression(bold("manual SLP (hPa)")),
       ylab=expression(bold("adjusted PalMOS\nSLP (hPa)")),
       xlim=c(950,1025), ylim=c(950,1025), 
       cex.lab=1.5, cex.axis=1.5, cex=1.5,
       col="transparent")
  
  abline(v=c(930,940,950,960,970,980,990,1000,1010,1020,1030,1040), 
         h=c(930,940,950,960,970,980,990,1000,1010,1020,1030,1040), lty = 9, col = "grey33", lwd = 0.75)
  abline(a=0,b=1, lty = 9, col="grey33")
  text(975,1015,expression(bold("manual—adjusted PalMOS\nSpearman's ρ = 0.996\nn = 656")))
  #text(975,1010,expression(bold("manual—adjusted PalMOS\nSpearman's ρ = 0.995\nn = 649\nCF = 0.996")))
  points(daily.man_overlap$Pressure.Average..mbar., daily.pal_overlap$Pressure, col="black")
  
  lsf <- lm(daily.pal_overlap$Pressure ~ daily.man_overlap$Pressure.Average..mbar.) # lm(y ~ x)
  summary(lsf)
  # rmse
  rmse <- sqrt(mean(lsf$residuals^2))
  round(rmse,1)
  
}
addfiglab("(d)")

### Fig 2d monthly pressure manual—adj. PalMOS
par(mar=c(4,6,0.5,0.5))
{ 
  mo_manual <- daily.man_overlap %>%
    group_by(month) %>%
    summarise(Air.Temp=mean(Temperature.Average..C., na.rm=TRUE),
              WS_avg_2min = mean(Wind.Average..knots._ms, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              precip.melted = sum(Precipitation.Melted..mm., na.rm=TRUE),
              Pressure = mean(Pressure.Average..mbar., na.rm=TRUE))
  
  mo_adj.pal <- daily.pal_overlap %>%
    group_by(month) %>%
    summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
              WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
              precip.melted = sum(precip.melted, na.rm=TRUE),
              Pressure = mean(Pressure, na.rm=TRUE))
  
  plot(mo_manual$month, mo_manual$Pressure, 
       ylim=c(975,1000),
       col=alpha("#ca0020",0.9), pch=0, lwd=3,cex=1.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("SLP (hPa)")),
  ) 
  points(mo_adj.pal$month, mo_adj.pal$Pressure, col=alpha("#0571b0",0.7),pch=1,lwd=3, cex=1.5)
  
  legend("topleft", legend=c(expression(bold("manual")), expression(bold("adjusted PalMOS"))), bg="transparent", bty="n",
         pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
}
addfiglab("(e)")

### Fig 2e mean differences monthly manual and adj palmos #####
{ 
  diff2 <- mo_adj.pal$Pressure - mo_manual$Pressure
  plot(diff2, 
       ylim=c(-2.1,-0.2),
       col="black",pch=15, lwd=3,cex=2.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("SLP (hPa)"))) 
  legend(1,-0.3, legend=c(expression(bold("adjusted PalMOS minus manual\nmonthly mean difference"))), 
         bg="transparent", bty="n",
         pch=15, pt.cex = 1.5,  col="black")
}
addfiglab("(f)")

mean(diff2)

# Temperature #####
### stats for Table S1 #####
cor.test(daily.man_overlap$Temperature.Average..C., daily.pal_overlap$Air.Temp, method="spearman")
wilcox.test(daily.man_overlap$Temperature.Average..C., daily.pal_overlap$Air.Temp, paired=TRUE)

par(mfrow=c(2,3))
par(mar=c(4,6,0.5,0.5))
### Fig 2c 1:1 Temp manual-adj Palmos #####
{
plot(daily.man_overlap$Temperature.Average..C., daily.pal_overlap$Air.Temp, 
     xlab=expression(bold("manual air temperature (ºC)")),
     ylab=expression(bold("adjusted PalMOS\nair temperature (ºC)")),
     xlim=c(-21,7), ylim=c(-21,7), cex.lab=1.5, cex.axis=1.5, cex=1.5,
     col="transparent")
     
abline(v=c(-20,-15,-10,-5,0,5), h=c(-20,-15,-10,-5,0,5), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(-13,1.5,expression(bold("manual—adjusted PalMOS\nSpearman's ρ = 0.995\nn = 656\nCF = 0.993")))
#text(-17.3,1.5, expression(bold("CF = 0.993")))
points(daily.man_overlap$Temperature.Average..C., daily.pal_overlap$Air.Temp, col="black")

lsf <- lm(daily.pal_overlap$Air.Temp ~ daily.man_overlap$Temperature.Average..C.) # lm(y ~ x)
summary(lsf)
# rmse
rmse <- sqrt(mean(lsf$residuals^2))
round(rmse,1)


}
addfiglab("(c)")

### Fig 2d monthly temp manual—adj. PalMOS
par(mar=c(4,6,0.5,0.5))
{ 
  mo_manual <- daily.man_overlap %>%
    group_by(month) %>%
    summarise(Air.Temp=mean(Temperature.Average..C., na.rm=TRUE),
              WS_avg_2min = mean(Wind.Average..knots._ms, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              precip.melted = sum(Precipitation.Melted..mm., na.rm=TRUE))
  
  mo_adj.pal <- daily.pal_overlap %>%
    group_by(month) %>%
    summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
              WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
              precip.melted = sum(precip.melted, na.rm=TRUE))
  
  plot(mo_manual$month, mo_manual$Air.Temp, 
       ylim=c(-7,5),
       col=alpha("#ca0020",0.9), pch=0, lwd=3,cex=1.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("air temperature (ºC)")),
  ) 
  points(mo_adj.pal$month, mo_adj.pal$Air.Temp, col=alpha("#0571b0",0.7),pch=1,lwd=3, cex=1.5)
  
  legend("topleft", legend=c(expression(bold("manual")), expression(bold("adjusted PalMOS"))), bg="transparent", bty="n",
         pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
}
addfiglab("(d)")

### Fig 2e mean differences monthly manual and adj palmos #####
{ 
diff1 <- mo_adj.pal$Air.Temp - mo_manual$Air.Temp
  plot(diff1, 
       ylim=c(-0.1,0.3),
       col="black",pch=15, lwd=3,cex=2.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("air temperature (ºC)")),
  ) 
  
  
  legend(1,0.289, legend=c(expression(bold("adjusted PalMOS minus manual\nmonthly mean difference"))), 
                          bg="transparent", bty="n",
         pch=15, pt.cex = 1.5,  col="black")
}
addfiglab("(e)")



### Fig SX residuals of manual-adjPalmos air Temperature overlap #####
{
  lsf <- lm(daily.pal_overlap$Air.Temp ~ daily.man_overlap$Temperature.Average..C.) # lm(y ~ x)
  summary(lsf)
  
  residuals <- residuals(lsf)
  #plot(daily.pal_overlap$Air.Temp, residuals, main = "Residual Plot", xlab = "Predictor Variable", ylab = "Residuals") 
  plot(fitted(lsf), residuals, 
       cex=1.5, cex.lab=1.5, cex.axis=1.5,
       xlab = expression(bold("Fitted manual air temperature (ºC)")), ylab = expression(bold("Residuals")))
  abline(h = 0, col="red", lwd=2) 
}
addfiglab("(b)")














#???# apply adjustment CF for Tmin/max and Pmin/Pmax from lsf #####

man_adj$adj_P <- NA
man_adj$adj_P <- man_adj$hilow_P - 0.06144204

man_adj$adj_T <- NA
man_adj$adj_T <- man_adj$hilow_T * 1.008



##################################################################### #
##################################################################### #
## IMPORTANT: these corrections get applied after the Tmin+Tmax/2 correction to Temperature occurs
##              on line 2477 below
##################################################################### #
##################################################################### #
#? remove Temp manual- adjusted Palmos monthly mean CFs #####
###################################################################M 

man$dates <- ymd(man$Date)
man$year <- format(as.Date(man$dates, format="Y%/%m/%d"),"%Y")
man$month <- format(as.Date(man$dates, format="Y%/%m/%d"),"%m")
man$day <- format(as.Date(man$dates, fromat="Y%/%m/%d"),"%d")

man$year <- as.numeric(man$year,na.rm=T)
man$month <- as.numeric(man$month,na.rm=T)
man$day <- as.numeric(man$day,na.rm=T)

colnames(man)
colnames(man_adj)

man$hilow_T <- NA
man$hilow_P <- NA

man$adj_P <- NA
man$adj_T <- NA


### combine manual Pmin/Pmax adjusted dataset with unadjusted manual to correct it and add with PAWS #####
man_adj2 <- rbind.data.frame(man_adj[c(1:2710),], (man[c(2711:4627),]))
tail(man[c(2711:4627),]) #extent of non-adjusted manual dataset

# create a new pressure column for adjusted old Pmin+Pmax method and from 09-1996 to 10-2001
man_adj2$comb_P <- c(man_adj2$hilow_P[c(1:2710)], man_adj2$Pressure.Average..mbar.[c(2711:4627)])
# create a new temperature column for adjusted old Tmin+Tmax method and from 09-1996 to 10-2001
man_adj2$comb_T <- c(man_adj2$hilow_T[c(1:2710)], man_adj2$Temperature.Average..C.[c(2711:4627)])

## apply CF manual Pressure #####
# adjust 04-1989 to 11-2001 mean of hi-low temperature + combine with non-adjusted 
# based on 2-year overlap analysis of adjustment factors
man_adj2$man_adj2_P <- NA
#man_adj2$man_adj2_P <- man_adj2$comb_P + -1.014483
#man_adj2$man_adj2_P <- man_adj2$comb_P + 0.996
man_adj2$comb_P <- man_adj2$comb_P + (8 / (29.27 * Tv)) # converts surf pressure to mslp based on elevation
man_adj2$man_adj2_P <- man_adj2$comb_P + -0.9986525 # adjusts the pressure using the mean monthly difference correction factor
#man_adj2$man_adj2_P <- man_adj2$comb_P * 0.995

# the newly adjusted manual Pressure data series
man_adj2$man_adj2_P <- round(man_adj2$man_adj2_P,1)
man_adj2$man_adj2_P

## apply CF manual Temperature #####
# adjust 04-1989 to 11-2001 mean of hi-low temperature + combine with non-adjusted 
# based on 2-year overlap analysis of adjustment factors
man_adj2$man_adj2_T <- NA
man_adj2$man_adj2_T <- man_adj2$comb_T * 0.993

# the newly adjusted manual Temperature data series
man_adj2$man_adj2_T <- round(man_adj2$man_adj2_T,1)
man_adj2$man_adj2_T



# Wind speed #####
 daily.man_overlap$Wind.Average.m.s <- daily.man_overlap$Wind.Average..knots. * 0.514444

### stats for Table S1 #####
 cor.test(daily.man_overlap$Wind.Average.m.s, daily.pal_overlap$WS_avg_2min, method="spearman")
 

par(mfrow=c(2,2))
par(mar=c(4,6,0.5,0.5)) 

### Fig 4d wind speed manual-adj PalMOS 1:1 plot #####
{
plot(daily.man_overlap$Wind.Average.m.s, daily.pal_overlap$WS_avg_2min,
      xlim=c(0,22), ylim=c(0,22),
     cex.lab=1.5, cex.axis=1.5, cex=1.5,
      xlab=expression(bold("manual wind speed (m/s)")),
      ylab=expression(bold("adjusted PalMOS\nwind speed (m/s)")),
     col="transparent")

abline(v=c(20,15,10,5,0), h=c(20,15,10,5,0), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(6.2,18,expression(bold("manual—adjusted PalMOS\nSpearman's ρ = 0.922\nn = 656\nCF = 1.005")))
points(daily.man_overlap$Wind.Average.m.s, daily.pal_overlap$WS_avg_2min, col="black")

lsf <- lm(daily.pal_overlap$WS_avg_2min ~ daily.man_overlap$Wind.Average.m.s) # lm(y ~ x)
summary(lsf)
# rmse
rmse <- sqrt(mean(lsf$residuals^2))
round(rmse,1)

}
addfiglab("(d)")

### Fig 4f monthly mean wind speed manual-adj. PalMOS #####
{
  diff_manws <- mo_manual$WS_avg_2min - mo_adj.pal$WS_avg_2min
  
  plot(diff_manws,
       ylim=c(-0.8,0.1),
       col="black", pch=15, lwd=3,cex=2.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("wind speed (m/s)")),
  ) 
  
  legend(1,0.05, legend=c(expression(bold("manual minus adjusted PalMOS\nmonthly mean differences"))), bg="transparent", bty="n",
         pch=15, pt.cex = 1.5,  col="black")
}
addfiglab("(e)")




### Fig 4e monthly wind speed manual-adj. PalMOS #####
{ 
  mo_manual <- daily.man_overlap %>%
    group_by(month) %>%
    summarise(Air.Temp=mean(Temperature.Average..C., na.rm=TRUE),
              WS_avg_2min = mean(Wind.Average.m.s, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              precip.melted = sum(Precipitation.Melted..mm., na.rm=TRUE))
  
  mo_adj.pal <- daily.pal_overlap %>%
    group_by(month) %>%
    summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
              WS_avg_2min = mean(WS_avg_2min, na.rm=TRUE),
              u = mean(u, na.rm=TRUE),
              v = mean(v, na.rm=TRUE),
              Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
              precip.melted = sum(precip.melted, na.rm=TRUE))
  
  plot(mo_manual$month, mo_manual$WS_avg_2min, 
       ylim=c(0,10),
       col=alpha("#ca0020",0.9), pch=0, lwd=3,cex=1.5, cex.lab=1.5, cex.axis=1.5,
       xlab=expression(bold("month")), 
       ylab=expression(bold("wind speed (m/s)")),
  ) 
  points(mo_adj.pal$month, mo_adj.pal$WS_avg_2min, col=alpha("#0571b0",0.7),pch=1,lwd=3, cex=1.5)
  
  legend("bottomleft", legend=c(expression(bold("manual")), expression(bold("adjusted PalMOS"))), bg="transparent", bty="n",
         pch=c(0,1), pt.cex = 1.5, pt.lwd=3, col=c(alpha("#ca0020",0.9), alpha("#0571b0",0.7)))
}
addfiglab("(d)")

### Fig SX residuals of manual-adjPalmos air Temperature overlap #####
{
  lsf <- lm(daily.pal_overlap$WS_avg_2min ~ daily.man_overlap$Wind.Average.m.s) # lm(y ~ x)
  summary(lsf)
  
  residuals <- residuals(lsf)
  #plot(daily.pal_overlap$Air.Temp, residuals, main = "Residual Plot", xlab = "Predictor Variable", ylab = "Residuals") 
  plot(fitted(lsf), residuals, 
       cex=1.5, cex.lab=1.5, cex.axis=1.5,
       xlab = expression(bold("Fitted manual wind speed (m/s)")), ylab = expression(bold("Residuals")))
  abline(h = 0, col="red", lwd=2) 
}
addfiglab("(f)")




## apply wind speed CF  #####
# based on 2-year overlap analysis of adjustment factors

man_adj2$man_adj_ws <- NA
man_adj2$man_adj_ws <- man_adj2$Wind.Average..knots._ms * 1.005


### wind rose paws raw palmos comparison #####

# library(openair)
# head(paws_wind[1074840,])
# tail(palmos_wind[4754000,])
# 
# tail(palmos[4241646:4643126,])
# palmos_wind <- palmos_wind[4241646:4754106,] # 11 sep 2015 to 30 Sep 2017
# paws_wind <-  paws_wind[1:1167116,]
# 
# windRose(palmos_wind, ws ="WS_avg_2min", wd="WD_avg_2min", 
#          #ws2=paws$WS.Avg.2min, wd2 = paws$WD.Avg.2min,
#          hemisphere="southern",paddle=F, breaks = c(0, 5, 10, 15, 20),
#          main="2-min PalMOS: 11 Sep 2015 - 30 Sep 2017", max.freq = 20)
# windRose(paws_wind, ws ="WS.Avg.2min", wd="WD.Avg.2min", 
#          hemisphere="southern",paddle=F, breaks = c(0, 5, 10, 15, 20),
#          main="1-min PAWS: 11 Sep 2015 - 30 Sep 2017", max.freq = 20)

# Wind direction Analysis#####
theme_set(theme_bw()) 
# https://plotly.com/r/polar-chart/
# palmos wind by month (raw data)

# https://www.eol.ucar.edu/content/wind-direction-quick-reference
wind_dir_trig_to = atan2(mo_manual$v, mo_manual$u) 
wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi #

wind_dir_trig_from_degrees = 270 - wind_dir_trig_to_degrees 
mo_manual$wd <- wind_dir_trig_from_degrees

mo_manual$rep_ws <- mo_manual$WS_avg_2min
### Fig 3b the polar scatter plot by month #####
{  
  fig1 <- plot_ly(mo_manual,
                  type = 'scatterpolar',
                  r = mo_manual$rep_ws[mo_manual$month=="1"],
                  theta = mo_manual$wd[mo_manual$month=="1"],
                  mode = 'markers',
                  name = "Jan",
                  #color= ~mo_manual$month, # this makes crazy plot axes
                  #colors = colors_map,
                  marker = list(
                    color = "#3B4552", #colors = "#8090c7",
                    opacity = 1.0,
                    symbol = 'square',
                    size = 20, rotation = 90
                  ))
  {
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="2"],
                                theta = mo_manual$wd[mo_manual$month=="2"],
                                mode = 'markers',
                                name = "Feb",
                                marker = list(
                                  color = "#C0C0C0", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="3"],
                                theta = mo_manual$wd[mo_manual$month=="3"],
                                mode = 'markers',
                                name = "Mar",
                                marker = list(
                                  color = "darkorange", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="4"],
                                theta = mo_manual$wd[mo_manual$month=="4"],
                                mode = 'markers',
                                name = "Apr",
                                marker = list(
                                  color = "#F4BA45", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="5"],
                                theta = mo_manual$wd[mo_manual$month=="5"],
                                mode = 'markers',
                                name = "May",
                                marker = list(
                                  color = "#F1E084", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="6"],
                                theta = mo_manual$wd[mo_manual$month=="6"],
                                mode = 'markers',
                                name = "Jun",
                                marker = list(
                                  color = "#040378", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="7"],
                                theta = mo_manual$wd[mo_manual$month=="7"],
                                mode = 'markers',
                                name = "Jul",
                                marker = list(
                                  color = "#3232BA", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="8"],
                                theta = mo_manual$wd[mo_manual$month=="8"],
                                mode = 'markers',
                                name = "Aug",
                                marker = list(
                                  color = "#458AC9", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="9"],
                                theta = mo_manual$wd[mo_manual$month=="9"],
                                mode = 'markers',
                                name = "Sep",
                                marker = list(
                                  color = "#72559D", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="10"],
                                theta = mo_manual$wd[mo_manual$month=="10"],
                                mode = 'markers',
                                name = "Oct",
                                marker = list(
                                  color = "#9368D1", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="11"],
                                theta = mo_manual$wd[mo_manual$month=="11"],
                                mode = 'markers',
                                name = "Nov",
                                marker = list(
                                  color = "#9592DB", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_manual$rep_ws[mo_manual$month=="12"],
                                theta = mo_manual$wd[mo_manual$month=="12"],
                                mode = 'markers',
                                name = "Dec",
                                marker = list(
                                  color = "#000000", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'square',
                                  size = 20, rotation = 90
                                ))
    
  }
  
  
  # # layout
  # fig1 <- fig1 %>% layout(fig1, 
  #          polar = list(
  #            angularaxis = list(
  #              tickfont = list(
  #                size = 12),
  #              rotation = 90,
  #              direction = 'clockwise'
  #            ),
  #            radialaxis = list(
  #              side = 'counterclockwise',
  #              showline = T,
  #              linewidth = 2,
  #              tickwidth = 2,
  #              gridcolor = '#FB8072“',
  #              gridwidth = 1,
  #              range=c(0,9)
  #            )), showlegend = TRUE)
  # 
  # fig1
}


wind_dir_trig_to = atan2(mo_adj.pal$v, mo_adj.pal$u) 
wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi 

wind_dir_trig_from_degrees = 270 - wind_dir_trig_to_degrees 
mo_adj.pal$wd <- wind_dir_trig_from_degrees
mo_adj.pal$rep_ws <- mo_adj.pal$WS_avg_2min

# Fig 3b PAWS polar scatter plot by month 
{
  {
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="1"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="1"],
                                mode = 'markers',
                                name = "Jan",
                                marker = list(
                                  color = "#3B4552", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ), showlegend=T)
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="2"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="2"],
                                mode = 'markers',
                                name = "Feb",
                                marker = list(
                                  color = "#C0C0C0", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ), showlegend=T)
    
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="3"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="3"],
                                mode = 'markers',
                                name = "Mar",
                                marker = list(
                                  color = "darkorange", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="4"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="4"],
                                mode = 'markers',
                                name = "Apr",
                                marker = list(
                                  color = "#F4BA45", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="5"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="5"],
                                mode = 'markers',
                                name = "May",
                                marker = list(
                                  color = "#F1E084", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="6"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="6"],
                                mode = 'markers',
                                name = "Jun",
                                marker = list(
                                  color = "#040378", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="7"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="7"],
                                mode = 'markers',
                                name = "Jul",
                                marker = list(
                                  color = "#3232BA", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="8"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="8"],
                                mode = 'markers',
                                name = "Aug",
                                marker = list(
                                  color = "#458AC9", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="9"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="9"],
                                mode = 'markers',
                                name = "Sep",
                                marker = list(
                                  color = "#72559D", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="10"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="10"],
                                mode = 'markers',
                                name = "Oct",
                                marker = list(
                                  color = "#9368D1", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="11"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="11"],
                                mode = 'markers',
                                name = "Nov",
                                marker = list(
                                  color = "#9592DB", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="12"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="12"],
                                mode = 'markers',
                                name = "Dec",
                                marker = list(
                                  color = "#000000", #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
    
    fig1 <- fig1 %>% add_trace( r = mo_adj.pal$rep_ws[mo_adj.pal$month=="4"],
                                theta = mo_adj.pal$wd[mo_adj.pal$month=="4"],
                                mode = 'markers',
                                name = "Apr",
                                marker = list(
                                  color = alpha("#F4BA45",6), #colors = "#8090c7",
                                  opacity = 1.0,
                                  symbol = 'circle',
                                  size = 20, rotation = 90
                                ))
    
  }
}

# layout
  fig1 <- fig1 %>% layout(fig1, 
                          polar = list(
                            angularaxis = list(
                              tickfont = list(
                                size = 18, 
                                color = "black"),
                              rotation = 90,
                              direction = 'clockwise'
                            ),
                            radialaxis = list(
                              tickfont = list(
                                size = 18, 
                                color = "black"),
                              side = 'counterclockwise',
                              showline = T,
                              linewidth = 2,
                              tickwidth = 2,
                              gridcolor = '#FB8072“',
                              gridwidth = 1,
                              range=c(0,9)
                            )), showlegend = F)
  
  fig1

### monthly mean difference wind speed #####
min(mo_manual$WS_avg_2min) # 3.2
max(mo_manual$WS_avg_2min) # 7.0

min(mo_adj.pal$WS_avg_2min) # 3.2
max(mo_adj.pal$WS_avg_2min) # 7.1 
diff_ws <- mo_adj.pal$WS_avg_2min - mo_manual$WS_avg_2min
diff_ws
mean(diff_ws)


#####princax analysis of principal variance #####
#### dataframes for matlab princax function #####
# w <- cbind.data.frame(daily.pal_overlap$u, daily.pal_overlap$v)
#   write.csv(w, "adj_palmos_w.csv")
# 
# w <- cbind.data.frame(daily.man_overlap$u, daily.man_overlap$v)
#  write.csv(w, "manual_w.csv")

 
# the difference in the direction of principal variance = 14.1º
# for adjusted PalMOS - manual direction of princ var

#### apply wind direction CF #####
# uses results from princax analysis in Matlab
man_adj2$man_adj_wd <- NA
man_adj2$man_adj_wd <- man_adj2$Wind.Peak.Direction - 14.1
man_adj2$man_adj_wd <- ifelse(man_adj2$man_adj_wd < 0, man_adj2$man_adj_wd + 360, man_adj2$man_adj_wd)


# convert to: man_adj_u and man_adj_v
man_adj2$man_adj_v <- -man_adj2$man_adj_ws * cos(man_adj2$man_adj_wd*pi/180)
man_adj2$man_adj_u <- -man_adj2$man_adj_ws * sin(man_adj2$man_adj_wd*pi/180)


#Precipitation catch ratios  manual-adj Pal  ##### 
#check that the datasets (adjusted palmos and manual) align
daily.pal_overlap$day - daily.man_overlap$day
daily.man_overlap$Precipitation.Melted..mm. <- as.numeric(daily.man_overlap$Precipitation.Melted..mm.)

### stats for Table S1 #####
cor.test(daily.man_overlap$Precipitation.Melted..mm., daily.pal_overlap$precip.melted, method="spearman")
wilcox.test(daily.man_overlap$Precipitation.Melted..mm., daily.pal_overlap$precip.melted, paired=TRUE)


### Fig 5c manual adj PalMOS overlap of precip #####
{
plot(daily.man_overlap$Precipitation.Melted..mm., daily.pal_overlap$precip.melted,
     cex.axis=1.5, cex.lab=1.5, cex=1.5,
     col="transparent",
     xlim=c(0,50), ylim=c(0,50),
     xlab=expression(bold("manual precipitation (mm)")),
     ylab=expression(bold("adjusted PalMOS\nprecipitation (mm)")))
abline(v=c(50,45,40,35,30,25,20,15,10,5,0), h=c(50,45,40,35,30,25,20,15,10,5,0), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(35,2,expression(bold("manual—adjusted PalMOS\nSpearman's ρ = 0.859\nn = 656")))
#text(33,2, expression(bold("CF = 1.11")))
points(daily.man_overlap$Precipitation.Melted..mm., daily.pal_overlap$precip.melted, col="black", cex=1.5)

lsf <- lm(daily.pal_overlap$precip.melted ~ daily.man_overlap$Precipitation.Melted..mm.) # lm(y ~ x)
summary(lsf)
# rmse
rmse <- sqrt(mean(lsf$residuals^2))
round(rmse,1)

}
addfiglab("(c)")

## monthly catch ratio values calculated from overlapping datasets #####
{
  par(mfrow=c(3,4))
  # sept 2002-2003 == 0.8
  {
    sep_pal_overlap <- subset(daily.pal_overlap, month=="9")
    sep_man_overlap <- subset(daily.man_overlap, month=="9")
    
    sep_man_overlap$Precipitation.Melted..mm. <- as.numeric(sep_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(sep_pal_overlap$precip.melted, sep_man_overlap$Precipitation.Melted..mm.) #palmos vs manual
          shapiro.test(sep_pal_overlap$precip.melted) 
    t.test(sep_pal_overlap$precip.melted, sep_man_overlap$Precipitation.Melted..mm., paired=TRUE) # palmos and paws are not different
    plot(sep_pal_overlap$precip.melted, sep_man_overlap$Precipitation.Melted..mm.) #palmos vs paws
    
    sep_catch <- sum(sep_pal_overlap$precip.melted, na.rm=T) / 
                          sum(sep_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)  
  }
    sep_catch
  
  # oct 2002 2003 == 0.8
  {
    oct_pal_overlap <- subset(daily.pal_overlap, month=="10") 
    oct_man_overlap <- subset(daily.man_overlap, month=="10")
  
    oct_man_overlap$Precipitation.Melted..mm. <- as.numeric(oct_man_overlap$Precipitation.Melted..mm.)
    cor.test(oct_pal_overlap$precip.melted, oct_man_overlap$Precipitation.Melted..mm.) #palmos vs paws
    shapiro.test(oct_pal_overlap$precip.melted) # data are normally distributed p>0.05
    t.test(oct_pal_overlap$precip.melted, oct_man_overlap$Precipitation.Melted..mm., paired=TRUE) # palmos and paws are not different
    plot(oct_pal_overlap$precip.melted, oct_man_overlap$Precipitation.Melted..mm.) #palmos vs paws
    
    oct_catch <- sum(oct_pal_overlap$precip.melted, na.rm=T) / 
                              sum(oct_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  oct_catch
  
  # nov 2002-2003 == 0.9
  {
    nov_pal_overlap <- subset(daily.pal_overlap, month=="11") 
    nov_man_overlap <- subset(daily.man_overlap, month=="11")
    
    nov_man_overlap$Precipitation.Melted..mm. <- as.numeric(nov_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(nov_pal_overlap$precip.melted, nov_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(nov_pal_overlap$precip.melted) 
    t.test(nov_pal_overlap$precip.melted, nov_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(nov_pal_overlap$precip.melted, nov_man_overlap$Precipitation.Melted..mm.) 
    
    nov_catch <- sum(nov_pal_overlap$precip.melted, na.rm=T) / 
                  sum(nov_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
    
   }
  nov_catch
  
  # dec 2002-2003 == 1.0
  {
    dec_pal_overlap <- subset(daily.pal_overlap, month=="12") 
    dec_man_overlap <- subset(daily.man_overlap, month=="12")
    
    dec_man_overlap$Precipitation.Melted..mm. <- as.numeric(dec_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(dec_pal_overlap$precip.melted, dec_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(dec_pal_overlap$precip.melted) 
    t.test(dec_pal_overlap$precip.melted, dec_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(dec_pal_overlap$precip.melted, dec_man_overlap$Precipitation.Melted..mm.) 
    
    dec_catch <- sum(dec_pal_overlap$precip.melted, na.rm=T) / 
                  sum(dec_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
    
  }
  dec_catch
  
  # Jan 2002-2003 == 1.3
  {
    jan_pal_overlap <- subset(daily.pal_overlap, month=="1") 
    jan_man_overlap <- subset(daily.man_overlap, month=="1")
    
    jan_man_overlap$Precipitation.Melted..mm. <- as.numeric(jan_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(jan_pal_overlap$precip.melted, jan_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(jan_pal_overlap$precip.melted) 
    t.test(jan_pal_overlap$precip.melted, jan_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(jan_pal_overlap$precip.melted, jan_man_overlap$Precipitation.Melted..mm.) 
    
    jan_catch <- sum(jan_pal_overlap$precip.melted, na.rm=T) / 
                    sum(jan_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
    
  }
  jan_catch
  
  # Feb 2002-2003 == 1.8
  {
    feb_pal_overlap <- subset(daily.pal_overlap, month=="2") 
    feb_man_overlap <- subset(daily.man_overlap, month=="2")
    
    feb_man_overlap$Precipitation.Melted..mm. <- as.numeric(feb_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(feb_pal_overlap$precip.melted, feb_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(feb_pal_overlap$precip.melted) 
    t.test(feb_pal_overlap$precip.melted, feb_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(feb_pal_overlap$precip.melted, feb_man_overlap$Precipitation.Melted..mm.) 
    
    feb_catch <- sum(feb_pal_overlap$precip.melted, na.rm=T) / 
                    sum(feb_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  feb_catch
  
  # Mar 2002-2003 ==. 1.6
  {
    mar_pal_overlap <- subset(daily.pal_overlap, month=="3") 
    mar_man_overlap <- subset(daily.man_overlap, month=="3")
    
    mar_man_overlap$Precipitation.Melted..mm. <- as.numeric(mar_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(mar_pal_overlap$precip.melted, mar_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(mar_pal_overlap$precip.melted) 
    t.test(mar_pal_overlap$precip.melted, mar_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(mar_pal_overlap$precip.melted, mar_man_overlap$Precipitation.Melted..mm.) 
    plot(mar_man_overlap$Precipitation.Melted..mm., type = "l") 
    lines(mar_pal_overlap$precip.melted, type = "l", col="blue") 
    
    mar_catch <- sum(mar_pal_overlap$precip.melted, na.rm=T) / 
                    sum(mar_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  mar_catch
  
  # Apr 2002-2003 == 0.8
  {
    apr_pal_overlap <- subset(daily.pal_overlap, month=="4") 
    apr_man_overlap <- subset(daily.man_overlap, month=="4")
    
    apr_man_overlap$Precipitation.Melted..mm. <- as.numeric(apr_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(apr_pal_overlap$precip.melted, apr_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(apr_pal_overlap$precip.melted) 
    t.test(apr_pal_overlap$precip.melted, apr_man_overlap$Precipitation.Melted..mm.) 
    plot(apr_pal_overlap$precip.melted, apr_man_overlap$Precipitation.Melted..mm.) 
    
    apr_catch <- sum(apr_pal_overlap$precip.melted, na.rm=T) / 
                    sum(apr_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  apr_catch
  
  # May 2002-2003 == 0.6
  {
    may_pal_overlap <- subset(daily.pal_overlap, month=="5") 
    may_man_overlap <- subset(daily.man_overlap, month=="5")
    
    may_man_overlap$Precipitation.Melted..mm. <- as.numeric(may_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(may_pal_overlap$precip.melted, may_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(may_pal_overlap$precip.melted) 
    t.test(may_pal_overlap$precip.melted, may_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(may_pal_overlap$precip.melted, may_man_overlap$Precipitation.Melted..mm.) 
    
    may_catch <- sum(may_pal_overlap$precip.melted, na.rm=T) / 
                    sum(may_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  may_catch
  
  # Jun 2002-2003 == 1.0
  {
    jun_pal_overlap <- subset(daily.pal_overlap, month=="6") 
    jun_man_overlap <- subset(daily.man_overlap, month=="6")
    
    jun_man_overlap$Precipitation.Melted..mm. <- as.numeric(jun_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(jun_pal_overlap$precip.melted, jun_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(jun_pal_overlap$precip.melted) 
    t.test(jun_pal_overlap$precip.melted, jun_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(jun_pal_overlap$precip.melted, jun_man_overlap$Precipitation.Melted..mm.) 
    
    jun_catch <- sum(jun_pal_overlap$precip.melted, na.rm=T) / 
                    sum(jun_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  jun_catch
  
  # Jul 2002-2003 == 4.4
  {
    jul_pal_overlap <- subset(daily.pal_overlap, month=="7") 
    jul_man_overlap <- subset(daily.man_overlap, month=="7")
    
    jul_man_overlap$Precipitation.Melted..mm. <- as.numeric(jul_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(jul_pal_overlap$precip.melted, jul_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(jul_pal_overlap$precip.melted) 
    t.test(jul_pal_overlap$precip.melted, jul_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(jul_pal_overlap$precip.melted, jul_man_overlap$Precipitation.Melted..mm.) 
    plot(jul_man_overlap$Precipitation.Melted..mm., type="l") 
    lines(jul_pal_overlap$precip.melted/4,col="blue") 
    
    jul_catch <- sum(jul_pal_overlap$precip.melted, na.rm=T) / 
                      sum(jul_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  jul_catch
  
  # Aug 2016 == 2.2
  {
    aug_pal_overlap <- subset(daily.pal_overlap, month=="8") 
    aug_man_overlap <- subset(daily.man_overlap, month=="8")
    
    aug_man_overlap$Precipitation.Melted..mm. <- as.numeric(aug_man_overlap$Precipitation.Melted..mm.)
    
    cor.test(aug_pal_overlap$precip.melted, aug_man_overlap$Precipitation.Melted..mm.) 
    shapiro.test(aug_pal_overlap$precip.melted) 
    t.test(aug_pal_overlap$precip.melted, aug_man_overlap$Precipitation.Melted..mm., paired=TRUE) 
    plot(aug_pal_overlap$precip.melted, aug_man_overlap$Precipitation.Melted..mm.) 
    
    aug_catch <- sum(aug_pal_overlap$precip.melted, na.rm=T) / 
                        sum(aug_man_overlap$Precipitation.Melted..mm., na.rm=TRUE)    
  }
  aug_catch
  
} 

## Fig. 5d Catch ratios annual & monthly  plotted #####
# 2-year catch ratio 1.116
{
  ann_catch_ratio_pal_vs_man <- sum(daily.pal_overlap$precip.melted, na.rm=TRUE) / sum(daily.man_overlap$Precipitation.Melted..mm., na.rm=TRUE)
   
  month_ann_catch_ratio_pal_vs_man <- c(jan_catch,feb_catch,
                             mar_catch,apr_catch,
                             may_catch,jun_catch,jul_catch,
                             aug_catch, sep_catch,oct_catch,
                             nov_catch, dec_catch)  
  
  x <- (c("Jan","Feb",
          "Mar","Apr","May","Jun","Jul","Aug",
          "Sep","Oct","Nov","Dec"))
  month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  twoyr.catch.ratios <- cbind.data.frame(x, month, month_ann_catch_ratio_pal_vs_man)

plot(twoyr.catch.ratios$month_ann_catch_ratio_pal_vs_man,
       cex.lab=1.5,cex=2,pch=16, cex.axis=1.5, col="transparent",
       #main="Dec-2001 to Nov-2003",
       ylim=c(0,2), xlab=expression(bold("month")), 
       ylab=expression(bold("precipitation catch ratio\n(adjusted PalMOS:manual)")))
  abline(h=ann_catch_ratio_pal_vs_man,col="darkgrey",lty=3,lwd=4)
  points(twoyr.catch.ratios$month_ann_catch_ratio_pal_vs_man, cex=2,pch=16 )
  legend("bottomleft", legend=c(expression(bold("total precipitation catch ratio"))),
       lwd=4, lty=3, col="darkgrey", bty="n")
  }
addfiglab("(d)")


### convert trace values to ZERO #####
# Define the conditions and replacement values
conditions <- c("T")
replacement_values <- c("0")
# Use replace() to replace
man_adj2$Precipitation.Melted..mm. <- replace(man_adj2$Precipitation.Melted..mm., man_adj2$Precipitation.Melted..mm. 
                    %in% conditions, replacement_values)

## apply precip CF #####
# multiply 1.2 by the palmos precip.melted value
man_adj2$adj_pm <- NA
man_adj2$Precipitation.Melted..mm. <- as.numeric(man_adj2$Precipitation.Melted..mm.,na.rm=TRUE)
man_adj2$adj_pm <- man_adj2$Precipitation.Melted..mm. * ann_catch_ratio_pal_vs_man # aka == 1.1
man_adj2$adj_pm

#plot(man_adj2$dates, man_adj2$adj_pm, type="l")

# GRAND COMBINATION ##### 
## PalMOS AWS dataset #####
# hourly
# final_pal_adj <- cbind.data.frame(pal_adj_not_for_overlap$date, pal_adj_not_for_overlap$year, pal_adj_not_for_overlap$month, pal_adj_not_for_overlap$day, pal_adj_not_for_overlap$hour,
#                                   pal_adj_not_for_overlap$adj_t, pal_adj_not_for_overlap$hourly_precip, #pal_adj_not_for_overlap$adj_pm, 
#                                   pal_adj_not_for_overlap$adj_ws,
#                                   pal_adj_not_for_overlap$adj_u, pal_adj_not_for_overlap$adj_v, pal_adj_not_for_overlap$adj_rh, pal_adj_not_for_overlap$adj_pres)
# 
# names(final_pal_adj)[1:12] <-c("date", "year", "month", "day", "hour",
#                                "temperature", "precip_melted", "WS_avg_2min",
#                                "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity", "Pressure")
# daily
final_pal_adj <- cbind.data.frame(pal_adj_not_for_overlap$date, pal_adj_not_for_overlap$year, pal_adj_not_for_overlap$month, pal_adj_not_for_overlap$day,
                                  pal_adj_not_for_overlap$adj_t, pal_adj_not_for_overlap$adj_pm, pal_adj_not_for_overlap$adj_ws,
                                  pal_adj_not_for_overlap$adj_u, pal_adj_not_for_overlap$adj_v, pal_adj_not_for_overlap$adj_rh, pal_adj_not_for_overlap$adj_pres)

names(final_pal_adj)[1:11] <-c("date", "year", "month", "day", 
                                   "temperature", "precip_melted", "WS_avg_2min",
                                   "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity", "Pressure")


# head(final_pal_adj) # 12-2001 to
# tail(final_pal_adj) # 08-2016
# head(final_man_adj) # 04-1989 to
# tail(final_man_adj) # 11-2001
# head(final_paws) # 04-1989 to # should start 2016-09-01
# tail(final_paws) # 04-2024
## manual observations dataset #####
man_adj2$empty_rh <- NA
final_man_adj <-  cbind.data.frame(man_adj2$dates, man_adj2$year, man_adj2$month, man_adj2$day,
                                        man_adj2$man_adj2_T, man_adj2$adj_pm, man_adj2$man_adj_ws,
                                   man_adj2$man_adj_u, man_adj2$man_adj_v, man_adj2$empty_rh, man_adj2$man_adj2_P)
names(final_man_adj)[1:11] <-c("date", "year", "month", "day", 
                               "temperature", "precip_melted", "WS_avg_2min",
                               "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity","Pressure")

## PAWS dataset #####
# setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/PAWS relative humidity 2016-present")
# paws <- read.csv("PAWS daily 2015-2024.csv", header=TRUE)
# head(paws,20) # begins 2015-10-01
# colnames(paws) # ends 2024-11-30
# 
# final_paws <- cbind.data.frame(paws$date, paws$year, paws$month, paws$day,
#                                paws$Air.temp, paws$precip.melted, paws$WS.Avg.2min,
#                                paws$u_wind, paws$v_wind, paws$Rel.Humidity)
# names(final_paws)[1:10] <-c("date", "year", "month", "day", 
#                                "temperature", "precip_melted", "WS_avg_2min",
#                                "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity")
# 

# set up paws dataset using 1min observations
{
 #setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate")
setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/PAWS relative humidity 2016-present")
paws <- read.csv("PAWS_2min_Oct2015_Sep2024.csv", header=TRUE)

#paws <- subset(paws, year== "2019")
head(paws,3)
tail(paws)

paws$dates <- as.POSIXlt(paws$dates, format = "%Y-%m-%d %H:%M:%S", tz="UTC")

## hourly code
#1 create column with hour
# paws$hour <- as.POSIXlt(paws$dates)$hour

# Paste the columns together into a character string
##date_string <- paste(paws$year, paws$month, paws$day, sep = "-")

# Convert the character string to a Date object
##paws$dates <- as.Date(date_string, format = "%Y-%m-%d")

paws$year <- format(as.Date(paws$dates, format="Y%/%m/%d"),"%Y")
paws$month <- format(as.Date(paws$dates, format="Y%/%m/%d"),"%m")
paws$day <- format(as.Date(paws$dates, format="Y%/%m/%d"),"%d")

paws$year <- as.numeric(paws$year,na.rm=T)
paws$month <- as.numeric(paws$month,na.rm=T)
paws$day <- as.numeric(paws$day,na.rm=T)

paws$Air.Pressure <- as.numeric(paws$Air.Pressure)
paws$Air.Temp <- as.numeric(paws$Air.Temp)
paws$Rel.Humidity <- as.numeric(paws$Rel.Humidity)
paws$WS.Avg.2min <- as.numeric(paws$WS.Avg.2min) 
paws$WD.Avg.2min <- as.numeric(paws$WD.Avg.2min)
paws$Rainfall <- as.numeric(paws$Rainfall)

paws$v <- -paws$WS.Avg.2min * cos(paws$WD.Avg.2min*pi/180)
paws$u <- -paws$WS.Avg.2min * sin(paws$WD.Avg.2min*pi/180)

# convert PAWS surface pressure to mean sea level pressure
paws$Air.Pressure <- paws$Air.Pressure + (40 / (29.27 * Tv))

# calc mean rel hum

# daily
paws_daily <- paws %>%
  group_by(year,month,day) %>%
  summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
            WS_avg_2min = mean(WS.Avg.2min, na.rm=TRUE),
            u = mean(u, na.rm=TRUE),
            v = mean(v, na.rm=TRUE),
            Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
            precip.melted = sum(Rainfall, na.rm=TRUE),
            Air.Pressure = mean(Air.Pressure, na.rm=TRUE)) %>%
  mutate(date = make_date(year, month, day))




# hourly
# paws_hourly <- paws %>%
#   group_by(year,month,day,hour) %>%
#   summarise(Air.Temp=mean(Air.Temp, na.rm=TRUE),
#             WS_avg_2min = mean(WS.Avg.2min, na.rm=TRUE),
#             u = mean(u, na.rm=TRUE),
#             v = mean(v, na.rm=TRUE),
#             Rel.Humidity = mean(Rel.Humidity, na.rm=TRUE),
#             precip.melted = sum(Rainfall, na.rm=TRUE),
#             Air.Pressure = mean(Air.Pressure, na.rm=TRUE)) %>%
#   mutate(date = make_datetime(year, month, day, hour))
# plot(paws_hourly$precip.melted, type="l")
# paws_hourly <- paws_hourly[-78850,]
# tail(paws_hourly)
# hourly final paws
# TRIM to: oct 2015 to 30-04-2024
# final_paws <- paws_hourly[c(1:73764),c(12,1,2,3,4, 5,10,6, 7,8,9,11)]
# names(final_paws)[1:12] <-c("date", "year", "month", "day", "hour", 
#                             "temperature", "precip_melted", "WS_avg_2min",
#                             "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity","Pressure")
# 


##### help rh old stuff #####
{
# add paws data april 01 2017 onward for rel humiidty and wind direction (u and v wind components)
# setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate")
# paws_rh <- read.csv("paws_rel_hum.csv",header=T)
# paws_rh <- paws_rh[c(1:3121),] # trim oct 2015 to 30-04-2024
# 
# 
# man$fake_rh <- NA
# 
# final_paws <-  cbind.data.frame(man$dates, man$year, man$month, man$day,
#                                 man$Temperature.Average..C.,  man$Precipitation.Melted..mm.,
#                                 man$Wind.Average..knots., man$u, man$v, man$fake_rh)
# names(final_paws)[1:10] <-c("date", "year", "month", "day", 
#                                "temperature", "precip_melted", "WS_avg_2min",
#                                "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity")
# final_paws <- final_paws[c(9649:12780),] # trim to [oct-01-2015 to apr-1-2024]
# final_paws$precip_melted <- as.numeric(final_paws$precip_melted)
# final_paws$WS_avg_2min <- final_paws$WS_avg_2min * 0.514444
# 


# # combine the rh with final_paws, by merging date column
# paws_rh$date <- as.Date(paws_rh$date)
# final_paws_rh <- merge(x = final_paws, y = paws_rh, #by="date")
#       by.x = "date", by.y = "date")
# colnames(final_paws_rh)
# 
# # clean up u and v wind in paws_rh  to match the other dataset col names
# final_paws_rh$U_WD_avg_2min <- final_paws_rh$u_wind
# final_paws_rh$V_WD_avg_2min <- final_paws_rh$v_wind
# 
# final_paws_rh <- final_paws_rh[,-c(10:14,16,17)]
# names(final_paws_rh)[1:10] <-c("date", "year", "month", "day", 
#                             "temperature", "precip_melted", "WS_avg_2min",
#                             "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity")
}

}

# trim oct 2015 to 30-04-2024
paws_daily[3119,]
head(paws_daily)
# daily final paws
final_paws <- paws_daily[c(1:3119),c(11,1,2,3,4,9, 5,6,7,8,10)]
names(final_paws)[1:11] <-c("date", "year", "month", "day", 
                               "temperature", "precip_melted", "WS_avg_2min",
                               "U_WD_avg_2min", "V_WD_avg_2min", "rel_humidity","Pressure")

# FINAL data set #####      

# remove the end of the Palmos dataset to extend paws to end of SEP 2015.
# final_pal_adj <-  final_pal_adj[-c(5017:5348),]

# hourly
# final_adj_Palmer <- rbind(final_pal_adj, final_paws)

# daily
# combine manual, palmos, paws
final_adj_Palmer <- rbind(final_man_adj, final_pal_adj, final_paws)

                                  # checks for missing stuff amrdc
                                  {
                                  # 01-APR-1989 to 30-NOV-2001        # 01-DEC-2001 to 30-SEP-2015       # 01-OCT-2015 to 30-APR-2024
                                  head(final_man_adj)
                                  tail(final_man_adj)
                                  
                                  head(final_pal_adj)
                                  tail(final_pal_adj)
                                  
                                  head(final_paws)
                                  tail(final_paws)
                                  
                                  # check amrdc raw data for the missing 2010 data; result= all set at 100% rh for 29 days
                                  # these file came from here: 
                                  # https://amrdcdata.ssec.wisc.edu/dataset/palmer-station-automated-weather-data-system-observational-data/resource/312404fc-3421-4fc6-af28-4ad2b70ab6d9
                                  {
                                  # setwd("/Users/dulcineagroff/Downloads/2010/September")
                                  # sep08 <- read.table("AR100908.100",sep="", skip = 2)
                                  # sep09 <- read.table("AR100909.100",sep="", skip = 2)
                                  # sep10 <- read.table("AR100910.100",sep="", skip = 2)
                                  # sep11 <- read.table("AR100911.100",sep="", skip = 2)
                                  # sep12 <- read.table("AR100912.100",sep="", skip = 2)
                                  # sep13 <- read.table("AR100913.100",sep="", skip = 2)
                                  # sep14 <- read.table("AR100914.100",sep="", skip = 2)
                                  # sep15 <- read.table("AR100915.100",sep="", skip = 2)
                                  # sep16 <- read.table("AR100916.100",sep="", skip = 2)
                                  # sep17 <- read.table("AR100917.100",sep="", skip = 2)
                                  # sep18 <- read.table("AR100918.100",sep="", skip = 2)
                                  # sep19 <- read.table("AR100919.100",sep="", skip = 2)
                                  # sep20 <- read.table("AR100920.100",sep="", skip = 2)
                                  # sep21 <- read.table("AR100921.100",sep="", skip = 2)
                                  # sep22 <- read.table("AR100922.100",sep="", skip = 2)
                                  # sep23 <- read.table("AR100923.100",sep="", skip = 2)
                                  # sep24 <- read.table("AR100924.100",sep="", skip = 2)
                                  # sep25 <- read.table("AR100925.100",sep="", skip = 2)
                                  # sep26 <- read.table("AR100926.100",sep="", skip = 2)
                                  # sep27 <- read.table("AR100927.100",sep="", skip = 2)
                                  # sep28 <- read.table("AR100928.100",sep="", skip = 2)
                                  # sep29 <- read.table("AR100929.100",sep="", skip = 2)
                                  # sep30 <- read.table("AR100930.100",sep="", skip = 2)
                                  # 
                                  # setwd("/Users/dulcineagroff/Downloads/2010/October")
                                  # oct01 <- read.table("AR101001.100",sep="", skip = 2)
                                  # oct02 <- read.table("AR101002.100",sep="", skip = 2)
                                  # oct03 <- read.table("AR101003.100",sep="", skip = 2)
                                  # oct04 <- read.table("AR101004.100",sep="", skip = 2)
                                  # oct05 <- read.table("AR101005.100",sep="", skip = 2)
                                  # oct06 <- read.table("AR101006.100",sep="", skip = 2)
                                  # oct07 <- read.table("AR101007.100",sep="", skip = 2)
                                  # 
                                  # 
                                  # missing.rel.hum <- rbind(sep08, sep09, sep10, sep11, sep12, sep13, sep14, sep15, sep16, sep17, sep18, sep19,
                                  #       sep20, sep21, sep22, sep23, sep24, sep25, sep26, sep27, sep28, sep29, sep30,
                                  #       oct01, oct02, oct03, oct04, oct05, oct06, oct07)
                                  # plot(missing.rel.hum$V14)
                                  
                                  
                                  # september 9, 2010 through october 6, 2010
                                  
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2010], final_adj_Palmer$precip_melted[final_adj_Palmer$year==2010], 
                                  #      type="l", xlab="", ylab="precipitation (mm)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2010], final_adj_Palmer$temperature[final_adj_Palmer$year==2010], 
                                  #      type="b", xlab="", ylab="temperature (ºC)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2010], final_adj_Palmer$WS_avg_2min[final_adj_Palmer$year==2010], 
                                  #      type="l", xlab="", ylab="wind speed (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2010], final_adj_Palmer$U_WD_avg_2min[final_adj_Palmer$year==2010], 
                                  #      type="l", xlab="", ylab="u wind (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2010], final_adj_Palmer$V_WD_avg_2min[final_adj_Palmer$year==2010], 
                                  #      type="l", xlab="", ylab="v wind (m/w)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2010], final_adj_Palmer$rel_humidity[final_adj_Palmer$year==2010], 
                                  #      type="l", xlab="", ylab="relative humidity", col="hotpink")
                                  # 
                                  # # 2009 to 2006 
                                  # {
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2009], final_adj_Palmer$precip_melted[final_adj_Palmer$year==2009], 
                                  #      type="l", xlab="", ylab="precipitation (mm)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2009], final_adj_Palmer$temperature[final_adj_Palmer$year==2009], 
                                  #      type="b", xlab="", ylab="temperature (ºC)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2009], final_adj_Palmer$WS_avg_2min[final_adj_Palmer$year==2009], 
                                  #      type="l", xlab="", ylab="wind speed (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2009], final_adj_Palmer$U_WD_avg_2min[final_adj_Palmer$year==2009], 
                                  #      type="l", xlab="", ylab="u wind (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2009], final_adj_Palmer$V_WD_avg_2min[final_adj_Palmer$year==2009], 
                                  #      type="l", xlab="", ylab="v wind (m/w)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2009], final_adj_Palmer$rel_humidity[final_adj_Palmer$year==2009], 
                                  #      type="l", xlab="", ylab="relative humidity", col="hotpink")
                                  # 
                                  # 
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2008], final_adj_Palmer$precip_melted[final_adj_Palmer$year==2008], 
                                  #      type="l", xlab="", ylab="precipitation (mm)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2008], final_adj_Palmer$temperature[final_adj_Palmer$year==2008], 
                                  #      type="b", xlab="", ylab="temperature (ºC)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2008], final_adj_Palmer$WS_avg_2min[final_adj_Palmer$year==2008], 
                                  #      type="l", xlab="", ylab="wind speed (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2008], final_adj_Palmer$U_WD_avg_2min[final_adj_Palmer$year==2008], 
                                  #      type="l", xlab="", ylab="u wind (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2008], final_adj_Palmer$V_WD_avg_2min[final_adj_Palmer$year==2008], 
                                  #      type="l", xlab="", ylab="v wind (m/w)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2008], final_adj_Palmer$rel_humidity[final_adj_Palmer$year==2008], 
                                  #      type="l", xlab="", ylab="relative humidity", col="hotpink")
                                  # 
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2007], final_adj_Palmer$precip_melted[final_adj_Palmer$year==2007], 
                                  #      type="l", xlab="", ylab="precipitation (mm)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2007], final_adj_Palmer$temperature[final_adj_Palmer$year==2007], 
                                  #      type="b", xlab="", ylab="temperature (ºC)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2007], final_adj_Palmer$WS_avg_2min[final_adj_Palmer$year==2007], 
                                  #      type="l", xlab="", ylab="wind speed (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2007], final_adj_Palmer$U_WD_avg_2min[final_adj_Palmer$year==2007], 
                                  #      type="l", xlab="", ylab="u wind (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2007], final_adj_Palmer$V_WD_avg_2min[final_adj_Palmer$year==2007], 
                                  #      type="l", xlab="", ylab="v wind (m/w)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2007], final_adj_Palmer$rel_humidity[final_adj_Palmer$year==2007], 
                                  #      type="l", xlab="", ylab="relative humidity", col="hotpink")
                                  # 
                                  # 
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2006], final_adj_Palmer$precip_melted[final_adj_Palmer$year==2006], 
                                  #      type="l", xlab="", ylab="precipitation (mm)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2006], final_adj_Palmer$temperature[final_adj_Palmer$year==2006], 
                                  #      type="b", xlab="", ylab="temperature (ºC)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2006], final_adj_Palmer$WS_avg_2min[final_adj_Palmer$year==2006], 
                                  #      type="l", xlab="", ylab="wind speed (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2006], final_adj_Palmer$U_WD_avg_2min[final_adj_Palmer$year==2006], 
                                  #      type="l", xlab="", ylab="u wind (m/s)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2006], final_adj_Palmer$V_WD_avg_2min[final_adj_Palmer$year==2006], 
                                  #      type="l", xlab="", ylab="v wind (m/w)")
                                  # plot(final_adj_Palmer$date[final_adj_Palmer$year==2006], final_adj_Palmer$rel_humidity[final_adj_Palmer$year==2006], 
                                  #      type="l", xlab="", ylab="relative humidity", col="hotpink")
                                  # 
                                   }
                                  
}


# adjusted but not gap filled hourly dataset 
setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/PAWS relative humidity 2016-present")
# write.csv(final_adj_Palmer, "final_adj_Palmer.csv", col.names =TRUE)
setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/PAWS relative humidity 2016-present")
final_adj_Palmer <- read.csv("final_adj_Palmer.csv", header=TRUE)
final_adj_Palmer <-  final_adj_Palmer[,-1]
final_adj_Palmer$date <- as.Date(final_adj_Palmer$date)

plot(final_adj_Palmer$date, final_adj_Palmer$temperature, type="l")
plot(final_adj_Palmer$date, final_adj_Palmer$WS_avg_2min, type="l")
plot(final_adj_Palmer$date, final_adj_Palmer$rel_humidity, type="l")
plot(final_adj_Palmer$date, final_adj_Palmer$precip_melted, type="l")

# hourly
# hourly Add Missing Rows #####
# start from bottom (most recent date) and work up to the top
{
  library ("berryFunctions")
  ### TOOL TOOOL 
   final_adj_Palmer[1322:1323,]
  # final_adj_Palmer$date <- as.character(final_adj_Palmer$date)
  # final_adj_Palmer[final_adj_Palmer$date == "2002-01-26", ]
  ### TOOL TOOL 
  
  # missing hourly: 2024-02-14
  final_adj_Palmer <- insertRows(final_adj_Palmer, 191993:192016 , new = NA) 
  { final_adj_Palmer[191993:192016, 1] = c("2024-02-14")
  final_adj_Palmer[191993:192016, 2] = c("2024")
  final_adj_Palmer[191993:192016, 3] = c("2")
  final_adj_Palmer[191993:192016, 4] = c("14") } # missing hourly: 2024-02-14
 
  # missing hourly: 2023-10-11 00:01:00 to 2023-10-14 14:00:00
  final_adj_Palmer <- insertRows(final_adj_Palmer, 189096:189180 , new = NA)
  {
  final_adj_Palmer[189096:189118, 1] = c("2023-10-11")
  final_adj_Palmer[189096:189118, 2] = c("2023")
  final_adj_Palmer[189096:189118, 3] = c("10")
  final_adj_Palmer[189096:189118, 4] = c("11")
  
  final_adj_Palmer[189119:189142, 1] = c("2023-10-12")
  final_adj_Palmer[189119:189142, 2] = c("2023")
  final_adj_Palmer[189119:189142, 3] = c("10")
  final_adj_Palmer[189119:189142, 4] = c("12")
  
  final_adj_Palmer[189143:189166, 1] = c("2023-10-13")
  final_adj_Palmer[189143:189166, 2] = c("2023")
  final_adj_Palmer[189143:189166, 3] = c("10")
  final_adj_Palmer[189143:189166, 4] = c("13")
  
  final_adj_Palmer[189167:189180, 1] = c("2023-10-14")
  final_adj_Palmer[189167:189180, 2] = c("2023")
  final_adj_Palmer[189167:189180, 3] = c("10")
  final_adj_Palmer[189167:189180, 4] = c("14")
  } # missing hourly: 2023-10-11 00:00:00 to 2023-10-14 14:00:00
  
  # missing hourly: 2022-10-17 to 2022-10-18
  final_adj_Palmer <- insertRows(final_adj_Palmer, 180601:180648 , new = NA)
  {final_adj_Palmer[180601:180624, 1] = c("2022-10-17")
  final_adj_Palmer[180601:180624, 2] = c("2022")
  final_adj_Palmer[180601:180624, 3] = c("10")
  final_adj_Palmer[180601:180624, 4] = c("17")
  
  final_adj_Palmer[180625:180648, 1] = c("2022-10-18")
  final_adj_Palmer[180625:180648, 2] = c("2022")
  final_adj_Palmer[180625:180648, 3] = c("10")
  final_adj_Palmer[180625:180648, 4] = c("18") }  # missing hourly: 2022-10-17 to 2022-10-18
  
  # missing hourly: 2019-11-21 to 2019-11-30
  final_adj_Palmer <- insertRows(final_adj_Palmer, 156144:156383 , new = NA)
  { final_adj_Palmer[156144:156167, 1] = c("2019-11-21")
  final_adj_Palmer[156144:156167, 2] = c("2019")
  final_adj_Palmer[156144:156167, 3] = c("11")
  final_adj_Palmer[156144:156167, 4] = c("21")
  
  final_adj_Palmer[156168:156191, 1] = c("2019-11-22")
  final_adj_Palmer[156168:156191, 2] = c("2019")
  final_adj_Palmer[156168:156191, 3] = c("11")
  final_adj_Palmer[156168:156191, 4] = c("22")
  
  final_adj_Palmer[156192:156215, 1] = c("2019-11-23")
  final_adj_Palmer[156192:156215, 2] = c("2019")
  final_adj_Palmer[156192:156215, 3] = c("11")
  final_adj_Palmer[156192:156215, 4] = c("23")
  
  final_adj_Palmer[156216:156239, 1] = c("2019-11-24")
  final_adj_Palmer[156216:156239, 2] = c("2019")
  final_adj_Palmer[156216:156239, 3] = c("11")
  final_adj_Palmer[156216:156239, 4] = c("24")
  
  final_adj_Palmer[156240:156263, 1] = c("2019-11-25")
  final_adj_Palmer[156240:156263, 2] = c("2019")
  final_adj_Palmer[156240:156263, 3] = c("11")
  final_adj_Palmer[156240:156263, 4] = c("25")
  
  final_adj_Palmer[156264:156287, 1] = c("2019-11-26")
  final_adj_Palmer[156264:156287, 2] = c("2019")
  final_adj_Palmer[156264:156287, 3] = c("11")
  final_adj_Palmer[156264:156287, 4] = c("26")
  
  final_adj_Palmer[156288:156311, 1] = c("2019-11-27")
  final_adj_Palmer[156288:156311, 2] = c("2019")
  final_adj_Palmer[156288:156311, 3] = c("11")
  final_adj_Palmer[156288:156311, 4] = c("27")
  
  final_adj_Palmer[156312:156335, 1] = c("2019-11-28")
  final_adj_Palmer[156312:156335, 2] = c("2019")
  final_adj_Palmer[156312:156335, 3] = c("11")
  final_adj_Palmer[156312:156335, 4] = c("28")
  
  final_adj_Palmer[156336:156359, 1] = c("2019-11-29")
  final_adj_Palmer[156336:156359, 2] = c("2019")
  final_adj_Palmer[156336:156359, 3] = c("11")
  final_adj_Palmer[156336:156359, 4] = c("29")

  final_adj_Palmer[156360:156383, 1] = c("2019-11-30")
  final_adj_Palmer[156360:156383, 2] = c("2019")
  final_adj_Palmer[156360:156383, 3] = c("11")
  final_adj_Palmer[156360:156383, 4] = c("30")
  } # missing hourly: 2019-11-21 to 2019-11-30
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 131086:131109 , new = NA)
  { final_adj_Palmer[131086:131109, 1] = c("2017-01-05")
  final_adj_Palmer[131086:131109, 2] = c("2017")
  final_adj_Palmer[131086:131109, 3] = c("01")
  final_adj_Palmer[131086:131109, 4] = c("05")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 106916:106987 , new = NA)
  { final_adj_Palmer[106916:106939, 1] = c("2014-03-29")
    final_adj_Palmer[106916:106939, 2] = c("2014")
    final_adj_Palmer[106916:106939, 3] = c("03")
    final_adj_Palmer[106916:106939, 4] = c("29")
    
    final_adj_Palmer[106940:106963, 1] = c("2014-03-30")
    final_adj_Palmer[106940:106963, 2] = c("2014")
    final_adj_Palmer[106940:106963, 3] = c("03")
    final_adj_Palmer[106940:106963, 4] = c("30")
    
    final_adj_Palmer[106964:106987, 1] = c("2014-03-31")
    final_adj_Palmer[106964:106987, 2] = c("2014")
    final_adj_Palmer[106964:106987, 3] = c("03")
    final_adj_Palmer[106964:106987, 4] = c("31")}
    
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(97614:97637) , new = NA)
  { final_adj_Palmer[97614:97637, 1] = c("2013-03-03")
    final_adj_Palmer[97614:97637, 2] = c("2013")
    final_adj_Palmer[97614:97637, 3] = c("03")
    final_adj_Palmer[97614:97637, 4] = c("03")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(98238:98261) , new = NA)
  { final_adj_Palmer[98238:98261, 1] = c("2013-03-29")
    final_adj_Palmer[98238:98261, 2] = c("2013")
    final_adj_Palmer[98238:98261, 3] = c("03")
    final_adj_Palmer[98238:98261, 4] = c("29")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 62839:62862 , new = NA)
  { final_adj_Palmer[62839:62862, 1] = c("2009-03-12")
    final_adj_Palmer[62839:62862, 2] = c("2009")
    final_adj_Palmer[62839:62862, 3] = c("03")
    final_adj_Palmer[62839:62862, 4] = c("12")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 62623:62646, new = NA)
  { final_adj_Palmer[62623:62646, 1] = c("2009-03-02")
    final_adj_Palmer[62623:62646, 2] = c("2009")
    final_adj_Palmer[62623:62646, 3] = c("03")
    final_adj_Palmer[62623:62646, 4] = c("02")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 60847:60870, new = NA)
  { final_adj_Palmer[60847:60870, 1] = c("2008-12-17")
    final_adj_Palmer[60847:60870, 2] = c("2008")
    final_adj_Palmer[60847:60870, 3] = c("12")
    final_adj_Palmer[60847:60870, 4] = c("17")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 57160:57183 , new = NA)
  { final_adj_Palmer[57160:57183, 1] = c("2008-07-14")
    final_adj_Palmer[57160:57183, 2] = c("2008")
    final_adj_Palmer[57160:57183, 3] = c("07")
    final_adj_Palmer[57160:57183, 4] = c("14")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(56296,56343) , new = NA)
  { final_adj_Palmer[56296:56319, 1] = c("2008-06-06")
    final_adj_Palmer[56296:56319, 2] = c("2008")
    final_adj_Palmer[56296:56319, 3] = c("06")
    final_adj_Palmer[56296:56319, 4] = c("06")
 
   final_adj_Palmer[56320:56343, 1] = c("2008-06-07")
    final_adj_Palmer[56320:56343, 2] = c("2008")
    final_adj_Palmer[56320:56343, 3] = c("06")
    final_adj_Palmer[56320:56343, 4] = c("07")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 55288:55311 , new = NA)
  {final_adj_Palmer[55288:55311, 1] = c("2008-04-24")
  final_adj_Palmer[55288:55311, 2] = c("2008")
  final_adj_Palmer[55288:55311, 3] = c("04")
  final_adj_Palmer[55288:55311, 4] = c("24")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(38524:38619) , new = NA)
  #final_adj_Palmer[c(6241:6244), 1] = c("2006-05-22","2006-05-23","2006-05-24","2006-05-25")
  { final_adj_Palmer[38524:38547, 1] = c("2006-05-22")
  final_adj_Palmer[38524:38547, 2] = c("2006")
  final_adj_Palmer[38524:38547, 3] = c("05")
  final_adj_Palmer[38524:38547, 4] = c("22")
  
  final_adj_Palmer[38548:38571, 1] = c("2006-05-23")
  final_adj_Palmer[38548:38571, 2] = c("2006")
  final_adj_Palmer[38548:38571, 3] = c("05")
  final_adj_Palmer[38548:38571, 4] = c("23")
  
  final_adj_Palmer[38572:38595, 1] = c("2006-05-24")
  final_adj_Palmer[38572:38595, 2] = c("2006")
  final_adj_Palmer[38572:38595, 3] = c("05")
  final_adj_Palmer[38572:38595, 4] = c("24")
  
  final_adj_Palmer[38596:38619, 1] = c("2006-05-25")
  final_adj_Palmer[38596:38619, 2] = c("2006")
  final_adj_Palmer[38596:38619, 3] = c("05")
  final_adj_Palmer[38596:38619, 4] = c("25")
  }
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(37599:37753) , new = NA)
  # final_adj_Palmer[c(6202:6208), 1] = c("2006-04-06","2006-04-07","2006-04-08","2006-04-09",
  #                                       "2006-04-10","2006-04-11","2006-04-12")
 { final_adj_Palmer[37599:37622, 1] = c("2006-04-06")
  final_adj_Palmer[37599:37622, 2] = c("2006")
  final_adj_Palmer[37599:37622, 3] = c("04")
  final_adj_Palmer[37599:37622, 4] = c("06")
  
  final_adj_Palmer[37623:37646, 1] = c("2006-04-07")
  final_adj_Palmer[37623:37646, 2] = c("2006")
  final_adj_Palmer[37623:37646, 3] = c("04")
  final_adj_Palmer[37623:37646, 4] = c("07")
 
  final_adj_Palmer[37647:37670, 1] = c("2006-04-08")
  final_adj_Palmer[37647:37670, 2] = c("2006")
  final_adj_Palmer[37647:37670, 3] = c("04")
  final_adj_Palmer[37647:37670, 4] = c("08")
 
  final_adj_Palmer[37671:37694, 1] = c("2006-04-09")
  final_adj_Palmer[37671:37694, 2] = c("2006")
  final_adj_Palmer[37671:37694, 3] = c("04")
  final_adj_Palmer[37671:37694, 4] = c("09")
 
  final_adj_Palmer[37695:37718, 1] = c("2006-04-10")
  final_adj_Palmer[37695:37718, 2] = c("2006")
  final_adj_Palmer[37695:37718, 3] = c("04")
  final_adj_Palmer[37695:37718, 4] = c("10")
 
  final_adj_Palmer[37719:37742, 1] = c("2006-04-11")
  final_adj_Palmer[37719:37742, 2] = c("2006")
  final_adj_Palmer[37719:37742, 3] = c("04")
  final_adj_Palmer[37719:37742, 4] = c("11")
 
  final_adj_Palmer[37743:37753, 1] = c("2006-04-12")
  final_adj_Palmer[37743:37753, 2] = c("2006")
  final_adj_Palmer[37743:37753, 3] = c("04")
  final_adj_Palmer[37743:37753, 4] = c("12")
}
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 28715:28738 , new = NA)
  #final_adj_Palmer[5830, 1] = c("2005-03-29")
  { final_adj_Palmer[28715:28738, 1] = c("2005-03-29")
  final_adj_Palmer[28715:28738, 2] = c("2005")
  final_adj_Palmer[28715:28738, 3] = c("3")
  final_adj_Palmer[28715:28738, 4] = c("29")}
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(5146:5193) , new = NA)
  #final_adj_Palmer[c(4845:4846), 1] = c("2002-07-16","2002-07-17")
  { final_adj_Palmer[5146:5169, 1] = c("2002-07-16")
  final_adj_Palmer[5146:5169, 2] = c("2002")
  final_adj_Palmer[5146:5169, 3] = c("7")
  final_adj_Palmer[5146:5169, 4] = c("16")
  
  final_adj_Palmer[5170:5193, 1] = c("2002-07-17")
  final_adj_Palmer[5170:5193, 2] = c("2002")
  final_adj_Palmer[5170:5193, 3] = c("7")
  final_adj_Palmer[5170:5193, 4] = c("17")}
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(5098:5145) , new = NA)
  #final_adj_Palmer[c(4843:4844), 1] = c("2002-07-12","2002-07-13")
  { final_adj_Palmer[5098:5121, 1] = c("2002-07-12")
  final_adj_Palmer[5098:5121, 2] = c("2002")
  final_adj_Palmer[5098:5121, 3] = c("7")
  final_adj_Palmer[5098:5121, 4] = c("12")
  
  final_adj_Palmer[5122:5145, 1] = c("2002-07-13")
  final_adj_Palmer[5122:5145, 2] = c("2002")
  final_adj_Palmer[5122:5145, 3] = c("7")
  final_adj_Palmer[5122:5145, 4] = c("13") }

  # start line 1323 hour 2 in 2002-01-26
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(1323:1537) , new = NA)
  # final_adj_Palmer[c(4685:4687), 1] = c("2002-02-01","2002-02-02","2002-02-03")
  # final_adj_Palmer[c(4685:4689), 1] = c("2002-01-27","2002-01-28","2002-01-29",
  #                                       "2002-01-30","2002-01-31")
  { final_adj_Palmer[1323:1345, 1] = c("2002-01-26")
  final_adj_Palmer[1323:1345, 2] = c("2002")
  final_adj_Palmer[1323:1345, 3] = c("1")
  final_adj_Palmer[1323:1345, 4] = c("26")
  
  final_adj_Palmer[1346:1369, 1] = c("2002-01-27")
  final_adj_Palmer[1346:1369, 2] = c("2002")
  final_adj_Palmer[1346:1369, 3] = c("1")
  final_adj_Palmer[1346:1369, 4] = c("27")
  
  final_adj_Palmer[1370:1393, 1] = c("2002-01-28")
  final_adj_Palmer[1370:1393, 2] = c("2002")
  final_adj_Palmer[1370:1393, 3] = c("1")
  final_adj_Palmer[1370:1393, 4] = c("28")
  
  final_adj_Palmer[1394:1417, 1] = c("2002-01-29")
  final_adj_Palmer[1394:1417, 2] = c("2002")
  final_adj_Palmer[1394:1417, 3] = c("1")
  final_adj_Palmer[1394:1417, 4] = c("29")
  
  final_adj_Palmer[1418:1441, 1] = c("2002-01-30")
  final_adj_Palmer[1418:1441, 2] = c("2002")
  final_adj_Palmer[1418:1441, 3] = c("1")
  final_adj_Palmer[1418:1441, 4] = c("30")
  
  final_adj_Palmer[1442:1465, 1] = c("2002-01-31")
  final_adj_Palmer[1442:1465, 2] = c("2002")
  final_adj_Palmer[1442:1465, 3] = c("1")
  final_adj_Palmer[1442:1465, 4] = c("31")
  
  final_adj_Palmer[1466:1489, 1] = c("2002-02-01")
  final_adj_Palmer[1466:1489, 2] = c("2002")
  final_adj_Palmer[1466:1489, 3] = c("2")
  final_adj_Palmer[1466:1489, 4] = c("1")
  
  final_adj_Palmer[1490:1513, 1] = c("2002-02-02")
  final_adj_Palmer[1490:1513, 2] = c("2002")
  final_adj_Palmer[1490:1513, 3] = c("2")
  final_adj_Palmer[1490:1513, 4] = c("2")
  
  final_adj_Palmer[1514:1537, 1] = c("2002-02-03")
  final_adj_Palmer[1514:1537, 2] = c("2002")
  final_adj_Palmer[1514:1537, 3] = c("2")
  final_adj_Palmer[1514:1537, 4] = c("3")}
  
  
  
  
  # trouble shoot missing values - add to table to calculate % missing in a month
  # final_adj_Palmer$check.dates <-  seq(as.Date("04/01/1989",format="%m/%d/%Y"),
  #                                      by="days",length=length(final_adj_Palmer$date))
  # 
  # 
  # final_adj_Palmer$check.dates[final_adj_Palmer$check.dates %in% final_adj_Palmer$date]
  # final_adj_Palmer$check.dates[!(final_adj_Palmer$check.dates %in% final_adj_Palmer$date)]
}
tail(final_adj_Palmer)



# daily
# daily Add Missing Rows #####
# USE THIS
{
  library ("berryFunctions")
  
  final_adj_Palmer[c(4680:4695), ]
  final_adj_Palmer[c(12562:12566), ]
  
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 12687 , new = NA) 
  final_adj_Palmer[12687, 1] = c("2024-02-14")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 12564:12565 , new = NA)
  final_adj_Palmer[12564:12565, 1] = c("2023-10-12","2023-10-13")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 12206:12207 , new = NA)
  final_adj_Palmer[12206:12207, 1] = c("2022-10-17","2022-10-18")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 11155:11164 , new = NA)
  final_adj_Palmer[11155:11164, 1] = c("2019-11-21", "2019-11-22", "2019-11-23", "2019-11-24", "2019-11-25", "2019-11-26", "2019-11-27", "2019-11-28", "2019-11-29", "2019-11-30")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 10106 , new = NA)
  final_adj_Palmer[10106, 1] = c("2017-01-05")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 9096:9098 , new = NA)
  final_adj_Palmer[9096:9098 , 1] = c("2014-03-29","2014-03-30","2014-03-31")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(8707,8733) , new = NA)
  final_adj_Palmer[c(8707,8733) , 1] = c("2013-03-03","2013-03-29")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 7256 , new = NA)
  final_adj_Palmer[7256, 1] = c("2009-03-12")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 7247 , new = NA)
  final_adj_Palmer[7247, 1] = c("2009-03-02")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 7173 , new = NA)
  final_adj_Palmer[7173, 1] = c("2008-12-17")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 7018 , new = NA)
  final_adj_Palmer[7018, 1] = c("2008-07-14")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(6982,6983) , new = NA)
  final_adj_Palmer[c(6982,6983), 1] = c("2008-06-06","2008-06-07")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 6940 , new = NA)
  final_adj_Palmer[6940, 1] = c("2008-04-24")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(6241:6244) , new = NA)
  final_adj_Palmer[c(6241:6244), 1] = c("2006-05-22","2006-05-23","2006-05-24","2006-05-25")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(6202:6208) , new = NA)
  final_adj_Palmer[c(6202:6208), 1] = c("2006-04-06","2006-04-07","2006-04-08","2006-04-09",
                                        "2006-04-10","2006-04-11","2006-04-12")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, 5830 , new = NA)
  final_adj_Palmer[5830, 1] = c("2005-03-29")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(4845:4846) , new = NA)
  final_adj_Palmer[c(4845:4846), 1] = c("2002-07-16","2002-07-17")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(4843:4844) , new = NA)
  final_adj_Palmer[c(4843:4844), 1] = c("2002-07-12","2002-07-13")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(4685:4687) , new = NA)
  final_adj_Palmer[c(4685:4687), 1] = c("2002-02-01","2002-02-02","2002-02-03")
  
  final_adj_Palmer <- insertRows(final_adj_Palmer, c(4685:4689) , new = NA)
  final_adj_Palmer[c(4685:4689), 1] = c("2002-01-27","2002-01-28","2002-01-29",
                                        "2002-01-30","2002-01-31")
  
  
  
  # trouble shoot missing values - add to table to calculate % missing in a month
  final_adj_Palmer$check.dates <-  seq(as.Date("04/01/1989",format="%m/%d/%Y"),
                                       by="days",length=length(final_adj_Palmer$date))
  
  
  final_adj_Palmer$check.dates[final_adj_Palmer$check.dates %in% final_adj_Palmer$date]
  final_adj_Palmer$check.dates[!(final_adj_Palmer$check.dates %in% final_adj_Palmer$date)]
}
tail(final_adj_Palmer)

# # are there months with >15% missing days?
# temp_NAs <- final_adj_Palmer[is.na(final_adj_Palmer$temperature),]
# # n=54 days missing out of 12814 or 0.4% (less than 1%)
# (54/12814)*100
# #write.csv(temp_NAs, "temp_NAs.csv")
# precip_NAs <- final_adj_Palmer[is.na(final_adj_Palmer$precip_melted),]
# # n=113 days missing out of 12814 or 0.9% are missing
# (113/12814)*100
# #write.csv(precip_NAs, "precip_NAs.csv")
# wspd_NAs <- final_adj_Palmer[is.na(final_adj_Palmer$WS_avg_2min),]
# # n=62 days missing out of 12814 or 0.5% are missing (less than (1%))
# (62/12814)*100
# #write.csv(wspd_NAs, "wspd_NAs.csv")
# uw_NAs <- final_adj_Palmer[is.na(final_adj_Palmer$U_WD_avg_2min),]
# # n=113 days missing out of 12814 or 0.9% are missing
# (113/12814)*100
# #write.csv(uw_NAs, "uw_NAs.csv")
# vw_NAs <- final_adj_Palmer[is.na(final_adj_Palmer$V_WD_avg_2min),]
# # n=113 days missing out of 12814 or 0.9% are missing
# (113/12814)*100
# #write.csv(vw_NAs, "vw_NAs.csv")
# rh_NAs <- final_adj_Palmer[is.na(final_adj_Palmer$rel_humidity),]
# rh_NAs <- rh_NAs[-c(1:4627),]
# # n=55 days missing out of (12814-4627) = 8187 or 0.7% are missing
# (55/8187)*100
# #write.csv(rh_NAs, "rh_NAs.csv")


setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate/JTECH 2025 submission")
#write.csv(final_adj_Palmer,"hourly_final_palmer_adjusted_NOT_GAP_FILLED.csv",col.names =T)
#write.csv(final_adj_Palmer,"final_palmer_adjusted_NOT_GAP_FILLED.csv",col.names =T)
final_adj_Palmer <- read.csv("final_palmer_adjusted_NOT_GAP_FILLED.csv", header=TRUE)
#write.csv(final_adj_Palmer,"final_palmer_adjusted_NOT_GAP_FILLED_rh_mean_mo_diffs.csv",col.names =T)
#write.csv(final_adj_Palmer,"final_palmer_adjusted_NOT_GAP_FILLED_rh_mo_diffs.csv",col.names =T)
#write.csv(final_adj_Palmer,"final_palmer_adjusted_NOT_GAP_FILLED_rh_regres.csv",col.names =T)

# *** read in daily ERA5 data #####
{
  # ERA5 Assembled file of daily values for all variables #####
  # mean sea level pressure
  setwd("/Users/dulcineagroff/era5_mslp_1989_2024_Palmer")
  pressure <- read.table("daily_era5_mslp_1989_2024_Palmer.csv",header=T, sep = ";")
  
  # # surface pressure
  # setwd("/Users/dulcineagroff/era5_10m_surface_pressure_1989_2024_Palmer")
  # pressure <- read.table("daily_surface_pressure_1989_2024_Palmer.csv",header=T, sep = ";")
  # 
  # temperature
  setwd("/Users/dulcineagroff/era5_2m_temp_1989_2024_Palmer")
  temperature <- read.table("daily_2m_temp_1989_2024_Palmer.csv",header=T, sep = ";")
  
  #total precipitation
  setwd("/Users/dulcineagroff/era5_tot_precip_1989_2024_Palmer")
  tot.precip <- read.table("daily_tot_precip_1989_2024_Palmer.csv",header=T, sep = ";")
  head(tot.precip)
  
  #2m dewpoint aka rel humidity
  setwd("/Users/dulcineagroff/era5_2m_dewpot_1989_2024_Palmer")
  dewpt <- read.table("daily_dewpt_1989_2024_Palmer.csv", header=T, sep=";")
  
  # v wind component of wind
  setwd("/Users/dulcineagroff/era5_10m_v_wind_1989_2024_Palmer")
  v <- read.table("daily_v_wind_1989_2024_Palmer.csv", header=T, sep=";")
  
  # u wind component of wind
  setwd("/Users/dulcineagroff/era5_10m_u_wind_1989_2024_Palmer")
  u <- read.table("daily_u_wind_1989_2024_Palmer.csv", header=T, sep=";")
  head(u)
  
  # convert temperature and calculate relative humidity
  # RH = 100 * (exp((17.625 * Td) / (243.04 + Td))) / (exp((17.625 * T) / (243.04 + T)))
  pressure$var1.hPa <- pressure$var1 / 100
  temperature$var1.C <- temperature$var1 - 273.15
  dewpt$var1.C <- dewpt$var1 - 273.15
  dewpt$RH = 100 * (exp((17.625 * dewpt$var1.C) / (243.04 + dewpt$var1.C))) / 
    (exp((17.625 * temperature$var1.C) / (243.04 + temperature$var1.C)))
  
  tot.precip$var1.mm <- tot.precip$var1*1000
  windspeed <- sqrt(u$var1^2 + v$var1^2)
  
  # combine time, temperature, precip, rel hum, v wind, u wind **
  era5_daily <- cbind.data.frame(dewpt$time, temperature$var1.C, tot.precip$var1.mm, dewpt$RH, v$var1, u$var1, windspeed, pressure$var1.hPa)
  names(era5_daily)[1:8]  <-c("date","temperature", "tot_precip", "rel_humidity", "v", "u","windspeed","pressure")
  
  # parse date into year, month, day, doy
  era5_daily$date <-  ymd_hms(era5_daily$date)
  era5_daily$year <- format(as.Date(era5_daily$date, format="Y%/%m/%d"),"%Y")
  era5_daily$month <- format(as.Date(era5_daily$date, format="Y%/%m/%d"),"%m")
  era5_daily$day <- format(as.Date(era5_daily$date, fromat="Y%/%m/%d"),"%d")
  era5_daily$year  <- as.numeric(era5_daily$year)
  era5_daily$month <- as.numeric(era5_daily$month)
  era5_daily$day <- as.numeric(era5_daily$day)
  
  # trim the era5 dataset to match the dates of adjusted palmer dataset
  era5_daily <- (era5_daily[c(91:12904),])
}

# *** read in hourly ERA5 data #####
{
  # ERA5 Assembled file of hourly values for all variables #####
  # pressure
  setwd("/Users/dulcineagroff/era5_10m_surface_pressure_1989_2024_Palmer")
  pressure <- read.table("hourly_era5_surface_pressure_1989_2024_Palmer.csv",header=T, sep = ";")
  
  # temperature
  setwd("/Users/dulcineagroff/hourly_era5_2m_temp_1989_2024_Palmer")
  temperature <- read.table("hourly_era5_2m_temp_1989_2024_Palmer.csv",header=T, sep = ";")
  
  #total precipitation
  setwd("/Users/dulcineagroff/era5_tot_precip_1989_2024_Palmer")
  tot.precip <- read.table("hourly_era5_tot_precip_1989_2024_Palmer.csv",header=T, sep = ";")
  head(tot.precip)
  
  #2m dewpoint aka rel humidity
  setwd("/Users/dulcineagroff/era5_2m_dewpot_1989_2024_Palmer")
  dewpt <- read.table("hourly_era5_2m_dewpot_1989_2024_Palmer.csv", header=T, sep=";")
  
  # v wind component of wind
  setwd("/Users/dulcineagroff/era5_10m_v_wind_1989_2024_Palmer")
  v <- read.table("hourly_era5_v_wind_1989_2024_Palmer.csv", header=T, sep=";")
  
  # u wind component of wind
  setwd("/Users/dulcineagroff/era5_10m_u_wind_1989_2024_Palmer")
  u <- read.table("hourly_era5_u_wind_1989_2024_Palmer.csv", header=T, sep=";")
  head(u)
  
  # convert temperature and calculate relative humidity
  # RH = 100 * (exp((17.625 * Td) / (243.04 + Td))) / (exp((17.625 * T) / (243.04 + T)))
  pressure$var1.hPa <- pressure$var1 / 100
  temperature$var1.C <- temperature$var1 - 273.15
  dewpt$var1.C <- dewpt$var1 - 273.15
  dewpt$RH = 100 * (exp((17.625 * dewpt$var1.C) / (243.04 + dewpt$var1.C))) / 
    (exp((17.625 * temperature$var1.C) / (243.04 + temperature$var1.C)))
  
  tot.precip$var1.mm <- tot.precip$var1*1000
  windspeed <- sqrt(u$var1^2 + v$var1^2)
  
  # combine time, temperature, precip, rel hum, v wind, u wind **
  era5_hourly <- cbind.data.frame(dewpt$time, temperature$var1.C, tot.precip$var1.mm, dewpt$RH, v$var1, u$var1, windspeed, pressure$var1.hPa)
  names(era5_hourly)[1:8]  <-c("date","temperature", "tot_precip", "rel_humidity", "v", "u","windspeed","pressure")
  
  # parse date into year, month, day, doy
  era5_hourly$date <-  ymd_hms(era5_hourly$date)
  era5_hourly$year <- format(as.Date(era5_hourly$date, format="Y%/%m/%d"),"%Y")
  era5_hourly$month <- format(as.Date(era5_hourly$date, format="Y%/%m/%d"),"%m")
  era5_hourly$day <- format(as.Date(era5_hourly$date, format="Y%/%m/%d"),"%d")
  era5_hourly$year  <- as.numeric(era5_hourly$year)
  era5_hourly$month <- as.numeric(era5_hourly$month)
  era5_hourly$day <- as.numeric(era5_hourly$day)
  
  # trim the era5 dataset to match the dates of adjusted palmer dataset
  era5_hourly <- (era5_hourly[c(113209:309696),])
}

# which rh CF do I use of the three?

# rh_mo_diffs <- read.csv("final_palmer_adjusted_NOT_GAP_FILLED_rh_mo_diffs.csv", header=TRUE)
# rh_regres <- read.csv("final_palmer_adjusted_NOT_GAP_FILLED_rh_regres.csv", header=TRUE)
# rh_mean_mo_diffs <- read.csv("final_palmer_adjusted_NOT_GAP_FILLED_rh_mean_mo_diffs.csv", header=TRUE)

# plot(rh_mo_diffs$rel_humidity, type="l", col="yellow4")
# lines(rh_regres$rel_humidity, type="l", col="pink")
# lines(rh_mean_mo_diffs$rel_humidity, type="l", col="grey33")

# Correction factors for era5 variables #####
par(mfrow=c(2,4))
par(mar=c(4,6,0.5,0.75)) 

# temperature CF = 0.914
{
lsf <- lm(final_adj_Palmer$temperature ~ era5_daily$temperature)
summary(lsf) # 

cor.test(final_adj_Palmer$temperature, era5_daily$temperature, method="spearman")
# n=12781

plot(era5_daily$temperature, final_adj_Palmer$temperature,
     xlim = c(-27,7), ylim = c(-27,7),
     xlab=expression(bold("ERA5 temperature (ºC)")),
     ylab=expression(bold("adjusted Palmer\ntemperature (ºC)")),
     col="transparent",
     lwd=0.3, cex=1.5, cex.axis=1.5, cex.lab=1.5
)

abline(v=c(-25,-20,-15,-10,-5,0,5), h=c(-25,-20,-15,-10,-5,0,5), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(-16.5,0,expression(bold("ERA5—adjusted Palmer\nSpearman's ρ = 0.949\nn = 12760\nCF = 0.914")))

points(era5_daily$temperature, final_adj_Palmer$temperature, col="black",lwd=0.3, cex=1.5)

}
addfiglab("(a)")

# rel humidity  CF = 0.833
{
lsf <- lm(final_adj_Palmer$rel_humidity ~ era5_daily$rel_humidity)
summary(lsf) #

cor.test(era5_daily$rel_humidity, final_adj_Palmer$rel_humidity, method="spearman")

plot(era5_daily$rel_humidity, final_adj_Palmer$rel_humidity,
     xlim = c(30,105), ylim = c(30,105),
     xlab=expression(bold("ERA5 relative humidity (%)")),
     ylab=expression(bold("adjusted Palmer\nrelative humidity (%)")),
     col="transparent",
     lwd=0.3, cex=1.5, cex.axis=1.5, cex.lab=1.5
     )

abline(v=c(30,40,50,60,70,80,90,100), h=c(30,40,50,60,70,80,90,100), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(53,87,expression(bold("ERA5—adjusted Palmer\nSpearman's\nρ = 0.658\nn = 8054\nCF = 0.833")))

points(era5_daily$rel_humidity, final_adj_Palmer$rel_humidity,col="black",lwd=0.3, cex=1.5)

}
addfiglab("(b)")

# precip CF = 0.493
{
  lsf <- lm(final_adj_Palmer$precip_melted ~ era5_daily$tot_precip)
  summary(lsf) # 
  
  cor.test(era5_daily$tot_precip, final_adj_Palmer$precip_melted, method="spearman")
  
  plot(era5_daily$tot_precip, final_adj_Palmer$precip_melted,
       xlim = c(0,96), ylim = c(0,96),
       xlab=expression(bold("ERA5 total precipitation (mm)")),
       ylab=expression(bold("adjusted Palmer\ntotal precipitation (mm)")),
       col="transparent",
       lwd=0.3, cex=1.5, cex.axis=1.5, cex.lab=1.5
  )
  
  abline(v=c(0,10,20,30,40,50,60,70,80,90), h=c(0,10,20,30,40,50,60,70,80,90), lty = 9, col = "grey33", lwd = 0.75)
  abline(a=0,b=1, lty = 9, col="grey33")
  text(70,2,expression(bold("ERA5—adjusted Palmer\nSpearman's ρ = 0.728\nn = 12216\nCF = 0.562")))
  
  points(era5_daily$tot_precip, final_adj_Palmer$precip_melted, col="black",lwd=0.3, cex=1.5)
  
}
addfiglab("(c)")

plot.new()

# wind speed CF = 1.102
{
# hurry <- subset(final_adj_Palmer, WS_avg_2min==0)
#   hurry$WS_avg_2min[hurry$WS_avg_2min == 0] <- NA

final_adj_Palmer$WS_avg_2min[final_adj_Palmer$WS_avg_2min ==0] <- NA

lsf <- lm(final_adj_Palmer$WS_avg_2min ~ era5_daily$windspeed)
summary(lsf) # 
# n=12781
cor.test(era5_daily$windspeed, final_adj_Palmer$WS_avg_2min, method="spearman")
     
plot(era5_daily$windspeed, final_adj_Palmer$WS_avg_2min,
     xlim = c(-1,25), ylim = c(-1,25),
     xlab=expression(bold("ERA5 wind speed (m/s)")),
     ylab=expression(bold("adjusted Palmer\nwind speed (m/s)")),
     col="transparent",
     lwd=0.3, cex=1.5, cex.axis=1.5, cex.lab=1.5
)

abline(v=c(25,20,15,10,5,0), h=c(25,20,15,10,5,0), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(18,0,expression(bold("ERA5—adjusted Palmer\nSpearman's ρ = 0.580\nn = 12730\nCF = 1.101")))

points(era5_daily$windspeed, final_adj_Palmer$WS_avg_2min, col="black",lwd=0.3, cex=1.5)


}
addfiglab("(d)")

# U wind CF = 0.679
{
final_adj_Palmer$U_WD_avg_2min[final_adj_Palmer$U_WD_avg_2min ==0] <- NA
  
lsf <- lm(final_adj_Palmer$U_WD_avg_2min ~ era5_daily$u)
summary(lsf) # 

cor.test(era5_daily$u, final_adj_Palmer$U_WD_avg_2min, method="spearman")


plot(era5_daily$u, final_adj_Palmer$U_WD_avg_2min,
     xlim = c(-23,17), ylim = c(-23,17),
     xlab=expression(bold("ERA5 u wind (m/s)")),
     ylab=expression(bold("adjusted Palmer u wind (m/s)")),
     col="transparent",
     lwd=0.3, cex=1.5, cex.axis=1.5, cex.lab=1.5
)

abline(v=c(-20,-15,-10,-5,15,10,5,0), h=c(-20,-15,-10,-5,15,10,5,0), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(-12,9.5,expression(bold("ERA5—adjusted Palmer\nSpearman's ρ = 0.722\nn = 12672\nCF = 0.679")))

points(era5_daily$u, final_adj_Palmer$U_WD_avg_2min, col="black",lwd=0.3, cex=1.5)


}
addfiglab("(e)")

# V wind CF = 1.460
{
final_adj_Palmer$V_WD_avg_2min[final_adj_Palmer$V_WD_avg_2min ==0] <- NA
  
lsf <- lm(final_adj_Palmer$V_WD_avg_2min ~ era5_daily$v)
summary(lsf) # 

cor.test(era5_daily$v, final_adj_Palmer$V_WD_avg_2min, method="spearman")

plot(era5_daily$v, final_adj_Palmer$V_WD_avg_2min,
     xlim = c(-23,17), ylim = c(-23,17),
     xlab=expression(bold("ERA5 v wind (m/s)")),
     ylab=expression(bold("adjusted Palmer v wind (m/s)")),
     col="transparent",
     lwd=0.3, cex=1.5, cex.axis=1.5, cex.lab=1.5
)

abline(v=c(-20,-15,-10,-5,15,10,5,0), h=c(-20,-15,-10,-5,15,10,5,0), lty = 9, col = "grey33", lwd = 0.75)
abline(a=0,b=1, lty = 9, col="grey33")
text(-12,9.5,expression(bold("ERA5—adjusted Palmer\nSpearman's ρ = 0.772\nn = 12672\nCF = 1.460")))

points(era5_daily$v, final_adj_Palmer$V_WD_avg_2min, col="black",lwd=0.3, cex=1.5)

}
addfiglab("(f)")
par(mar=c(4,6,0.5,1)) 
# # pressure CF = 1.009
{
  lsf <- lm(final_adj_Palmer$Pressure ~ era5_daily$pressure)
  summary(lsf) # 
  
  cor.test(final_adj_Palmer$Pressure, era5_daily$pressure, method="spearman")
  # n=12781
  
  plot(era5_daily$pressure, final_adj_Palmer$Pressure,
       xlim = c(920,1039), ylim = c(920,1039),
       xlab=expression(bold("ERA5 SLP (hPa)")),
       ylab=expression(bold("adjusted Palmer SLP (hPa)")),
       col="transparent",
       lwd=0.3, cex=1.5, cex.axis=1.5, cex.lab=1.5
  )
  
  abline(v=c(920,940,960,980,1000,1020,1040), h=c(920,940,960,980,1000,1020,1040), lty = 9, col = "grey33", lwd = 0.75)
  abline(a=0,b=1, lty = 9, col="grey33")
  text(1000,925,expression(bold("ERA5—adjusted Palmer\nSpearman's ρ = 0.993\nn = 12741\nCF = 1.004")))
  
  points(era5_daily$pressure, final_adj_Palmer$Pressure, col="black",lwd=0.3, cex=1.5)
  
}
addfiglab("(g)")


#setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate")
#final_adj_Palmer <- read.csv("hourly_final_palmer_adjusted_NOT_GAP_FILLED.csv", header=TRUE)
colnames(final_adj_Palmer)
final_adj_Palmer <- final_adj_Palmer[,-1]

# rownames(era5_hourly) <- c(1:196488)

# ∆ REPLACE MIssING PALMER WITH ERA5 - HOURLY #####
{# JAN 2002
  {
    
    # missing 5 days
    # Jan 27-31 2002
    final_adj_Palmer[1346,] # 2002-01-27
    final_adj_Palmer[1465,] # 2002-01-31
    
    # replace with ERA5 daily values for relative humidity (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[1369,] # 2002-01-27
    era5_hourly[1488,] # 2002-01-31
    plot(era5_hourly$rel_humidity[1369:1488], type="l")
     
    final_adj_Palmer[c(1346:1465),11] = era5_hourly[c(1369:1488),4] * 0.832
    plot(final_adj_Palmer$rel_humidity[1346:1465], type="l")
  }
  
  # APR 2006
  {
    # missing 7 days
    # Apr 6-12 2006
    final_adj_Palmer[37934,] # 2006-04-06
    final_adj_Palmer[38088,] # 2006-04-12

    # replace with ERA5 daily values for relative humidity (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[38089,] # 2006-04-06
    era5_hourly[38243,] # 2006-04-12 10:00:00 UTC
    plot(era5_hourly$rel_humidity[38089:38243], type="l")
    # adjust the APR daily values by the MAM mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(37934:38088),11] = era5_hourly[c(38089:38243),4] * 0.832
    plot(final_adj_Palmer$rel_humidity[38089:38243], type="l")
  }
  
  # NOV 2019
  {
    # missing 10 days
    # Nov 21-30 2019
    final_adj_Palmer[156996,] # 2019-11-21
    final_adj_Palmer[157235,] # 2019-11-30
    
    # replace with ERA5 daily values for relative humidity (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[157537,] # 2019-11-21
    era5_hourly[157776,] # 2019-11-30
    plot(era5_hourly$rel_humidity[157537:157776], type="l")
    # adjust the NOV daily values by the SON mean difference between
    # era5 and Palmer; this means adding 2.2% to relative humidity bc eRA5 is larger by 2.2% in SON
    final_adj_Palmer[c(156996:157235),11] = era5_hourly[c(157537:157776),4] * 0.832
    plot(final_adj_Palmer$rel_humidity[156996:157235], type="l")
  }
  
  #  PRESSURE replaced w/ ERA5 #####
  # APR 2006 
  {
    # missing 7 days
    # Apr 6-12 2006
    final_adj_Palmer[37934,] # 2006-04-06
    final_adj_Palmer[38088,] # 2006-04-12
    
    # replace with ERA5 daily values for temperature (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[38089,] # 2006-04-06
    era5_hourly[38243,] # 2006-04-12
    plot(era5_hourly$temperature[38089:38243], type="l")
    # adjust the APR daily values by the MAM mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(37934:38088),12] = era5_hourly[c(38089:38243),8] * 1.025
    plot(final_adj_Palmer$Pressure[37934:38088], type="l")
  }
  
  # NOV 2019 
  {
    # missing 10 days
    # Nov 21-30 2019
    final_adj_Palmer[156996,] # 2019-11-21
    final_adj_Palmer[157235,] # 2019-11-30
    
    # replace with ERA5 daily values for temperature (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[157537,] # 2019-11-21
    era5_hourly[157776,] # 2019-11-30
    plot(era5_hourly$pressure[157537:157776], type="l")
    # adjust the NOV daily values by the SON mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(156996:157235),12] = era5_hourly[c(157537:157776),8] * 1.025
    plot(final_adj_Palmer$Pressure[156996:157235], type="l")
  }
  
  
  #  TEMPERATURE replaced w/ ERA5 #####
  # JAN 2002 
  {
    # missing 5 days
    # Jan 27-31 2002
    final_adj_Palmer[1346,] # 2002-01-27
    final_adj_Palmer[1465,] # 2002-01-31
    
    # replace with ERA5 daily values for temperature (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[1369,] # 2002-01-27
    era5_hourly[1488,] # 2002-01-31
    plot(era5_hourly$temperature[1369:1488], type="l")
    # adjust the JAN daily values by the DJF mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(1346:1465),6] = era5_hourly[c(1369:1488),2] * 0.914
    plot(final_adj_Palmer$temperature[1346:1465], type="l")
  }
  
  # APR 2006 
  {
    # missing 7 days
    # Apr 6-12 2006
    final_adj_Palmer[37934,] # 2006-04-06
    final_adj_Palmer[38088,] # 2006-04-12
    
    # replace with ERA5 daily values for temperature (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[38089,] # 2006-04-06
    era5_hourly[38243,] # 2006-04-12
    plot(era5_hourly$temperature[38089:38243], type="l")
    # adjust the APR daily values by the MAM mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(37934:38088),6] = era5_hourly[c(38089:38243),2] * 0.914
    plot(final_adj_Palmer$temperature[37934:38088], type="l")
  }
  
  # NOV 2019 
  {
    # missing 10 days
    # Nov 21-30 2019
    final_adj_Palmer[156996,] # 2019-11-21
    final_adj_Palmer[157235,] # 2019-11-30
    
    # replace with ERA5 daily values for temperature (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[157537,] # 2019-11-21
    era5_hourly[157776,] # 2019-11-30
    plot(era5_hourly$temperature[157537:157776], type="l")
    # adjust the NOV daily values by the SON mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(156996:157235),6] = era5_hourly[c(157537:157776),2] * 0.914
    plot(final_adj_Palmer$temperature[156996:157235], type="l")
  }
  
  #  PRECIPITATION replaced w/ ERA5 #####
  # JAN 2002 == 
  {
    # missing 5 days
    # Jan 27-31 2002
    final_adj_Palmer[1346,] # 2002-01-27
    final_adj_Palmer[1465,] # 2002-01-31
    
    # replace with ERA5 daily values (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[1369,] # 2002-01-27
    era5_hourly[1488,] # 2002-01-31
    plot(era5_hourly$tot_precip[1369:1488], type="l")
    # 
    final_adj_Palmer[c(1346:1465),7] = era5_hourly[c(1369:1488),3] * 0.562
    
  }
  
  # DO NOT RUN
  # NOT MISSING
  # JUL 2002 == 
  {
    # missing 7 days
    # Jul -- 12,13,16,17,25,26,30 -- 2002
    final_adj_Palmer[5313,] # 07/12/2002
    final_adj_Palmer[5360,] # 07/13/2002
    
    final_adj_Palmer[5409,] # 07/16/2002
    final_adj_Palmer[5456,] # 07/17/2002
    
    final_adj_Palmer[5625,] # 07/25/2002
    final_adj_Palmer[5672,] # 07/26/2002
    
    final_adj_Palmer[5724,] # 07/30/2002
    final_adj_Palmer[5747,] # 07/30/2002 
    
    era5_hourly[5353,] # 07/12/2002
    era5_hourly[5400,] # 07/13/2002
    
    era5_hourly[5449,] # 07/16/2002
    era5_hourly[5496,] # 07/17/2002
    
    era5_hourly[5665,] # 07/25/2002
    era5_hourly[5712,] # 07/26/2002
    
    era5_hourly[5785,] # 07/30/2002
    era5_hourly[5808,] # 07/30/2002    
   
    plot(era5_hourly$tot_precip[5353:5808], type="l")
    
    # to precip bc eRA5 is larger by 1.8ºC in DJF
    final_adj_Palmer[c(5313:5360, 5409:5456, 5625:5672, 5724:5747),7] = era5_hourly[c(5353:5400, 5449:5496, 5665:5712, 5785:5808),3] * 0.562
    
  }
  # NOT MISSING
  # SEP 2002 == 
  {
    # missing 6 days
    # Sep -- 3,6,12,18,20,26 -- 2002
    final_adj_Palmer[6564:6587,] # 09/03/2002 not missing
    final_adj_Palmer[6636:6659,] # 09/06/2002 not missing
    final_adj_Palmer[6780:6803,] # 09/12/2002 not missing
    final_adj_Palmer[4919,] # 09/18/2002 not missing
    final_adj_Palmer[4921,] # 09/20/2002 not missing
    final_adj_Palmer[4927,] # 09/26/2002  not missing
    
  
    
    final_adj_Palmer[c(4904, 4907, 4913, 4919, 4921, 4927),7] = era5_daily[c(4904, 4907, 4913, 4919, 4921, 4927),3] * 0.562
    
  }
  # NOT MISSING
  # DEC 2002 == 
  {
    # missing 6 days
    # Sep -- 6,10,12,20,28,29 -- 2002
    final_adj_Palmer[4998,] # 12/06/2002
    final_adj_Palmer[5002,] # 12/10/2002
    final_adj_Palmer[5004,] # 12/12/2002
    final_adj_Palmer[5012,] # 12/20/2002
    final_adj_Palmer[5020,] # 12/28/2002
    final_adj_Palmer[5021,] # 12/29/2002
    
    # adjust the DEC daily values by the DJF mean difference between
    # era5 and Palmer; this means dividing 154 mm by 89 days in DJF == 1.7 mm
    # 154/89 # == 1.7 mm per day that should be subtracted from daily PALMER
    
    final_adj_Palmer[c(4998, 5002, 5004, 5012, 5020, 5021),6] = era5_daily[c(4998, 5002, 5004, 5012, 5020, 5021),3] * 0.562
    
  }
  # DO NOT RUN
  
  
  # APR 2006 == 
  {
    # missing 7 days
    # APR -- 6 to 12 -- 2006
    final_adj_Palmer[37934,] # 04/06/2002
    final_adj_Palmer[38088,] # 04/12/2002
    
    final_adj_Palmer[c(37934:38088),7] = era5_hourly[c(38089:38243),3] * 0.562
    
  }
  
  # OCT 2009 == 
  {
    # missing 7 days
    # OCT -- 9 to 31 -- 2009
    final_adj_Palmer[68577,] # 10/09/2009
    final_adj_Palmer[69128,] # 10/31/2009
    
    final_adj_Palmer[c(68577:69128),7] = era5_hourly[c(68857:69408),3] * 0.562
    
  }
  
  # NOV 2019 == 
  {
    # missing 10 days
    # NOV -- 21 to 30 -- 2019
    final_adj_Palmer[156996,] # 11/21/2019
    final_adj_Palmer[157235,] # 11/30/2019
    
    # adjust the NOV daily values by the SON mean difference between
    # era5 and Palmer; this means dividing 304 mm by 91 days in SON == 3.3 mm
    # 304/91 # == 3.3 mm per day that should be subtracted from daily PALMER
    final_adj_Palmer[c(156996:157235),7] = era5_hourly[c(157537:157776),3] * 0.562
    
  }
  
  # replace negative values with zeros  !!!!!!
  #final_adj_Palmer$precip_melted <- ifelse(final_adj_Palmer$precip_melted < 0, 0, final_adj_Palmer$precip_melted)
  
  #  Wind speed replaced w/ ERA5 #####
  # JAN 2002 == 
  {
    # missing 5 days
    # Jan 27-31 2002
    final_adj_Palmer[1346,] # 2002-01-27
    final_adj_Palmer[1465,] # 2002-01-31
    
    # replace with ERA5 daily values (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[1369,] # 2002-01-27
    era5_hourly[1488,] # 2002-01-31
    plot(era5_hourly$windspeed[1369:1488], type="l")
    # adjust the JAN daily values by the DJF mean difference between
    # era5 and Palmer; this means adding 1.2 m/s to windspeed bc eRA5 is smaller by 1.2 m/s in DJF
    final_adj_Palmer[c(1346:1465),8] = era5_hourly[c(1369:1488),7] * 1.101
  }
  
  # APR 2006 == 
  {
    # missing 7 days
    # Apr 6-12 2006
    final_adj_Palmer[37934,] # 2006-04-06
    final_adj_Palmer[38088,] # 2006-04-12
    
    # replace with ERA5 daily values for temperature (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[38089,] # 2006-04-06
    era5_hourly[38243,] # 2006-04-12
    plot(era5_hourly$windspeed[38089:38243], type="l")
    # adjust the APR daily values by the MAM mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(37934:38088),8] = era5_hourly[c(38089:38243),7] * 1.101
    
  }
  
  # MAY 2016 ==
  # missing 9 days
  # May -- 8,9,10,14,15,17,18,19,27 -- 2016
  {
    final_adj_Palmer[126114,] # 2016-05-8
    #final_adj_Palmer[9901,] #  2016-05-9
    final_adj_Palmer[126185,] # end of 2016-05-10
    
    final_adj_Palmer[126258,] # 2016-05-14
    final_adj_Palmer[126305,] # end of 2016-05-15
    
    final_adj_Palmer[126330,] # 2016-05-17
    #final_adj_Palmer[9910,] # 2016-05-18
    final_adj_Palmer[126401,] # end of 2016-05-19
    
    final_adj_Palmer[126570:126593,] # 2016-05-27
    
    # replace with ERA5 daily values for temperature (adjusted)
    era5_hourly[126529,] # 2016-05-8
    #final_adj_Palmer[9901,] #  2016-05-9
    era5_hourly[126600,] # end of 2016-05-10
    
    era5_hourly[126673,] # 2016-05-14
    era5_hourly[126720,] # end of 2016-05-15
    
    era5_hourly[126745,] # 2016-05-17
    #final_adj_Palmer[9910,] # 2016-05-18
    era5_hourly[126816,] # end of 2016-05-19
    
    era5_hourly[126985:127008,] # 2016-05-27
    # era5 and Palmer; 
    final_adj_Palmer[c(126114:126185, 126258:126305, 126330:126401, 126570:126593),8] = era5_hourly[c(126529:126600, 126673:126720, 126745:126816,126985:127008),7] * 1.101
  }
  
  # NOV 2019 == 
  {
    # missing 10 days
    # Nov 21-30 2019
    final_adj_Palmer[156996,] # 2019-11-21
    final_adj_Palmer[157235,] # 2019-11-30
    
    # replace with ERA5 daily values for temperature (adjusted)
    # this data frame comes from Palmer Climate adjusted data analyses
    era5_hourly[157537,] # 2019-11-21
    era5_hourly[157776,] # 2019-11-30
    plot(era5_hourly$temperature[157537:157776], type="l")
    # adjust the NOV daily values by the SON mean difference between
    # era5 and Palmer; 
    final_adj_Palmer[c(156996:157235),8] = era5_hourly[c(157537:157776),7] * 1.005
    
  }
  
  #  U wind component replaced w/ ERA5 #####
  # JAN 2002 == 
  {
    # missing 5 days
    # Jan 27-31 2002
    final_adj_Palmer[1346,] # 2002-01-27
    final_adj_Palmer[1465,] # 2002-01-31
    
    final_adj_Palmer[c(1346:1465),9] = era5_hourly[c(1369:1488),6] * 0.604
  }
  
  # APR 2006 == 
  {
    # missing 7 days
    # Apr 6-12 2006
    final_adj_Palmer[37934,] # 2006-04-06
    final_adj_Palmer[38088,] # 2006-04-12
    
    final_adj_Palmer[c(37934:38088),9] = era5_hourly[c(38089:38243),6] * 0.604
    
  }
  
  # NOV 2019 == 
  {
    # missing 10 days
    # Nov 21-30 2019
    final_adj_Palmer[156996,] # 2019-11-21
    final_adj_Palmer[157235,] # 2019-11-30
    
    final_adj_Palmer[c(156996:157235),9] = era5_hourly[c(157537:157776),6] * 0.604
    
  }
  
  #  V wind component replaced w/ ERA5 #####
  # JAN 2002 == 
  {
    # missing 5 days
    # Jan 27-31 2002
    final_adj_Palmer[1346,] # 2002-01-27
    final_adj_Palmer[1465,] # 2002-01-31
    
    final_adj_Palmer[c(1346:1465),10] = era5_hourly[c(1369:1488),5] * 1.349
  }
  
  # APR 2006 == 
  {
    # missing 7 days
    # Apr 6-12 2006
    final_adj_Palmer[37934,] # 2006-04-06
    final_adj_Palmer[38088,] # 2006-04-12
    
    final_adj_Palmer[c(37934:38088),10] = era5_hourly[c(38089:38243),5] * 1.349
    
  }
  
  # NOV 2019 == 
  {
    # missing 10 days
    # Nov 21-30 2019
    final_adj_Palmer[156996,] # 2019-11-21
    final_adj_Palmer[157235,] # 2019-11-30
    
    final_adj_Palmer[c(156996:157235),10] = era5_hourly[c(157537:157776),5] * 1.349
    
  }
  
}




# ∆ REPLACE MIssING PALMER WITH ERA5 - DAILY #####
{# JAN 2002
  
{
   
  # missing 5 days
  # Jan 27-31 2002
  final_adj_Palmer[4685,] # 2002-01-27
  final_adj_Palmer[4689,] # 2002-01-31
  
  # replace with ERA5 daily values for relative humidity (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[4685,] # 2002-01-27
  era5_daily[4689,] # 2002-01-31
  plot(era5_daily$rel_humidity[4685:4689], type="l")
  # adjust the JAN daily values by the DJF mean difference between
  # era5 and Palmer; 
  
  final_adj_Palmer[c(4685:4689),10] = era5_daily[c(4685:4689),4] * 0.832
  plot(final_adj_Palmer$rel_humidity[4685:4689], type="l")
}

# APR 2006
{
  # missing 7 days
  # Apr 6-12 2006
  final_adj_Palmer[6215,] # 2006-04-06
  final_adj_Palmer[6221,] # 2006-04-12
  
  # replace with ERA5 daily values for relative humidity (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[6215,] # 2006-04-06
  era5_daily[6221,] # 2006-04-12
  plot(era5_daily$rel_humidity[6215:6221], type="l")
  # adjust the APR daily values by the MAM mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(6215:6221),10] = era5_daily[c(6215:6221),4] * 0.832
  plot(final_adj_Palmer$rel_humidity[6215:6221], type="l")
}

# NOV 2019
{
  # missing 10 days
  # Nov 21-30 2019
  final_adj_Palmer[11192,] # 2019-11-21
  final_adj_Palmer[11201,] # 2019-11-30
  
  # replace with ERA5 daily values for relative humidity (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[11192,] # 2019-11-21
  era5_daily[11201,] # 2019-11-30
  plot(era5_daily$rel_humidity[11192:11201], type="l")
  # adjust the NOV daily values by the SON mean difference between
  # era5 and Palmer; this means adding 2.2% to relative humidity bc eRA5 is larger by 2.2% in SON
  final_adj_Palmer[c(11192:11201),10] = era5_daily[c(11192:11201),4] * 0.832
  plot(final_adj_Palmer$rel_humidity[11192:11201], type="l")
}

#  PRESSURE replaced w/ ERA5 #####
# APR 2006 
{
  # missing 7 days
  # Apr 6-12 2006
  final_adj_Palmer[6215,] # 2006-04-06
  final_adj_Palmer[6221,] # 2006-04-12
  
  # replace with ERA5 daily values for temperature (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[6215,] # 2006-04-06
  era5_daily[6221,] # 2006-04-12
  plot(era5_daily$temperature[6215:6221], type="l")
  # adjust the APR daily values by the MAM mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(6215:6221),11] = era5_daily[c(6215:6221),8] * 1.009
  plot(final_adj_Palmer$Pressure[6215:6221], type="l")
}

# NOV 2019 
{
  # missing 10 days
  # Nov 21-30 2019
  final_adj_Palmer[11192,] # 2019-11-21
  final_adj_Palmer[11201,] # 2019-11-30
  
  # replace with ERA5 daily values for temperature (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[11192,] # 2019-11-21
  era5_daily[11201,] # 2019-11-30
  plot(era5_daily$pressure[11192:11201], type="l")
  # adjust the NOV daily values by the SON mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(11192:11201),11] = era5_daily[c(11192:11201),8] * 1.009
  plot(final_adj_Palmer$Pressure[11192:11201], type="l")
}


#  TEMPERATURE replaced w/ ERA5 #####
# JAN 2002 
{
  # missing 5 days
  # Jan 27-31 2002
  final_adj_Palmer[4685,] # 2002-01-27
  final_adj_Palmer[4689,] # 2002-01-31
  
  # replace with ERA5 daily values for temperature (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[4685,] # 2002-01-27
  era5_daily[4689,] # 2002-01-31
  plot(era5_daily$temperature[4685:4689], type="l")
  # adjust the JAN daily values by the DJF mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(4685:4689),5] = era5_daily[c(4685:4689),2] * 0.914
  plot(final_adj_Palmer$temperature[4685:4689], type="l")
}

# APR 2006 
{
  # missing 7 days
  # Apr 6-12 2006
  final_adj_Palmer[6215,] # 2006-04-06
  final_adj_Palmer[6221,] # 2006-04-12
  
  # replace with ERA5 daily values for temperature (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[6215,] # 2006-04-06
  era5_daily[6221,] # 2006-04-12
  plot(era5_daily$temperature[6215:6221], type="l")
  # adjust the APR daily values by the MAM mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(6215:6221),5] = era5_daily[c(6215:6221),2] * 0.914
  plot(final_adj_Palmer$temperature[6215:6221], type="l")
}

# NOV 2019 
{
  # missing 10 days
  # Nov 21-30 2019
  final_adj_Palmer[11192,] # 2019-11-21
  final_adj_Palmer[11201,] # 2019-11-30
  
  # replace with ERA5 daily values for temperature (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[11192,] # 2019-11-21
  era5_daily[11201,] # 2019-11-30
  plot(era5_daily$temperature[11192:11201], type="l")
  # adjust the NOV daily values by the SON mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(11192:11201),5] = era5_daily[c(11192:11201),2] * 0.914
  plot(final_adj_Palmer$temperature[11192:11201], type="l")
}

#  PRECIPITATION replaced w/ ERA5 #####
# JAN 2002 == 
{
  # missing 5 days
  # Jan 27-31 2002
  final_adj_Palmer[4685,] # 2002-01-27
  final_adj_Palmer[4689,] # 2002-01-31
  
  # replace with ERA5 daily values (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[4685,] # 2002-01-27
  era5_daily[4689,] # 2002-01-31
  plot(era5_daily$tot_precip[4685:4689], type="l")
  # adjust the JAN daily values by the DJF mean difference between
  # era5 and Palmer; this means dividing 154mm by 89 days in DJF == 1.7 mm
  # 154/89 # == 1.7 mm per day that should be subracted from daily PALMER
  
  # to precip bc eRA5 is larger by 1.8ºC in DJF
  final_adj_Palmer[c(4685:4689),6] = era5_daily[c(4685:4689),3] * 0.562
  
}

# JUL 2002 == 
{
  # missing 7 days
  # Jul -- 12,13,16,17,25,26,30 -- 2002
  final_adj_Palmer[4851,] # 07/12/2002
  final_adj_Palmer[4852,] # 07/13/2002
  final_adj_Palmer[4855,] # 07/16/2002
  final_adj_Palmer[4856,] # 07/17/2002
  final_adj_Palmer[4864,] # 07/25/2002
  final_adj_Palmer[4865,] # 07/26/2002
  final_adj_Palmer[4869,] # 07/30/2002
  
  # replace with ERA5 daily values (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  #era5_daily[4685,] # 
  #era5_daily[4689,] # 
  plot(era5_daily$tot_precip[4685:4689], type="l")
  # adjust the JUL daily values by the JJA mean difference between
  # era5 and Palmer; this means dividing 283mm by 92 days in JJA == 3.1 mm
  #283/92 # == 3.1 mm per day that should be subtracted from daily PALMER
  
  # to precip bc eRA5 is larger by 1.8ºC in DJF
  final_adj_Palmer[c(4851, 4852, 4855, 4856, 4864, 4865, 4869),6] = era5_daily[c(4851, 4852, 4855, 4856, 4864, 4865, 4869),3] * 0.562
  
}

# SEP 2002 == 
{
  # missing 6 days
  # Sep -- 3,6,12,18,20,26 -- 2002
  final_adj_Palmer[4904,] # 09/03/2002
  final_adj_Palmer[4907,] # 09/06/2002
  final_adj_Palmer[4913,] # 09/12/2002
  final_adj_Palmer[4919,] # 09/18/2002
  final_adj_Palmer[4921,] # 09/20/2002
  final_adj_Palmer[4927,] # 09/26/2002


  # adjust the SEP daily values by the SON mean difference between
  # era5 and Palmer; this means dividing 304 mm by 91 days in SON == 3.3 mm
  # 304/91 # == 3.3 mm per day that should be subtracted from daily PALMER
  
  # to precip bc eRA5 is larger by 1.8ºC in DJF
  final_adj_Palmer[c(4904, 4907, 4913, 4919, 4921, 4927),6] = era5_daily[c(4904, 4907, 4913, 4919, 4921, 4927),3] * 0.562
  
}

# DEC 2002 == 
{
  # missing 6 days
  # Sep -- 6,10,12,20,28,29 -- 2002
  final_adj_Palmer[4998,] # 12/06/2002
  final_adj_Palmer[5002,] # 12/10/2002
  final_adj_Palmer[5004,] # 12/12/2002
  final_adj_Palmer[5012,] # 12/20/2002
  final_adj_Palmer[5020,] # 12/28/2002
  final_adj_Palmer[5021,] # 12/29/2002
  
  # adjust the DEC daily values by the DJF mean difference between
  # era5 and Palmer; this means dividing 154 mm by 89 days in DJF == 1.7 mm
  # 154/89 # == 1.7 mm per day that should be subtracted from daily PALMER
  
  final_adj_Palmer[c(4998, 5002, 5004, 5012, 5020, 5021),6] = era5_daily[c(4998, 5002, 5004, 5012, 5020, 5021),3] * 0.562
  
}

# APR 2006 == 
{
  # missing 7 days
  # APR -- 6 to 12 -- 2006
  final_adj_Palmer[6215,] # 04/06/2002
  final_adj_Palmer[6221,] # 04/12/2002

  # adjust the APR daily values by the MAM mean difference between
  # era5 and Palmer; this means dividing 223 mm by 92 days in MAM == 2.4 mm
  # 223/92 # == 2.4 mm per day that should be subtracted from daily PALMER
  final_adj_Palmer[c(6215:6221),6] = era5_daily[c(6215:6221),3] * 0.562
  
}

# OCT 2009 == 
{
  # missing 7 days
  # OCT -- 9 to 31 -- 2009
  final_adj_Palmer[7497,] # 10/09/2009
  final_adj_Palmer[7519,] # 10/31/2009
  
  # adjust the OCT daily values by the SON mean difference between
  # era5 and Palmer; this means dividing 304 mm by 91 days in SON == 3.3 mm
  # 304/91 # == 3.3 mm per day that should be subtracted from daily PALMER
  final_adj_Palmer[c(7497:7519),6] = era5_daily[c(7497:7519),3] * 0.562
  
}

# NOV 2019 == 
{
  # missing 10 days
  # NOV -- 21 to 30 -- 2019
  final_adj_Palmer[11192,] # 11/21/2019
  final_adj_Palmer[11201,] # 11/30/2019
  
  # adjust the NOV daily values by the SON mean difference between
  # era5 and Palmer; this means dividing 304 mm by 91 days in SON == 3.3 mm
  # 304/91 # == 3.3 mm per day that should be subtracted from daily PALMER
  final_adj_Palmer[c(11192:11201),6] = era5_daily[c(11192:11201),3] * 0.562
  
}

# replace negative values with zeros  !!!!!!
#final_adj_Palmer$precip_melted <- ifelse(final_adj_Palmer$precip_melted < 0, 0, final_adj_Palmer$precip_melted)

#  Wind speed replaced w/ ERA5 #####
# JAN 2002 == 
{
  # missing 5 days
  # Jan 27-31 2002
  final_adj_Palmer[4685,] # 2002-01-27
  final_adj_Palmer[4689,] # 2002-01-31
  
  # replace with ERA5 daily values (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[4685,] # 2002-01-27
  era5_daily[4689,] # 2002-01-31
  plot(era5_daily$windspeed[4685:4689], type="l")
  # adjust the JAN daily values by the DJF mean difference between
  # era5 and Palmer; this means adding 1.2 m/s to windspeed bc eRA5 is smaller by 1.2 m/s in DJF
  final_adj_Palmer[c(4685:4689),7] = era5_daily[c(4685:4689),7] * 1.101
}

# APR 2006 == 
{
  # missing 7 days
  # Apr 6-12 2006
  final_adj_Palmer[6215,] # 2006-04-06
  final_adj_Palmer[6221,] # 2006-04-12
  
  # replace with ERA5 daily values for temperature (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[6215,] # 2006-04-06
  era5_daily[6221,] # 2006-04-12
  plot(era5_daily$windspeed[6215:6221], type="l")
  # adjust the APR daily values by the MAM mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(6215:6221),7] = era5_daily[c(6215:6221),7] * 1.101
  
}

# MAY 2016 ==
# missing 9 days
# May -- 8,9,10,14,15,17,18,19,27 -- 2016
{
  final_adj_Palmer[9900,] # 2016-05-8
  final_adj_Palmer[9901,] # 2016-05-9
  final_adj_Palmer[9902,] # 2016-05-10
  final_adj_Palmer[9906,] # 2016-05-14
  final_adj_Palmer[9907,] # 2016-05-15
  final_adj_Palmer[9909,] # 2016-05-17
  final_adj_Palmer[9910,] # 2016-05-18
  final_adj_Palmer[9911,] # 2016-05-19
  final_adj_Palmer[9919,] # 2016-05-27

# replace with ERA5 daily values for temperature (adjusted)
# this data frame comes from Palmer Climate adjusted data analyses
era5_daily[9900,] 
era5_daily[9919,] 
plot(era5_daily$windspeed[9900:9919], type="l")
# adjust the MAY daily values by the CF = 1.101
# era5 and Palmer; 
final_adj_Palmer[c(9900,9901,9902,9906,9907,9909,9910,9911,9919),7] = era5_daily[c(9900,9901,9902,9906,9907,9909,9910,9911,9919),7] * 1.101
}

# NOV 2019 == 
{
  # missing 10 days
  # Nov 21-30 2019
  final_adj_Palmer[11192,] # 2019-11-21
  final_adj_Palmer[11201,] # 2019-11-30
  
  # replace with ERA5 daily values for temperature (adjusted)
  # this data frame comes from Palmer Climate adjusted data analyses
  era5_daily[11192,] # 2019-11-21
  era5_daily[11201,] # 2019-11-30
  plot(era5_daily$temperature[11192:11201], type="l")
  # adjust the NOV daily values by the SON mean difference between
  # era5 and Palmer; 
  final_adj_Palmer[c(11192:11201),7] = era5_daily[c(11192:11201),7] * 1.005
  
}

#  U wind component replaced w/ ERA5 #####
# JUN 1994 == 
{
  # missing 4 days
  # Jun 5,6,8,11
  final_adj_Palmer[1892,] # 1994-06-05
  final_adj_Palmer[1893,] # 1994-06-06
  final_adj_Palmer[1895,] # 1994-06-08
  final_adj_Palmer[1898,] # 1994-06-11
  
  final_adj_Palmer[c(1892,1893,1895,1896),8] = era5_daily[c(1892,1893,1895,1896),6] * 0.604
}

# MAR 1995 == 
{
  # missing 25 days
  # Mar 3, 6-12, 14-25, 27-31
  final_adj_Palmer[2163,] # 1995-03-03
  final_adj_Palmer[2166,] # 1995-03-06 # start
  final_adj_Palmer[2172,] # 1995-03-12 # end
  final_adj_Palmer[2174,] # 1995-03-14 # start 
  final_adj_Palmer[2185,] # 1995-03-25 # end
  final_adj_Palmer[2187,] # 1995-03-27 # start 
  final_adj_Palmer[2191,] # 1995-03-31 # end
  
  final_adj_Palmer[c(2163,2166:2172,2174:2185,2187:2191),8] = era5_daily[c(2163,2166:2172,2174:2185,2187:2191),6] * 0.604
}

# JAN 2002 == 
{
  # missing 5 days
  # Jan 27-31 2002
  final_adj_Palmer[4685,] # 2002-01-27
  final_adj_Palmer[4689,] # 2002-01-31
  
  final_adj_Palmer[c(4685:4689),8] = era5_daily[c(4685:4689),6] * 0.604
}

# APR 2006 == 
{
  # missing 7 days
  # Apr 6-12 2006
  final_adj_Palmer[6215,] # 2006-04-06
  final_adj_Palmer[6221,] # 2006-04-12
  
  final_adj_Palmer[c(6215:6221),8] = era5_daily[c(6215:6221),6] * 0.604
  
}

# NOV 2019 == 
{
  # missing 10 days
  # Nov 21-30 2019
  final_adj_Palmer[11192,] # 2019-11-21
  final_adj_Palmer[11201,] # 2019-11-30
  
  final_adj_Palmer[c(11192:11201),8] = era5_daily[c(11192:11201),6] * 0.604
  
}

#  V wind component replaced w/ ERA5 #####
# JUN 1994 == 
{
  # missing 4 days
  # Jun 5,6,8,11
  final_adj_Palmer[1892,] # 1994-06-05
  final_adj_Palmer[1893,] # 1994-06-06
  final_adj_Palmer[1895,] # 1994-06-08
  final_adj_Palmer[1898,] # 1994-06-11
  
  final_adj_Palmer[c(1892,1893,1895,1896),9] = era5_daily[c(1892,1893,1895,1896),5] * 1.349
}

# MAR 1995 == 
{
  # missing 25 days
  # Mar 3, 6-12, 14-25, 27-31
  final_adj_Palmer[2163,] # 1995-03-03
  final_adj_Palmer[2166,] # 1995-03-06 # start
  final_adj_Palmer[2172,] # 1995-03-12 # end
  final_adj_Palmer[2174,] # 1995-03-14 # start 
  final_adj_Palmer[2185,] # 1995-03-25 # end
  final_adj_Palmer[2187,] # 1995-03-27 # start 
  final_adj_Palmer[2191,] # 1995-03-31 # end
  
  final_adj_Palmer[c(2163,2166:2172,2174:2185,2187:2191),9] = era5_daily[c(2163,2166:2172,2174:2185,2187:2191),5] * 1.349
}

# JAN 2002 == 
{
  # missing 5 days
  # Jan 27-31 2002
  final_adj_Palmer[4685,] # 2002-01-27
  final_adj_Palmer[4689,] # 2002-01-31
  
  final_adj_Palmer[c(4685:4689),9] = era5_daily[c(4685:4689),5] * 1.349
}

# APR 2006 == 
{
  # missing 7 days
  # Apr 6-12 2006
  final_adj_Palmer[6215,] # 2006-04-06
  final_adj_Palmer[6221,] # 2006-04-12
  
  final_adj_Palmer[c(6215:6221),9] = era5_daily[c(6215:6221),5] * 1.349
  
}

# NOV 2019 == 
{
  # missing 10 days
  # Nov 21-30 2019
  final_adj_Palmer[11192,] # 2019-11-21
  final_adj_Palmer[11201,] # 2019-11-30
  
  final_adj_Palmer[c(11192:11201),9] = era5_daily[c(11192:11201),5] * 1.349
  
}

}


#final_adj_Palmer <- final_adj_Palmer[1:12814,]
final_adj_Palmer[c(1927),7] = NA
final_adj_Palmer[c(46646),7] = NA
final_adj_Palmer[c(13815),7] = NA

# plot final_adj_Palmer #####
par(mfrow=c(4,2))
par(mar = c(3, 4.5, 0.5, 0.5)) #par(mar = c(bottom, left, top, right))

# Convert the character string to a Date object, specifying the input format
#final_adj_Palmer$date <- as.Date(final_adj_Palmer$date, format = "%Y/%m/%d")
final_adj_Palmer$date <-  as.POSIXct(final_adj_Palmer$check.dates, tz="UTC", format="%Y-%m-%d")

#final_adj_Palmer <- final_adj_Palmer[1:12764,]
plot(final_adj_Palmer$date, final_adj_Palmer$precip_melted, type="l",
     ylab="precipitation (mm)")
plot(final_adj_Palmer$date, final_adj_Palmer$temperature, type="l",
     ylab="air temperature (ºC)")
plot(final_adj_Palmer$date, final_adj_Palmer$WS_avg_2min, type="l",
     ylab="wind speed (m/s)")
plot(final_adj_Palmer$date, final_adj_Palmer$U_WD_avg_2min, type="l",
     ylab="u wind (m/s)")
plot(final_adj_Palmer$date, final_adj_Palmer$V_WD_avg_2min, type="l",
     ylab="v wind (m/s)")
plot(final_adj_Palmer$date, final_adj_Palmer$rel_humidity, type="l",
     ylab="relative humidity (%)")
plot(final_adj_Palmer$date, final_adj_Palmer$Pressure, type="l",
     ylab="air pressure (hPa)")

setwd("/Users/dulcineagroff/Desktop/Antarctica/PalmerSt_Journal of Climate")
#write.csv(daily.pal_overlap, "PalMOS_Data_Dec2001_Sep2003.csv")
head(final_adj_Palmer)
#write.csv(final_adj_Palmer, "Palmer Station un-adjusted observations 1989-2024.csv", col.names=TRUE)


final_adj_Palmer <- as.data.frame(final_adj_Palmer)
#write.csv(final_adj_Palmer, "Daily Palmer Station adjusted gap-filled observations 1989-2024.csv", col.names=TRUE)



