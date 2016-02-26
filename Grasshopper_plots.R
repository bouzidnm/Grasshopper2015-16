##########################
## Grasshopper Plotting ##
## Sima -- 06 Feb 2016  ##
##########################

library(ggplot2)
library(nlme)
library(gdata)

## Read in and clean data
data <- read.csv("GD_1st.csv", na.strings = c("", " "))
alive <- data[,-which(data[1,]=="Number dead")]
dead <- data[,-which(data[1,]=="Number alive")]
#write.csv(alive, "GD_1st_alive_only.csv")
#write.csv(dead, "GD_1st_dead_only.csv")

## Format data properly for number of individuals alive
  # I took some fields out in Excel
alive_edit <- read.csv("GD_1st_alive_only_edit.csv", na.strings=c("", " ", "NA"), stringsAsFactors = F)

## Re-format dates
Date_temp <- rep(colnames(alive_edit[,7:40]),length(alive_edit$Pod_ID)) 
Date <- sub("X", "", Date_temp)
Date <- strptime(Date, "%m.%d.%Y")

#alive_edit$Pod_ID[duplicated(alive_edit$Pod_ID)] # check for duplicates
Pod_ID <- vector()
Count <- vector()
alive_data <- t(alive_edit[,7:40])
#colnames(c("Pod_ID", "Date", "Count"))
for (i in 1:length(alive_edit$Pod_ID)){
  Pod_ID <- append(Pod_ID, rep(alive_edit$Pod_ID[i], length(alive_edit[,7:40])))
  Count <- append(Count, alive_data[,i])
}
alive_final <- cbind(Pod_ID, Date, Count)
#write.csv(alive_final, "GH_1st_alive_final.csv")

####################################
## Hatching & Survival
## For each treatment I will make a facets plot of # hatched by days since hatching
GH_data <- read.csv("GH_1st_alive_final.csv", stringsAsFactors = F, na.strings = c("", " ", "NA"))

T_24L <- subset(GH_data, Treatment=="24L"); T_24S <- subset(GH_data, Treatment=="24S"); 
T_28L <- subset(GH_data, Treatment=="28L"); T_28S <- subset(GH_data, Treatment=="28S");
T_20L <- subset(GH_data, Treatment=="20L"); T_20S <- subset(GH_data, Treatment=="20S")
## By treatment
qplot(Date, Count, data=T_28L, colour=Site) + geom_line(aes(group=Pod_ID)) + facet_grid(Species ~ .)
qplot(Date, Count, data=T_28S, colour=Site) + geom_line(aes(group=Pod_ID)) + facet_grid(Species ~ .)
qplot(Date, Count, data=T_24L, colour=Site) + geom_line(aes(group=Pod_ID)) + facet_grid(Species ~ .)
qplot(Date, Count, data=T_24S, colour=Site) + geom_line(aes(group=Pod_ID)) + facet_grid(Species ~ .)
## By species
date_sub <- c()
dodg <- subset(GH_data, Species == "dodgei" & Date !%in% "5/10/15")
qplot(Date, Count, data=dodg, colour=Site) + geom_line(aes(group=Pod_ID)) + facet_grid(Treatment ~ .)


####################################
## Growth
  # For species in each treatment I will plot mass by date to find mis-ID'd instars
  GH_ins <- read.csv("GrasshopperData_3rdToAdult_SPR15_edit.csv", na.strings = c("", " ", "NA"), stringsAsFactors=F, header=T)

# Reformat date columns
  col_nums <- colnames(GH_ins[,c(6, 9, 12, 15, 18, 21, 26)])
  dates <- GH_ins[,col_nums]
  #dates <- as.data.frame(apply(dates, 2, FUN=strptime, format="%m/%d/%Y"))
  GH_ins[,col_nums] <- dates
  #write.csv(GH_ins, "GH_3to5_reformat.csv")

## Added in the Pod.IDs so that i can add hatching time
  GH_ins2 <- read.csv("GH_3to5_reformat.csv", na.strings = c("", " ", "NA"), stringsAsFactors=F, header=T)
  GH_ins2[,col_nums] <- dates

  min_date <- vector()
  for (i in unique(GH_data$Pod_ID)){
    vec <- subset(GH_data$Date, Pod_ID==i & !is.na(Count))
    min_date <- c(min_date, min(vec))
  }
  hatch <- data.frame(Pod_ID=as.character(unique(GH_data$Pod_ID)), min_date)

  # Check that pod ID's are the same
  all(unique(GH_ins2$Pod_ID) == as.character(hatch$Pod_ID)) #no
unique(GH_ins2$Pod_ID)[as.character(hatch$Pod_ID)]
  '%nin%' <- Negate('%in%')
  mismatches <- unique(GH_ins2$Pod_ID)[unique(GH_ins2$Pod_ID) %nin% as.character(hatch$Pod_ID)] # # in unique(GH_ins2$Pod_ID) but not hatch$Pod_ID 
mismatches2 <- as.character(hatch$Pod_ID)[as.character(hatch$Pod_ID) %nin% unique(GH_ins2$Pod_ID)] # in hatch$Pod_ID but not unique(GH_ins2$Pod_ID)

  #GH_ins2 <- GH_ins2[c(GH_ins2$Pod_ID %nin% mismatches),] #got rid of the mismatches

  Date_Hatch <- vector()
  for (i in GH_ins2$Pod_ID){
    #print(i)
    if (i %in% as.character(hatch$Pod_ID)){
      temp <- as.character(hatch$min_date[which(hatch$Pod_ID==i)]) 
    } else {
      temp <- NA
    }
    Date_Hatch <- c(Date_Hatch, temp) ## Problem is here
  }
GH_ins2$Date_Hatch <- Date_Hatch

# Reformat dates, again
col_nums <- colnames(GH_ins2[,c(6, 9, 12, 15, 18, 21, 26, 31)])
dates <- GH_ins2[,col_nums]
dates <- as.data.frame(apply(dates, 2, FUN=strptime, format="%m/%d/%Y"))
GH_ins2[,col_nums] <- dates

## Add in days since hatching
GH_ins2$HD_2 <- with(GH_ins2, Date_2-Date_Hatch)
GH_ins2$HD_3 <- with(GH_ins2, Date_3-Date_Hatch)
GH_ins2$HD_4 <- with(GH_ins2, Date_4-Date_Hatch)
GH_ins2$HD_5 <- with(GH_ins2, Date_5-Date_Hatch)
GH_ins2$HD_5. <- with(GH_ins2, Date_5.-Date_Hatch)
GH_ins2$HD_Adult <- with(GH_ins2, Date_Adult-Date_Hatch)
GH_ins2$HD_Adult. <- with(GH_ins2, Date_Adult.-Date_Hatch)

T28S <- GH_ins2[GH_ins2$Treatment=="28S",]; T28L <- GH_ins2[GH_ins2$Treatment=="28L",];
T24S <- GH_ins2[GH_ins2$Treatment=="24S",]; T24L <- GH_ins2[GH_ins2$Treatment=="24L",]

crazy_dh <- function(df){
  d_hatching <- c(df$HD_2, df$HD_3, df$HD_4, df$HD_5, df$HD_5., df$HD_Adult, df$HD_Adult.)
  instars <- c(rep(2, nrow(df)), rep(3, nrow(df)), rep(4, nrow(df)), rep(5, nrow(df)), rep("5.", nrow(df)), rep("Adult", nrow(df)), rep("Adult.", nrow(df)))
  spp <- rep(df$Species, 7)
  instar_DH <- data.frame(instars, d_hatching, spp)
  return(instar_DH)
}

T28S_DH <- crazy_dh(T28S)
T28L_DH <- crazy_dh(T28L)
T24S_DH <- crazy_dh(T24S)
T24L_DH <- crazy_dh(T24L)

spp.col <- seq(1, length(unique(T28S_DH$spp)), by=1)

 plot.new()
par(mfrow=c(1,1))

plot(T28S_DH$instars, T28S_DH$d_hatching, col=spp.col, ylim=c(0, 60), main="28S")
plot(T28L_DH$instars, T28L_DH$d_hatching, col=spp.col, ylim=c(0, 60), main="28L")
plot(T24S_DH$instars, T24S_DH$d_hatching, col=spp.col, ylim=c(0, 60), main="24S")
plot(T24L_DH$instars, T24L_DH$d_hatching, col=spp.col, ylim=c(0, 60), main="24L")

#write.csv(GH_ins2, "GH_3rd_Adult_data_19Feb2016.csv")
data <- read.csv("GH_3rd_Adult_data_19Feb2016.csv",  na.strings = c("", " ", "NA"), stringsAsFactors=F, header=T)
exclude <- which(is.na(data$Species))
exclude <- c(exclude, data$Species[which(data$Species=="sang?")] | data$Species[which(data$Species=="pell?")])
data <- data[-c(),]


## Make a crazy df to replicate Lauren's plot
d_hatching <- c(GH_ins2$HD_2, GH_ins2$HD_3, GH_ins2$HD_4, GH_ins2$HD_5, GH_ins2$HD_5., GH_ins2$HD_Adult, GH_ins2$HD_Adult.)
instars <- c(rep(2, nrow(GH_ins2)), rep(3, nrow(GH_ins2)), rep(4, nrow(GH_ins2)), rep(5, nrow(GH_ins2)), rep("5.", nrow(GH_ins2)), rep("Adult", nrow(GH_ins2)), rep("Adult.", nrow(GH_ins2)))
instar_DH <- data.frame(instars, d_hatching)


## I refer to dodgei, sanguinipes as D and S (not enough pellucida or clavatus)
  T28S.D <- subset(GH_ins, Treatment=="28S" & Species == "dodg")

qplot(Date_2, Mass_2, data=T28S.D, colour=Species, na.rm=T)
+ geom_line(aes(group=Pod_ID)) + facet_grid(Species ~ .)

par(mfrow=c(1,3))

plot(T28S.D$Date_3, T28S.D$Mass_3, type="p", xlim=range(T28S.D[,c(9,12,15)], na.rm=T))
plot(T28S.D$Date_4, T28S.D$Mass_4, type="p", xlim=range(T28S.D$Date_3))
plot(T28S.D$Date_5, T28S.D$Mass_5, type="p", xlim=range(T28S.D$Date_3))

## Read in cleaned data
clean <- read.csv("GH_3rd_Adult_data_22Feb2016.csv", header=T, na.strings = c("", " ", "NA"), stringsAsFactors = FALSE)
#clean$HD_3 <- with(clean, as.Date(Date_3)-as.Date(Date_Hatch))
#write.csv(clean, "GH_3rd_Adult_data_22Feb2016.csv")

## Separate out by species
pell <- subset(clean, Species=="pell")
dodg <- subset(clean, Species=="dodg")
sang <- subset(clean, Species=="sang")

clean<-sang

## how many of each species per treatment? What are my n's?
T28S <- subset(clean, Treatment=="28S"); T28L <- subset(clean, Treatment=="28L"); T24S <- subset(clean, Treatment=="24S");
T24L <- subset(clean, Treatment=="24L")
nT28S <- table(T28S$Species); nT28L <- table(T28L$Species); nT24S <- table(T24S$Species); nT24L <- table(T24L$Species)
nT28S; nT28L; nT24S; nT24L
## how many of each species REACHED ADULTHOOD per treatment? What are my An's?
A_T28S <- subset(clean, Treatment=="28S" & !is.na(Date_Adult)); A_T28L <- subset(clean, Treatment=="28L" & !is.na(Date_Adult)); A_T24S <- subset(clean, Treatment=="24S" & !is.na(Date_Adult)); A_T24L <- subset(clean, Treatment=="24L" & !is.na(Date_Adult))
AnT28S <- table(A_T28S$Species); AnT28L <- table(A_T28L$Species); AnT24S <- table(A_T24S$Species); AnT24L <- table(A_T24L$Species)
AnT28S; AnT28L; AnT24S; AnT24L ## wow, not many. Dodgei were decimated in 28S (fungus)

#### From Lauren's paper

ltys= c("solid", "dashed", "dotted", "twodash")
#cols= c("gray80", "gray50", "black") 
#blue.pal <- colorRampPalette(c("darkblue", "lightblue"))
#cols= blue.pal(n=6)[1:6]
cols = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")
sites=c("A1", "B1", "C1", "RR", "RF", "KF")
elev_labs=c(2195, 2591, 3048,0,0,0) #I dont know the last 3 elevations
treats_lab=c("24L", "24S", "28L", "28S")

di= c(3,4,5,6)
time.dat=data.frame(clean[,c("Species", "Site", "Treatment", "HD_3","HD_4","HD_5","HD_Adult")])
time.dat <- data.frame(time.dat[,1:3], apply(time.dat[,4:7], MARGIN = 2, FUN=as.numeric))
time.dat.agg <- aggregate(time.dat, by=list(time.dat$Treatment,time.dat$Site, time.dat$Species), FUN=median, na.rm=TRUE)
time.dat.agg <- time.dat.agg[,c(1:3, 7:10)]
colnames(time.dat.agg)[1:3]= c("Treatment", "Site", "Species")
#time.dat.agg.se <- aggregate(time.dat, by=list(time.dat$Treatment,time.dat$Site, time.dat$Species), FUN=stderr) #problem

#dur.dat=clean[,c("Site", "Treatment", "dur.h3","dur.34","dur.45","dur.5a")] #come back to this
mass.dat=data.frame(clean[,c("Species", "Site", "Treatment", "Mass_3", "Mass_4", "Mass_5", "Mass_Adult")])
mass.dat <- data.frame(mass.dat[,1:3], apply(mass.dat[,4:7], MARGIN = 2, FUN=as.numeric))
mass.dat.agg <- aggregate(mass.dat, by=list(mass.dat$Treatment,mass.dat$Site, mass.dat$Species), FUN=median, na.rm=TRUE)
mass.dat.agg <- mass.dat.agg[,c(1:3, 7:10)]
colnames(mass.dat.agg)[1:3]= c("Treatment", "Site", "Species")

#time.dat.se=dat.agg.se[,c("Site", "Temp", "time.3","time.4","time.5","Hatch.Adult")]
#dur.dat.se=dat.agg.se[,c("Site", "Temp", "dur.h3","dur.34","dur.45","dur.5a")]
#mass.dat.se=dat.agg.se[,c("Site", "Temp", "Mass.3","Mass.4","Mass.5","Mass.A")]

#B. DEVELOPMENT TIME

par(mfrow=c(1,2), oma=c(2,0,2,0))
par(mfrow=c(1,2), lwd=2, mar=c(3, 3, 1, 0.5), mgp=c(1.3, 0.5, 0), oma=c(2,0,2,0))

for(r in 1:nrow(time.dat.agg)){
  col1= cols[match(time.dat.agg[r,"Site"], sites)]
  lty1=  ltys[match(time.dat.agg[r,"Treatment"], treats_lab)]
  
  if(r==1) plot(di, time.dat.agg[r,4:7], type="b", col=col1, lty=lty1, lwd=2, main="Timing", xlab="Development Stage",ylab="Days since hatching", ylim=range(time.dat.agg[,4:7],na.rm = TRUE) )
  if(r>1) points(di, time.dat.agg[r,4:7], type="b", col=col1, lty=lty1, lwd=2)
  
 # low= time.dat.agg[r,3:6]-time.dat.agg.se[r,3:6]
  #up= time.dat.agg[r,3:6]+time.dat.agg.se[r,3:6]
  #arrows(di, as.numeric(low), di, as.numeric(up), code=3, angle=90,length=0.03, col=col1)
}

#add legend
legend("topleft", treats_lab, lty=ltys, bty="n", lwd=2)
legend("bottomright", sites, col=cols, lty="solid", bty="n", lwd=2)

#DURATION IN INSTAR
#for(r in 1:nrow(dur.dat)){
#col1= cols[match(dur.dat[r,"Site"], sites)]
#lty1=  ltys[match(dur.dat[r,"Temp"], temps)]

#if(r==1) plot(di, dur.dat[r,3:6], type="b", col=col1, lty=lty1, xlab="Development Stage",ylab="Days in developmental stage", ylim=range(dur.dat[,3:6]) )
#if(r>1) points(di, dur.dat[r,3:6], type="b", col=col1, lty=lty1)

#low= dur.dat[r,3:6]-dur.dat.se[r,3:6]
#up= dur.dat[r,3:6]+dur.dat.se[r,3:6]
#arrows(di, as.numeric(low), di, as.numeric(up), code=3, angle=90,length=0.03, col=col1)
#}

#C. MASS
for(r in 1:nrow(mass.dat.agg)){
  col1= cols[match(time.dat.agg[r,"Site"], sites)]
  lty1=  ltys[match(time.dat.agg[r,"Treatment"], treats_lab)]
  
  if(r==1) plot(di, mass.dat.agg[r,4:7], type="b", col=col1, lty=lty1, lwd=2, main="Mass", xlab="Development Stage",ylab="Mass(g)", ylim=range(mass.dat.agg[,4:7],na.rm=T) )
  if(r>1) points(di, mass.dat.agg[r,4:7], type="b", col=col1, lty=lty1, lwd=2)
  
  #low= mass.dat.agg[r,3:6]-mass.dat.agg.se[r,3:6]
  #up= mass.dat.agg[r,3:6]+mass.dat.agg.se[r,3:6]
  #arrows(di, as.numeric(low), di, as.numeric(up), code=3, angle=90,length=0.03, col=col1)
}
title("Sanguinipes", outer=T)




