#################################
# palindrom day
# 02.02.2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
#################################



palindrome <- function(date){
  identical(date, paste(rev(strsplit(date, "")[[1]]), collapse=""))
  
}


dd <- data.frame(seq(as.Date("1000/01/01"), as.Date("2500/01/01"), "days"))

#empty df
df_EU <- data.frame(paldate=as.character())
df_US <- data.frame(paldate=as.character())


#loop through all the dates
for (i in 1:nrow(dd)){  
  dat <- dd[i,1]
#Year Month Day format
  #dat <- format(dat, "%m%d%Y")

  #Year  Day month format
  dat <- format(dat, "%d%m%Y")
  
  if (palindrome(dat) == TRUE) {
   df_EU <- rbind(df_EU, data.frame(paldate=as.character(dat)))
    
  }
    
}

#loop through all the dates
for (i in 1:nrow(dd)){  
  dat <- dd[i,1]
  #Year Month Day format
  dat <- format(dat, "%m%d%Y")
  
  #Year  Day month format
  #dat <- format(dat, "%Y%d%m")
  
  if (palindrome(dat) == TRUE) {
    df_US <- rbind(df_US, data.frame(paldate=as.character(dat)))
    
  }
  
}

#Check nof rows
nrow(df_EU)
nrow(df_US)


## Years distribution

head(df_EU)
df_EU$Year <- as.numeric(substring(df_EU$paldate,5,8))
df_EU$Month <- as.numeric(substring(df_EU$paldate,3,4))
df_EU$Day <- as.numeric(substring(df_EU$paldate,1,2))
df_EU$Region <- "EU"

head(df_US)
df_US$Year <- as.numeric(substring(df_US$paldate,5,8))
df_US$Month <- as.numeric(substring(df_US$paldate,1,2))
df_US$Day <- as.numeric(substring(df_US$paldate,3,4))
df_US$Region <- "US"


df_EU$lastDate <- as.Date(sprintf('%s-12-31',df_EU$Year))
df_US$lastDate <- as.Date(sprintf('%s-12-31',df_US$Year))
df_EU$TheDate <- as.Date(with(df_EU, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
df_US$TheDate <- as.Date(with(df_US, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

df_EU$DaysDiff <- difftime(df_EU$lastDate, df_EU$TheDate, units = "days")
df_US$DaysDiff <- difftime(df_US$lastDate, df_US$TheDate, units = "days")




# Merge dataset
df_ALL <- data.frame(rbind(df_EU, df_US))


library(ggplot2)
library(plyr)
df_ALL_m <- ddply(df_ALL, "Region", summarise, grp.mean=mean(Year))
head(df_ALL_m)

# Interleaved histograms
ggplot(df_ALL, aes(x=Year, color=Region)) +
  geom_histogram(fill="white", position="identity")+
  theme(legend.position="top")
# Add mean lines
p<-ggplot(df_ALL, aes(x=Year, color=Region)) +
  geom_histogram(fill="white", position="identity")+
  geom_vline(data=df_ALL_m, aes(xintercept=grp.mean, color=Region),
             linetype="dashed")+
  theme(legend.position="top")
p
  
df_ALL_m_m <- ddply(df_ALL, "Region", summarise, grp.mean=median(Month))

# Interleaved histograms
ggplot(df_ALL, aes(x=Month, color=Region)) +
  geom_histogram(fill="white", position="identity")+
  theme(legend.position="top")
# Add mean lines
pm<-ggplot(df_ALL, aes(x=Month, color=Region)) +
  geom_histogram(fill="white", position="identity")+
  geom_vline(data=df_ALL_m_m, aes(xintercept=grp.mean, color=Region),
             linetype="dashed")+
  theme(legend.position="top")
pm




df_ALL_m_d <- ddply(df_ALL, "Region", summarise, grp.mean=median(Day))

# Interleaved histograms
ggplot(df_ALL, aes(x=Day, color=Region, fill=Region)) +
  geom_histogram(fill="white", position="identity")+
  theme(legend.position="top")
# Add mean lines
pd<-ggplot(df_ALL, aes(x=Day, color=Region,fill=Region)) +
  geom_histogram(fill="white", position="identity", binwidth = 2)+
  geom_vline(data=df_ALL_m_d, aes(xintercept=grp.mean, color=Region),
             linetype="dashed")+
  theme(legend.position="top")
pd

library(gridExtra)

grid.arrange(p, pm, pd, ncol = 1)


## Get distribution for the days in the year (13th day of the year, 241st day of the year, etc)

v <- ggplot(df_ALL, aes(y=DaysDiff, x=Region)) + geom_violin(trim=FALSE)  + geom_dotplot(binaxis='DaysDiff', stackdir='center', dotsize=1)

b <- ggplot(df_ALL, aes(x=Region, y=DaysDiff)) +  geom_boxplot()  
b <- b  + geom_jitter(shape=16, position=position_jitter(0.2))


grid.arrange(v, b, nrow = 1)
