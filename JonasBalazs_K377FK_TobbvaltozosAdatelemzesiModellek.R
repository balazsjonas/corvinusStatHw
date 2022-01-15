# Project:  Többváltozós adatelemzési módszerek
# Szerző:   Jónás Balázs


# Előkészületek ####

## Szükséges packagek telepítése ====
install.packages("readxl")
install.packages("RcmdrMisc")
install.packages("tidyr") # 
install.packages("psych") # csúcsosság, ferdeség
install.packages("rcompanion")
install.packages("ggplot2")

## szükséges packagek betöltése
library(readxl)
library(tidyr)
library(RcmdrMisc)
library(psych)
library(rcompanion)
library(ggplot2)
## Adatok betöltése és tisztítása ====
auctions <- read_excel("AUCTION_220110173944.xlsx")
auctions <- as.data.frame(auctions)
auctions$year <- as.numeric(format(auctions$Date, format="%Y"))
periodLength <- 10
periodStarts <- seq(1990, 2030, periodLength)
auctions$period <- .bincode(auctions$year, periodStarts, right=F)
auctions$PeriodStart  <- as.factor(periodStarts[auctions$period])
plot(auctions$year, auctions$PeriodStart) # check

# 1 éves lejáratra kétféle jelölést használtak
auctions$Tenor[auctions$Tenor=="12M"]<-"1Y"
auctions$Tenor<-as.factor(auctions$Tenor)

# Új kibocsátások szűrése
auctions<-auctions[auctions$Type=="ISSUE",]  ## TODO REMOVE
issues<-auctions[auctions$Type=="ISSUE",]


# cserepapírokra nem lesz szükség
# auctions <- auctions[1:21]
# issues <- issues[1:21]

# rendezés hetek szerint
auctions$yearAndweeks <- strftime(auctions$date, format = "%Y-W%V")
str(auctions)

y <- pivot_wider(auctions, names_from = Tenor, values_from =  `Avg Yield (%)`)
plot(y$Date, y$`10Y`)
plot(y$Date, y$`15Y`)
plot(y$Date, y$`10Y`)
plot(y$Date, y$`20Y`)
plot(y$Date, y$`5Y`)


# Az 5 éves kötvények átlaghozamának leíró statisztikája ####

bond5 <- issues[issues$Tenor == "5Y", c("Date", "Accepted (mln)", "Avg Yield (%)", "PeriodStart")]
# Oszlopok nevét egyszerűsítem
colnames(bond5) <- c("Date", "AcceptedAmount", "Yield", "PeriodStart")


## Gyakorisági táblázat ####

binnedCounts(bond5$AcceptedAmount, breaks = 8)


## Hisztogram ####
hist(bond5$AcceptedAmount,
     xlab="Elfogadott mennyiség",
     main="Az elfogadott mennyiségek az 5 éves kötvények elsődleges kibocsátásakor")
ggplot(data=bond5, aes(x=AcceptedAmount/1000)) +
  geom_histogram(bins=21)+
  labs(title="Az elfogadott mennyiségek az 5 éves kötvények elsődleges kibocsátásakor",
       x="Elfogadott mennyiség (milliárd Forint)",
       y= "Gyakoriság (db)")
# hist(bond5[bond5$Date>"2010-01-01", "Avg Yield (%)"])
# hist(bond5[bond5$Date<"2000-01-01", "Avg Yield (%)"])


## Helyzetmutatók ####

acceptedBonds5 <- bond5$AcceptedAmount
### Módusz ####
mostFrequentValues <- names(table(acceptedBonds5)[table(acceptedBonds5) == max(table(acceptedBonds5))])
modusz <- mean(as.numeric(mostFrequentValues))
sprintf("módusz: %s", modusz)

### Medián ####
sprintf("medián: %s", median(acceptedBonds5, na.rm = TRUE))

### Átlag ####
sprintf("átlag: %s", format(mean(acceptedBonds5, na.rm = TRUE), digits =3))

### alak mutatók ----
describe(acceptedBonds5)

# A helyzetmutatók között határozott sorrend állapítható meg: módusz < medián < átlag
# valamint a ferdeség pozitív. Ez alapján az eloszlás jobbra elnyúló.

### Kvantilisek ####                            ))
summary(acceptedBonds5)

### Doboz ábra ----
boxplot(acceptedBonds5) # TODO REMOVE
ggplot(data=bond5, aes(y=AcceptedAmount)) +
  geom_boxplot()


### Szóródás ----
sprintf("szórás: %s", format(sd(acceptedBonds5, na.rm = T), digits = 3))
relativ_szoras <- sd(acceptedBonds5, na.rm = T)/mean(acceptedBonds5, na.rm = T)


plot(bond5$Date, bond5$AcceptedAmount)
# 2009 körül stratégia váltás: gyakoribb aukciók során kisebb mennyiségek kibocsátása

### Outlierek
q<-summary(acceptedBonds5) # named numbers of min, Q1, median, mean, Q3, max
Q3 <- q[5]
Q1 <- q[2]
korlat = Q3 + 1.5 * (Q3 - Q1)

# Eredeti adasorban hol szerepelnek a kiugró értékek:
(auctions[auctions$Tenor=="5Y" & auctions$`Accepted (mln)`> korlat,"Date"])
# A dátumokból látszi, hogy a nagyobb kibocsátások 2007-2008-as és 2020-as
# nehezebb időkben történek 


# Különböző lejáratokra meghírdetett aukciók gyakorisága ####
summary(auctions$Tenor)
# értelmezhető: gyakoriság




# Intervallumbecslés és egymintás hipotézisvizsgálat ####


ggplot(data=bond5,aes(y=AcceptedAmount, x=PeriodStart)) +
  geom_boxplot()
ggplot(data=bond5,aes(y=Yield, x=PeriodStart)) +
  geom_boxplot()

groupwiseMean(Yield ~ PeriodStart,
              data=bond5,
              conf=0.95,
              digits=3,
              na.rm=T)

x <- issues[,c("Tenor", "Avg Yield (%)")]
x <- x[x$Tenor=="3M" | x$Tenor == "6M" | x$Tenor=="1Y"|
         x$Tenor == "3Y" | x$Tenor=="5Y" | x$Tenor == "10Y"|
         x$Tenor == "15Y" | x$Tenor== "20Y", ]
colnames(x)<- c("Tenor", "Yield")
y<-groupwiseMean(Yield ~ Tenor,
              data = x,
              conf= 0.95,
              digits = 3,
              na.rm = T)
y$order <- c(6,7,3,8,1,4,5,2)
y[order(y$order),]
# TODO order by tenor!!!!!

###
## This Is a Level 2 Header ================================

### This is a level 3 header. ------------------------------

# TODO 12M vs 1Y ####