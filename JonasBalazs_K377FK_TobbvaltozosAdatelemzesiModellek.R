# Project:  Többváltozós adatelemzési módszerek
# Szerző:   Jónás Balázs


# Előkészületek ####

## Szükséges packagek telepítése ====
install.packages("rmarkdown") # dokumentáció
install.packages("readxl") # Excel
#install.packages("RcmdrMisc")
install.packages("tidyr")
install.packages("psych") # csúcsosság, ferdeség
install.packages("rcompanion") # groupwise statistics
install.packages("ggplot2")
install.packages("questionr") # Cramer
install.packages("lawstat") # levene.test
## szükséges packagek betöltése
library(readxl)
library(tidyr)
library(RcmdrMisc)
library(psych)
library(rcompanion)
library(ggplot2)
library(rcompanion)
library(lawstat)

## Adatok betöltése és tisztítása ====
auctions <- read_excel("AUCTION_220110173944.xlsx")
auctions <- as.data.frame(auctions)
year <- as.numeric(format(auctions$Date, format="%Y"))

# 10 éves időszakok generálása
periodLength <- 10
periodStarts <- seq(1990, 2030, periodLength)
period <- .bincode(year, periodStarts, right=F)
auctions$Period  <- as.factor(paste(
  periodStarts[period],
  "-", 
  periodStarts[period]+periodLength-1))
# plot(auctions$Date, auctions$Period) # check

# 1 éves lejáratra kétféle jelölést használtak
auctions$Tenor[auctions$Tenor=="12M"]<-"1Y"

referenceBonds <- c("3M", "6M",
                    "1Y", "3Y","5Y",
                    "10Y", "15Y", "20Y",
                    "other")
# manuális rendezés:
auctions$Tenor<- factor(auctions$Tenor,
                        levels=referenceBonds)
auctions$Tenor <- auctions$Tenor %>% replace_na(replace = "other")


# Szűrés új kibocsátásokra és fix kamatozási kötvényekre
issues<-auctions[auctions$Type=="ISSUE"
                 & auctions$`Interest Type`!="Float",]


# Az 5 éves kötvények átlaghozamának leíró statisztikája ####

bond5 <- issues[issues$Tenor == "5Y", c("Date", "Accepted (mln)", "Avg Yield (%)", "Period")]

# Oszlopok nevét egyszerűsítem
colnames(bond5) <- c("Date", "AcceptedAmount", "Yield", "Period")

# Olvashatóság miatt az elfogadott mennyiséget milliárd forintban használom
bond5$AcceptedAmount <- bond5$AcceptedAmount/1000


## Gyakorisági táblázat ####

binnedCounts(bond5$AcceptedAmount, breaks = 6)


## Hisztogram ####

ggplot(data=bond5, aes(x=AcceptedAmount)) +
  geom_histogram(bins=16)+
  labs(title="Az elfogadott mennyiségek az 5 éves kötvények elsődleges kibocsátásakor",
       x="Elfogadott mennyiség (milliárd Forint)",
       y= "Gyakoriság (db)")

## Helyzetmutatók ####

acceptedBonds5 <- bond5$AcceptedAmount


### Módusz ----
mostFrequentValues <- names(table(acceptedBonds5)[table(acceptedBonds5) == max(table(acceptedBonds5))])
modusz <- mean(as.numeric(mostFrequentValues))
sprintf("Módusz: %s", modusz)


### Medián ----
sprintf("Medián: %s", median(acceptedBonds5, na.rm = TRUE))

### Átlag ----
sprintf("Átlag: %s", format(mean(acceptedBonds5, na.rm = TRUE), digits =3))

## Alak mutatók ====
describe(acceptedBonds5)

# A helyzetmutatók között határozott sorrend állapítható meg: módusz < medián < átlag
# valamint a ferdeség pozitív. Ez alapján az eloszlás jobbra elnyúló.

## Kvantilisek ====
summary(acceptedBonds5)

### Doboz ábra ----
ggplot(data=bond5, aes(y=AcceptedAmount)) +
  geom_boxplot()


### Szóródás ----
sprintf("Szórás: %s", format(sd(acceptedBonds5, na.rm = T), digits = 3))
relativ_szoras <- sd(acceptedBonds5, na.rm = T)/mean(acceptedBonds5, na.rm = T)


# 2009 körül stratégia váltás: gyakoribb aukciók során kisebb mennyiségek kibocsátása
ggplot(data=bond5, aes(x=Date, y=AcceptedAmount)) +
  geom_point()+
  labs(title = "Kibocsátott 5 éves kötvények az idő függvényében",
      x="Dátum",
      y="Elfogadott mennyiség (Md forint)")

### Outlierek ----
q<-summary(acceptedBonds5) # named numbers of min, Q1, median, mean, Q3, max
Q3 <- q[5]
Q1 <- q[2]
korlat = Q3 + 1.5 * (Q3 - Q1)

# Eredeti adatsorban hol szerepelnek a kiugró értékek:
(bond5[bond5$AcceptedAmount>korlat, "Date"])
# A dátumokból látszik, hogy a nagyobb kibocsátások 2007-2008-as és 2020-as
# nehezebb időkben történek 


# Különböző lejáratokra meghirdetett aukciók gyakorisága ####
summary(auctions$Tenor)
# értelmezhető: gyakoriság



# Intervallumbecslés és egymintás hipotézisvizsgálat ####

# A hozamok vizsgálatánál csak a sikeres aukciókat veszem figyelembe.
bond5successful <- bond5[bond5$AcceptedAmount>0, ]

ggplot(data=remove_missing(bond5successful, na.rm=TRUE, vars="Yield"),
  aes(y=Yield, x=Period)) +
  geom_boxplot() +
  labs(title="5 éves kötvények hozama",
       x="Időszak",
       y="Hozam (%)")


## Átlag becslése időszakok szerint bontva ====
groupwiseMean(Yield ~ Period,
              data=bond5successful,
              conf=0.95,
              digits=3,
              na.rm=T)

## Medián becslése időszakok szerint bontva ====
groupwiseMedian(Yield ~ Period,
              data=bond5successful,
              conf=0.95,
              digits=3,
              R = 1000,
              percentile = TRUE,
              bca = FALSE
              )

## Az előző időszak arányai ====
round(prop.table(table(issues[issues$Period=="2010 - 2019", "Tenor"]))*100, 1)

auctionTypes <- issues[, c("Period", "Tenor") ]
auctionTypes$fiveYear <- 0
auctionTypes$fiveYear[auctionTypes$Tenor == "5Y"] <- 1
groupwiseMean(fiveYear ~ 1,
              data=auctionTypes,
              conf=0.95,
              digits=3
              )
groupwiseMean(fiveYear ~ Period,
              data=auctionTypes, 
              conf = 0.95,
              digits = 3,
              na.rm = TRUE
              )


## Egymintás hipotézisvizsgálat ====

### Igaz-e, hogy az Államadósság Kezelő a három hónapos aukciókon a meghirdetettnél több ajánlatot fogad el. ----

bond3 <- issues[issues$Tenor=="3M",]
bond3$Rate <- bond3$`Accepted (mln)`/bond3$`Announced (mln)`

# Állítás: bond3$rate > 1
# H0: mu = 1
# H1: mu < 1

# p-érték: 
t.test(bond3$Rate, mu = 1, alternative = "less")

### Igaz-e, hogy az 5 éves hozamok mediánja 4.9% ----
# H0: median = 4.9
# H1: median != 4.9
# hist(bond5$Yield) # TODO ggplot
wilcox.test(bond5$Yield, mu=4.9, exact=FALSE)


## Időszak és lejárat kapcsolata ====

# A legutolsó időszakból még csak két év telt el, de az aukciók aránya már értelmezhető.

ggplot(data=subset(issues,issues$`Accepted (mln)`> 0 ), 
       aes(x=Period, fill =Tenor)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette="Dark2") +
  labs(title="Lejáratok aránya",
       x="Arány (%)",
       y="Időszak")

cramer.v(table(issues[issues$`Accepted (mln)`>0,c("Period", "Tenor")]))

chisq.test(table(issues[issues$`Accepted (mln)`>0,c("Period", "Tenor")]))


## Időszak és hozam kapcsolata ====

aov(Yield ~ Period, data=bond5[bond5$AcceptedAmount>0,])

sprintf("Between: %s", 2143.8)
sprintf("Residuals: %s", 1672.6)
sprintf("H2: %s", 2143.8/ (2143.8 + 1672.6))
summary(aov(Yield ~ Period, data=bond5[bond5$AcceptedAmount>0,]))
sd(bond5[bond5$AcceptedAmount>0 & bond5$Period=="2000 - 2009",]$Yield)
sd(bond5[bond5$AcceptedAmount>0 & bond5$Period=="2010 - 2019",]$Yield)


## Két mennyiségi változó ====

ggplot(data=bond5, aes(x=AcceptedAmount, y=Yield)) +
  geom_point() +
  labs("Hozam és elfogadott mennyiség kapcsolata\n5 éves kötvények aukcióján",
       x="Elfogadott mennyiség",
       y="Hozam")

sprintf("A két változó közötti korreláció: %s",
        cor(bond5$AcceptedAmount, bond5$Yield))
(reg <- lm(Yield ~ AcceptedAmount, bond5))
summary(reg)

# CLEAN UP ####

# rm(list = ls()) 

