# Project:  Többváltozós adatelemzési módszerek
# Szerző:   Jónás Balázs

# Előkészületek ####
## Szükséges packagek telepítése ====
install.packages("readxl")
install.packages("RcmdrMisc")
install.packages("tidyr") # 
install.packages("psych") # csúcsosság, ferdeség
library(readxl)
library(tidyr)
library(RcmdrMisc)

## Adatok betöltése és tisztítása ====
auctions <- read_excel("AUCTION_220110173944.xlsx")
auctions <- as.data.frame(auctions)


# 1 éves lejáratra kétféle jelölést használtak
auctions[auctions$Tenor=="12M"]<-"1Y"
auctions$Tenor<-as.factor(auctions$Tenor)

# Új kibocsátások szűrése
auctions<-auctions[auctions$Type=="ISSUE",]

# cserepapírokra nem lesz szükség
auctions <- auctions[1:21]

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
auctions$`Accepted (mln)`
bond5 <- auctions[auctions$Tenor == "5Y", c("Date", "Avg Yield (%)")]
bond5 <- auctions[auctions$Tenor == "5Y", c("Date", "Accepted (mln)", "Announced (mln)")]
yield5 <- bond5$`Accepted (mln)`

## Gyakorisági táblázat ####

binnedCounts(yield5, breaks = 8)


## Hisztogram ####
hist(yield5,
     xlab="Átlagos hozam",
     main="Az 5 éves kötvények átlagos hozama kibocsátáskor")
# hist(bond5[bond5$Date>"2010-01-01", "Avg Yield (%)"])
# hist(bond5[bond5$Date<"2000-01-01", "Avg Yield (%)"])


## Helyzetmutatók ####

### Módusz ####
sprintf("módusz: %s", names(table(yield5)[table(yield5) == max(table(yield5))]))

### Medián ####
sprintf("medián: %s", median(yield5, na.rm = TRUE))

### Átlag ####
sprintf("átlag: %s", format(mean(yield5, na.rm = TRUE), digits =3))

### Kvantilisek ####                            ))
summary(yield5)

### Doboz ábra ----
boxplot(yield5)

### Szóródás ----
sprintf("szórás: %s", format(sd(yield5, na.rm = T), digits = 3))
relativ_szoras <- sd(yield5, na.rm = T)/mean(yield5, na.rm = T)

### alak mutatók ----
describe(yield5)
# positive skew (jobbra )

plot(bond5$Date, bond5$`Accepted (mln)`)
# 2009 körül stratégia váltás

# TODO ####
# kiugró értékek
# átlag medián viszonya + histogram

# nem numerikus változó
summary(auctions$Tenor)
# mi értelmezhető


# Intervallumbecslés és egymintás hipotézisvizsgálat ####

###
## This Is a Level 2 Header ================================

### This is a level 3 header. ------------------------------

# TODO 12M vs 1Y ####