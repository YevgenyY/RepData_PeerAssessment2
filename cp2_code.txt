

dataset <- read.csv(bzfile("repdata%2Fdata%2FStormData.csv.bz2"))
df <- dataset[dataset$INJURIES!=0 | dataset$FATALITIES != 0 | dataset$PROPDMG != 0 | dataset$CROPDMG != 0, ]

## Clean dataset
## Work with levels
levels(df$CROPDMGEXP)[levels(df$CROPDMGEXP) == ""] <- 0
levels(df$CROPDMGEXP)[levels(df$CROPDMGEXP) == "?"] <- 0
levels(df$CROPDMGEXP)[levels(df$CROPDMGEXP) == "0"] <- 1
levels(df$CROPDMGEXP)[levels(df$CROPDMGEXP) == "2"] <- 100
levels(df$CROPDMGEXP)[levels(df$CROPDMGEXP) == "B"] <- 10^9
levels(df$CROPDMGEXP)[levels(df$CROPDMGEXP) == "k" | levels(df$CROPDMGEXP) == "K"] <- 10^3
levels(df$CROPDMGEXP)[levels(df$CROPDMGEXP) == "m" | levels(df$CROPDMGEXP) == "M"] <- 10^6

levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "" | levels(df$PROPDMGEXP) == "?"] <- 0
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "+" | levels(df$PROPDMGEXP) == "-"] <- 0
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "1"] <- 10
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "0"] <- 1
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "2" | levels(df$PROPDMGEXP) == "h" | levels(df$PROPDMGEXP) == "H"] <- 100
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "3" | levels(df$PROPDMGEXP) == "k" | levels(df$PROPDMGEXP) == "K"] <- 1000
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "4"] <- 10^4
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "5"] <- 10^5
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "6" | levels(df$PROPDMGEXP) == "m" | levels(df$PROPDMGEXP) == "M"] <- 1000
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "7"] <- 10^7
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "8"] <- 10^8
levels(df$PROPDMGEXP)[levels(df$PROPDMGEXP) == "B"] <- 10^9

df$CROPDMG_FIN <- df$CROPDMG*as.numeric(levels(df$CROPDMGEXP))[df$CROPDMGEXP]
df$PROPDMG_FIN <- df$PROPDMG*as.numeric(levels(df$PROPDMGEXP))[df$PROPDMGEXP]

## Find sum of fatalities and injures 
dmg_hpop <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, df, FUN=sum)
top10_fat<-head(dmg_hpop[order(dmg_hpop$FATALITIES, decreasing = T),1:2], n=10)
top10_inj<-head(dmg_hpop[order(dmg_hpop$INJURIES, decreasing = T),c(1,3)], n=10)

par(mfrow=c(1,2))
barplot(top10_fat$FATALITIES, names=top10_fat$EVTYPE,las=2, col="red", 
        main="Top 10 most dedliest weather events", cex.names=0.6)
barplot(top10_inj$INJURIES, names=top10_inj$EVTYPE,las=2, col="red", 
        main="Top 10 weather events for injures", cex.names=0.6)

## Find economical impact of severe weather events
dmg_econ <- aggregate(cbind(PROPDMG_FIN, CROPDMG_FIN, PROPDMG_FIN + CROPDMG_FIN) ~ EVTYPE, 
                      df, FUN=sum)

names(dmg_econ)[4] <- "TOTAL_DMG"


top10_total <- head(dmg_econ[order(dmg_econ$TOTAL_DMG, decreasing=T), ], n=10)
top10_prop <- head(dmg_econ[order(dmg_econ$PROPDMG_FIN, decreasing=T), c(1,2)], n=10)
top10_crop <- head(dmg_econ[order(dmg_econ$CROPDMG_FIN, decreasing=T), c(1,3)], n=10)

### Plot the results
par(mfrow=c(1,3))
barplot(rbind(top10_total$PROPDMG_FIN, top10_total$CROPDMG_FIN), names=top10_total$EVTYPE, las=2, 
        col=c("red","green"), 
        main="Top 10 severe weather events\n by their economical impact", cex.names=0.6)

barplot(top10_prop$PROPDMG_FIN, names=top10_prop$EVTYPE, las=2, col="red", 
        main="Top10 severe weather events by their \nimpact for damages on property", cex.names=0.6)

barplot(top10_crop$CROPDMG_FIN, names=top10_crop$EVTYPE, las=2, col="green", 
        main="Top10 severe weather events by their \nimpact for damages on crops", cex.names=0.6)


