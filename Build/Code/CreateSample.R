# This organizes processed raw data and has some basic filters
source(here("Build", "Code", "BrushUpCleanData.R"))
# Load functions
source(here("Build", "Code", "BuildFunctions.R"))

# ~ SAMPLE DESCRIPTION ~
# keep single player games launched when prices were available, not EA, and with 
# at least medPlayersCutoff players playing on a median day. Releasedate should be 
# after sampleStartDate. Data was already prefiltered in 
# BrushUpCleanData.R, but some more filtering will be necessary

# Define some variables
medPlayersCutoff <- 4
# min age
min.age <- 7
one.year <- anydate("2016-01-01") - anydate("2015-01-01")
one.day<- anydate("2014-01-02") - anydate("2014-01-01")
sampleStartDate <- "2017-01-01"
sampleFinalDate <- anydate("2019-01-01")

# ~FINAL SAMPLE SELECTION ~ 
load(here("Build", "Temp", "infoSample.Rda"))
info<-info[mult==F&multOnline ==F&crossplatform==F&mmo==F&ea==F&
           start>=sampleStartDate&medPlayers2Years>=medPlayersCutoff][,
          mult:=NULL][,multOnline:=NULL][,mmo:=NULL][,crossplatform:=F]
info[,ea:=NULL][,eaExit:=NULL]

# Drop the largest outlier game
info <- info[ID != 437060, ]

load(here("Build", "Temp", "pricesSample.Rda"))
prices[, discNew := discount > 0]
prices[, date := anydate(date)]

# some manual corrections of scraping mistakes. These mistakes seem to steamDB's,
# not mine
prices <- rbind(prices, list(626640, anydate("2017-10-26"), 6.99, 0.00, F))
setkey(prices, ID, "date")
prices <- rbind(prices, list(366870, anydate("2018-02-20"), 19.99, 0.00, F))
prices <- rbind(prices, list(583760, anydate("2017-08-22"), 0.99, 0.00, F))
prices <- rbind(prices, list(586090, anydate("2017-12-15"), 0.99, 0.00, F))
prices <- rbind(prices, list(595440, anydate("2018-02-19"), 0.99, 0.00, F))
prices <- rbind(prices, list(595440, anydate("2018-04-16"), 0.49, 0.51, T))
prices <- rbind(prices, list(601530, anydate("2017-10-02"), 5.99, 0.00, F))
prices <- rbind(prices, list(665090, anydate("2017-10-02"), 9.99, 0.00, F))
prices <- rbind(prices, list(708580, anydate("2018-11-27"), 4.99, 0.00, F))
prices <- rbind(prices, list(712840, anydate("2017-10-26"), 1.99, 0.00, F))
prices <- rbind(prices, list(712840, anydate("2017-11-22"), 1.19, 0.4, T))
prices <- rbind(prices, list(429660, anydate("2018-11-05"), 49.99, 0.0, F))
prices <- rbind(prices, list(429660, anydate("2018-11-21"), 12.49, 0.75, T))
prices <- rbind(prices, list(462770, anydate("2017-10-02"), 19.99, 0.0, F))
prices[ID==485030 & date=="2018-11-27", price := 19.99]
prices[ID==485030 & date=="2018-11-27", discount := 0.0]
prices[ID==485030 & date=="2018-11-27", discNew := F]
prices <- rbind(prices, list(543870, anydate("2018-11-05"), 19.99, 0.0, F))
prices <- rbind(prices, list(543870, anydate("2018-11-21"), 9.99, 0.5, T))
prices <- rbind(prices, list(588730, anydate("2018-11-27"), 5.99, 0.0, F))
prices <- rbind(prices, list(618970, anydate("2018-11-27"), 34.99, 0.0, F))
prices <- rbind(prices, list(634160, anydate("2018-11-27"), 14.99, 0.0, F))
setkey(prices, ID, "date")


# Player Data
load(here("Build", "Temp", "playersSample.Rda"))
players[, twitch := NULL]

# The resulting panel is a panel of single player non F2P games with the first date
# being the (well-behaved) release date
panel <- merge(players, prices, by = c("ID", "date"), all = T)

# remove free games
panel <- panel[ID %in% panel[, sum(price, na.rm = T), by = ID][V1 > 0, ID], ]
rm(players, prices)
panel[, price := fillMissingPrices(price), by = ID]
panel[, discount := fillMissingPrices(discount), by = ID]
panel[is.na(discNew), discNew := F]
panel <- merge(panel, info[, .(ID, releaseDate, relDateEA)], by=c("ID"), all.y = F)
panel <- panel[ID %in% panel[, sum(is.na(price)), by = ID][V1 <= 4, ID]]
panel[, price := rev(fillMissingPrices(rev(price))), by = ID]
panel[, discount := rev(fillMissingPrices(rev(discount))), by = ID]

# ignore pre-release discounts
panel<-panel[date>=ifelse(is.na(releaseDate),relDateEA,releaseDate),][!is.na(number),]
panel[, releaseDate:= NULL][, relDateEA:=NULL]


# Load reviews
load(here("Build", "Temp", "reviewsSample.Rda"))
reviews <- reviews[,.(positiveVisible = max(positive_visible), 
                      negativeVisible = max(negative_visible),
                      positiveSteam = max(positive_onSteam), 
                      negativeSteam = max(negative_onSteam),
                      positiveOffSteam = max(positive_offSteam),
                      negativeOffSteam = max(negative_offSteam)),
                   by = c("ID", "date")]
reviews[is.na(positiveVisible), positiveVisible := 0]
reviews[, totalVisible:=positiveVisible+negativeVisible]
reviews[, totalSteam:=positiveSteam+negativeSteam]
reviews[, totalOffSteam:=positiveOffSteam+negativeOffSteam]
panel <- merge(panel, reviews[, .(ID, date, pReviews = positiveVisible, 
                                  nReviews = negativeVisible,
                                  reviews = totalVisible)],
               by=c("ID","date"), all.x = T)
rm(reviews)
panel[, reviews  := fillMissingPrices(reviews), by = ID]
panel[, pReviews := fillMissingPrices(pReviews), by = ID]
panel[, nReviews := fillMissingPrices(nReviews), by = ID]
panel[, score := pReviews/reviews]

# Steam rounds the score down
panel[, score:=floor(100*score)]
panel[is.na(reviews), reviews := 0]
panel[reviews < 10, score := NA]

# define review labels
panel[, noScore := reviews < 10 | is.na(score)]
panel[, negative := ifelse(noScore, F, score < 40)]
panel[, mixed := ifelse(noScore, F, (score>=40) & (score<70))]
panel[, mPositive := ifelse(noScore, F, (score>=70) & (score<80))]
panel[, positive := ifelse(noScore, F, score>=80)]
panel[, ovPositive := score >= 95 & reviews >= 500]
panel[, vPositive := score >= 80 & reviews >= 50 & ovPositive == F]
# positive label above bundles together realPositive, vPositive, and ovPositive
panel[, realPositive := ifelse(noScore, F, score >= 80 & reviews < 50)]

# Introduce panel time
panel[,day:=factor(weekdays(date), 
                   levels=c("Monday", "Tuesday", "Wednesday",
                              "Thursday", "Friday", "Saturday","Sunday"),
                   labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))]
setnames(panel, old = "date", new = "t")
t.min <- as.integer(anydate(sampleStartDate))
t.max <- as.integer(sampleFinalDate) - t.min

panel[, t := as.integer(t) - t.min]
panel[, week := factor(t %/% 7)]
panel[, age := t - t[1], by = ID]
panel[, young := age<=14]



# Load the information on updates
load(here("Build", "Input", "developerNotes.rda"))
notes <- all_notes[, .(url_to_note, GameID, date, note_type, missing_tag, update_tag)]
rm(all_notes)

notes <- notes[url_to_note!="",][, url_to_note:= NULL]
notes <- notes[, date := anydate(date)][date<sampleFinalDate, ]
notes[, t:=as.integer(date) - t.min]
setnames(notes, old = "GameID", new = "ID")
setkey(notes, ID, t)
notes <- notes[ID%in%info[, ID] & missing_tag==F & update_tag==T, 
               .(ID = as.integer(ID), t)]

notes <- notes[!duplicated.data.frame(notes),]
# updates is the total number of updates to date
notes[, updates := 1:length(t), by = ID]

# Exclude all games we have updates data for if they had more than 5 
# updates in our sample.
notes <- notes[, .(maxUpdates = max(updates,na.rm = T)), by = ID]
exclude.ids <- notes[maxUpdates>=5, ID]
info <- info[!(ID %in% exclude.ids), ]
panel <- panel[!(ID %in% exclude.ids), ]
rm(exclude.ids, notes)


# Start  Seasonal Discounts
# discounts contains all discounts that we see starting in the data
discounts <- panel[, .(avDisc = mean(discount)), keyby = t]
discounts[t>=14, discSeason := (avDisc >= 0.2)]
panel = merge(panel, discounts, by = c("t"))
rm(discounts)

# For all discounts we calculate time since previous discount, which, 
# preciesly, counts the number of days without a discount plus 1. Duration 
# of the discount measures the number of days that had a discount
panel[, tToPChange := timeOfNextPriceChange(discount, t) - t, by = ID]

# define time since a discount
panel[, auxV := 0]
panel[age==1 | discNew==T, auxV:=1]
panel[,tWODisc:=rev(timeOfNextPriceChange(rev(auxV),age)-age), 
      by = ID]
panel[discNew==T | age==1, tWODisc := 0]
panel[, auxV:=NULL]

# manually code the lag variables
panel[, numberLag := shift(number), by = ID]
panel[, discNewLag := shift(discNew), by = ID]

# drop observations of the first day because the lag players are missing
panel <- panel[!is.na(numberLag),]
# a couple of games did not have a single review -- drop them
norevids <- panel[, max(reviews), by = ID][V1==0, ID]
panel <- panel[!(ID %in% norevids), ]

# Exclude observations after sampleFinalDate and then games that spent less than 
# a week in the sample
panel <- panel[t < t.max, ]
# panel <- panel[t >= one.year + one.year]
pan.ids <- panel[, .(m.age=max(age, na.rm = T)), by = ID][m.age>=min.age, ID]
panel <- panel[ID %in% pan.ids]
info <- info[ID %in% panel[, unique(ID)], ]
setkey(panel, ID, t)

#number of games
n <- info[, .N]
# relabel ids for easier indexing in data table in Julia
for (i in 1:n){
  info[i, id:=i]
}

save(panel, file=here("Build", "Output", "panel.Rda"))
save(info, file=here("Build", "Output",  "info.Rda"))

# remove temporary files
for (file in list.files(here("Build", "Temp"))){
  unlink(here("Build", "Temp", file))
}


# ~ PREPARE THE DATA FOR THE NONLINEAR ESTIMATION IN JULIA ~ 
# renormalize for better behavior in the non-linear solver
panel[, age := age/100]
panel[, score:=score/10]

# score and lrevs are really score and lrevs times (1-noScore).
panel[noScore==T, score := 0]
panel[noScore==T, reviews := 0]
panel[, lrev := log(reviews+1)]
panel[, price := round(price/(price[1]/(1-discount[1])), digits = 2), by=ID]

# price for the game that has a 100% discount in the data
panel[is.na(price), price := 1.0]

panel <- merge(panel, info[, .(ID, id)], by = "ID")
panel <- dummy_cols(panel, select_columns = "day")

regressors = c("price", "discSeason", "age", "young", 
               "noScore", "negative", "mPositive", "positive", 
               "day_Tue", "day_Wed", "day_Thu", "day_Fri", "day_Sat",
               "day_Sun", "lrev")

# for this exercise keep only the stuff that is needed
panel <- panel[, c("id","t","number","numberLag",regressors), with=F]
setkey(panel, "id", "t")
fwrite(panel, file=here("Build", "Output", "juliaData.csv"))
