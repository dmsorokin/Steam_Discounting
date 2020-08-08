# Load functions
source(here("Build", "Code", "BuildFunctions.R"))

# Load cleaned dataset 
load(here("Build", "Input", "allSteamReviews.Rda"))

# Drop games based on specific date 
#  For earlier run, we used date of second policy change : 03-09-2017
#  We now include all dates before May 1, 2019
dateThreshold=as.Date('05-01-2019','%m-%d-%Y')
allSteamReviews[,min_date:=min(posted_date),by=c('game_id')]
sample_data=allSteamReviews[min_date<=dateThreshold,]
sample_data[,min_date:=NULL]
rm(list=c("allSteamReviews"))
# Create review counts by day + game + activation type
aggSteamReviews = create_ts_reviews(sample_data)

# Create dummy for whether game had early access
setDT(sample_data)
early_access_data=sample_data[,list(yes_early_access=sum(early_access_review!='')>0),
                              by=c('game_id')]
early_access_dates=sample_data[early_access_review=='Early Access Review',
                               list(early_access_start=min(posted_date),
                                    early_access_end=max(posted_date)),by=c('game_id')]
early_access_data=merge(early_access_data,
                        early_access_dates,
                        all.x=T,
                        on='game_id')
early_access_data[,early_access_daysInEA:=as.integer(early_access_end-early_access_start)]
save(early_access_data,file=here("Build","Temp","earlyAccess.dta"))
rm(list=c("sample_data"))
# Create columns that are the visible reviews 
setDT(aggSteamReviews)


# Create columns that are cumulative sums of postive/negative review counts
aggSteamReviews[,positive_review_total:=cumsum(positive_review_by_day)
                ,by=list(game_id,steam_key_activation,posted_date)]
aggSteamReviews[,negative_review_total:=cumsum(negative_review_by_day)
                ,by=list(game_id,steam_key_activation,posted_date)]
aggSteamReviews[,total_review_total:=cumsum(total_review_by_day)
                ,by=list(game_id,steam_key_activation,posted_date)]


# ~~ Create columns for reviews left by users who bought their game on steam and 
# all other users ~~
#
# onSteam  -> reviewers bought game on steam
# offSteam -> reviewers received game another way (either off steam or with a 
# developer key on steam)
#
# 

# ~ On Steam Columns ~ #
reviews_onSteam = aggSteamReviews[steam_key_activation==T,]
reviews_onSteam[,c("positive_onSteam","negative_onSteam","total_onSteam"):=
                  list(cumsum(positive_review_by_day),
                       cumsum(negative_review_by_day),
                       cumsum(positive_review_by_day + negative_review_by_day)),
                        by=c("game_id")]
reviews_onSteam=reviews_onSteam[,list(game_id,
                                      posted_date,
                                      positive_onSteam,
                                      negative_onSteam,
                                      total_onSteam)]
# ~ Off Steam Columns ~ #
reviews_offSteam = aggSteamReviews[steam_key_activation==F,]
reviews_offSteam[,c("positive_offSteam","negative_offSteam","total_offSteam"):=
                   list(cumsum(positive_review_by_day),
                        cumsum(negative_review_by_day),
                        cumsum(positive_review_by_day + negative_review_by_day)),
                        by=c("game_id")]
reviews_offSteam=reviews_offSteam[,list(game_id,
                                      posted_date,
                                      positive_offSteam,
                                      negative_offSteam,
                                      total_offSteam)]

# ~ Create dataset with both on and off steam columns ~ #
panelSteamReviews = merge(reviews_onSteam,
                          reviews_offSteam,
                          by=c('game_id','posted_date'),
                          all=T)

# ~~ Create column for reviews that were visible ~~ # 
panelSteamReviews[,free_reviews_removed:=posted_date>as.Date('03-09-2017','%m-%d-%Y')]
panelSteamReviews[,keys_reviews_removed:=posted_date>as.Date('09-12-2016','%m-%d-%Y')]
panelSteamReviews[,c("positive_visible","negative_visible","total_visible"):=
                    create_reviews_col(positive_onSteam,
                                        negative_onSteam,
                                        total_onSteam,
                                        positive_offSteam,
                                        negative_offSteam,
                                        total_offSteam,
                                        keys_reviews_removed),
                  by=seq_len(nrow(panelSteamReviews))]

# ~~ Fill in NAs ~~ #
panelSteamReviews[is.na(positive_onSteam),positive_onSteam:= 0]
panelSteamReviews[is.na(negative_onSteam),negative_onSteam:= 0]
panelSteamReviews[is.na(total_onSteam),total_onSteam:= 0]
panelSteamReviews[is.na(positive_offSteam),positive_offSteam:= 0]
panelSteamReviews[is.na(negative_offSteam),negative_offSteam:= 0]
panelSteamReviews[is.na(total_offSteam),total_offSteam:= 0]

# ~~ Create review scores ~~ #
panelSteamReviews[,review_score_visible:=positive_visible/total_visible]
panelSteamReviews[,review_score_onSteam:=(positive_onSteam)/
                    (total_onSteam)]
panelSteamReviews[,review_score_offSteam:=(positive_offSteam)/
                    (total_offSteam)]

# ~~ Unique ~~ #
panelSteamReviews = unique(panelSteamReviews)

# ~~ Rename ~~ #
panelReviews <- copy(panelSteamReviews)
rm(panelSteamReviews, aggSteamReviews, reviews_offSteam, reviews_onSteam)


# ~~ Move on to the other data sources
info <- fread(here("Build", "Input", "info.csv"), encoding="UTF-8", 
              na.strings = "")

# Release Dates were not displayed correctly on the website during the initial
# scrapping. These ones were collected later.
relDates <- fread(here("Build", "Input", "releaseDates.csv"), 
                  col.names = c("ID", "releaseDate2", "comingSoon"))
relDates[, len:= nchar(releaseDate2)]
relDates[len == 11, releaseDate2 := stri_sub_replace(releaseDate2, 5, 4, 
                                                     value = "0")]
ids <- intersect(info[, ID], relDates[, ID])
relDates <- relDates[ID %in% ids, ]
info <- info[ID %in% ids, ]
info <- merge(info, relDates, by = "ID")
info[, releaseDate := anydate(releaseDate2)]
info[, releaseDate2 := NULL]
setkey(info, ID)
rm(relDates)

# Only keep games we have reviews for
# load(here("Build","Output","AnalysisData","panelReviews.Rda"))
ids <- intersect(ids, panelReviews[, unique(game_id)])
info <- info[ID %in% ids, ]
info <- info[, firstDayReview:=
               panelReviews[, anydate(posted_date[1]), by=game_id][, V1]
             ]


# players data
players <- fread(here("Build", "Input", "players.csv"), 
                 encoding = "UTF-8", na.strings = "")
players[, date := anydate(date)]
setkey(players, ID, date)
# Keep games with recorded history exceeding 1 week
ids <- intersect(players[!is.na(players), .N, by = ID][N >= 7, ID], ids) 
players <- players[ID %in% ids,]
info <- info[ID %in% ids, ]


# Markers
markers <- fread(here("Build", "Input", "markers.csv"), 
                 encoding = "UTF-8", 
                 na.strings = "")
markers <- markers[!is.na(date), ]
setkey(markers, ID, date)

# price data
prices <- fread(here("Build", "Input", "prices.csv"),
                encoding = "UTF-8", na.strings = "")
prices[, date := anydate(date)]
setkey(prices, ID, date)
prices <- prices[ID %in% ids,]

# Define the "starting" date for each game, a good guess of when the game
# actually became playable.
info[,releaseDate:=as.Date(releaseDate,'%Y-%m-%d')]
info[,firstDayReview:=as.Date(firstDayReview,'%Y-%m-%d')]
info[, start := min(releaseDate, firstDayReview), by = ID]

# Use price and players data to calculate maximum and median player counts after the 
# start of the game
players<-merge(players, info[, .(start), by = ID], by = "ID")
info <- merge(info, players[date>=start,.(n.obs=.N), by=ID], by='ID')
info <- merge(info, players[, .(firstDayPlayers = date[1]), by = ID], by = "ID")
# Free games will have NAN as firstDayPrice
info <- merge(info, prices[, .(firstDayPrice = date[1]), by = ID], by = "ID")


# ~~ Sample Selection, Step 1 ~~ #
# This is preliminary, and will be strengthened further. The goal is to get
# rid of games that won't be used for sure
info[n.obs >= 2 &
     !is.na(firstDayPrice) &
     releaseDate >= firstDayPlayers &
     firstDayReview > "2013-10-25", 
     sample:=T]

# Only keep IDs from the sample. Drop observations before the start date.
# Interpolate the series
players <- players[ID %in% info[sample==T, ID], ]
players <- players[, number := interpolateVariable(players), by = ID]
players <- players[date >= start, ]
players <- players[, .(ID, date, number, twitch, start)]

# Calculate the number of players on the median and the best days
info <- merge(info, players[,.(maxPlayers=max(number, na.rm=T),
                               medPlayers=median(number, na.rm=T)), by=ID], by='ID')

# Look at the first two years of activity only to calculate the median player count
two.years <- anydate("2017-01-01") - anydate("2015-01-01")
info <- merge(info, players[date<=start+two.years,.(
                            medPlayers2Years=median(number, na.rm=T)), by=ID], by='ID')
players[, start := NULL]

# ~~ Sample Selection, Step 2~~ #
# deselect games with inadequate median player counts #
info[sample==T & (medPlayers <= 3 | medPlayers2Years<=0), 
     sample:=F]


# Load information on Early Access games
load(here("Build", "Temp", "earlyAccess.dta"))
setnames(early_access_data, old = "game_id", new = "ID")
setnames(early_access_data, old = "yes_early_access", new = "ea")
setnames(early_access_data, old = "early_access_start", new = "eaStart")
setnames(early_access_data, old = "early_access_end", new = "eaEnd")
early_access_data[, early_access_daysInEA:= NULL]
info <- merge(info, early_access_data, by = c("ID"), all.x = T)
rm(early_access_data)

# Games that exited EA have the last EA review before the release date (+1 day, perhaps)
# All other EA games have not exited, except for a few outliers that we need to encode 
# manually. The outliers are described by having the last EA review after the release
# (which suggests that they are still EA), but also having the first EA review
# more than a week away from the release date.
info[eaEnd<=releaseDate+1, eaExit := T]
info[eaEnd>releaseDate +1 & abs(eaStart-releaseDate)<=7, eaExit := F]
info[start==releaseDate & ea==T, eaExit := F]
info[eaExit==F, relDateEA := releaseDate][eaExit==F, releaseDate := NA]
info[eaExit==T, relDateEA := start]
info[, eaStart:=NULL][, eaEnd:=NULL]

# Finalize the sample
info <- info[sample == T, .(ID, gameName, developer, publisher, start,
                            releaseDate, contSupp, metacritic, 
                            posReview, negReview, maxPlayers, medPlayers, medPlayers2Years,
                            ea, eaExit, relDateEA)]
info[, gameName := clean.text(gameName, numbers = F)]
info[, developer := clean.text(developer, numbers = F)]
info[, publisher := clean.text(publisher, numbers = F)]

# Add genre tags
mmoIDs <- fread(here("Build", "Input", "mmo.csv"))[, V1]
multIDMult <- fread(here("Build", "Input", "multiplayer.csv"))[, V1]
multIDOnline <- fread(here("Build", "Input", "online.csv"))[, V1]
multIDCross <- fread(here("Build", "Input", "crossPlatformIDs.csv"))[, V1]

info[, mmo := ID %in% mmoIDs]
info[, mult := ID %in% multIDMult]
info[, multOnline := ID %in% multIDOnline]
info[, crossplatform := ID %in% multIDCross]
rm(mmoIDs, multIDMult, multIDCross, multIDOnline)


# Only keep IDs from the sample. Prices could be missing for games, either because
# they are FTP, or because we only observe prices later
players <- players[ID %in% info[, ID]]
prices <- prices[ID %in% info[, ID], ]
prices <- merge(prices, info[, .(ID, start)], by = "ID")

# Drop multiple prices on the same day (keep the smallest positive price)
prices <- prices[, remove.dup.prices(.SD), by = .(ID, date)]
prices <- unique(prices)

# Add columns: the difference between the price and the previous price (p.change),
# the duration (p.dur) of the price, and the change the next price will bring.
prices[, p.change := price - shift(price), by = ID]
prices <- prices[p.change != 0 | is.na(p.change), ]
prices[, p.dur := shift(date,-1) - date, by=ID]
prices[, p.change.next := shift(price, type = "lead") - price, by = ID]

# Recode some of the discounts that were not reflected as discounts. The majority of 
# discounts are at most 3 weeks long, so code all price decreases that last for 
# less than 3 weeks as discounts.
prices[, discount := round(discount/100, digits = 2)]
prices[p.change < 0 & p.change.next > 0 & p.dur <= 14 & discount == 0, 
       discount := -round(p.change/(price - p.change), digits = 2)]
prices <- prices[,.(ID, date, price, discount)]

# Export the Sample
save(info, file = here("Build", "Temp", "infoSample.Rda"))
save(players, file = here("Build", "Temp",  "playersSample.Rda"))
save(prices, file = here("Build", "Temp", "pricesSample.Rda"))
rm(players, prices)

# Reformat the reviews data a bit, and export that as well
panelReviews <- panelReviews[game_id %in% info[, ID]]
panelReviews[, ID := game_id][, date := anydate(posted_date)]
panelReviews[, game_id := NULL][, posted_date := NULL]
setkey(panelReviews, ID, date)
setcolorder(panelReviews, c(14, 15, 1:13))
reviews <- unique(panelReviews)
save(reviews, file=here("Build","Temp","reviewsSample.Rda"))

rm(list = ls())