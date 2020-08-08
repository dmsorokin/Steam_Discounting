interpolateInterval <- function(tVec, yVec, toleratedDif = 1){
  # Interpolates between (t0, y0) and (t1, y1). tVec=(t0, t1) and yVec=(y0, y1)
  # If the difference between the points is too big, don't interpolate.
  # By default, the tolerance is 1, which means that all points would be interpolated
  nPoints <- tVec[2] - tVec[1] + 1
  
  if(abs(yVec[2]-yVec[1])/max(yVec[1], yVec[2]) < toleratedDif){
    seq(yVec[1], yVec[2], length.out = nPoints)[2:(nPoints-1)] %>%
      as.integer()
  }
  else{
    seq(0, 0, length.out = nPoints)[2:(nPoints-1)] %>%
      as.integer()
  }
}

interpolateVariable <- function(v, toleratedDif = 1){
  # Take a vector of number and replace zeroes with interpolation of the closest
  # non-zero elements. Example: 
  # > interpolateVariable(c(NA, NA, 0, 1, 4, 0, 0, 1)) 
  # > NA NA  0  1  4  3  2  1
  
  # temporarily replace NA with 0
  naInd <- is.na(v)
  v[naInd] <- 0
  
  # indentify the positions of non-zero elements
  nonZerov <- v != 0
  lenV <- length(v)
  nonZeroInd <- (1:lenV)[nonZerov]
  n <- length(nonZeroInd)
  if (n <= 1){
    v[naInd] <- NA
    return(v)
  }
  first <- nonZeroInd[1]
  last <- nonZeroInd[n]

  # eliminate all the non-zero elements that follow each other and which, 
  # for that reason, are not needed in interpolation
  shiftNonZero <- c(nonZeroInd[-1], last + 1)
  indVec <- (1:n)[(shiftNonZero - nonZeroInd - 1) != 0]
  
  # pairs of indicies (t0,t1) to interpolate between
  tVecs <- lapply(indVec, function(x) nonZeroInd[c(x, x+1)])
  # pairs of values (y0,y1) to interpolate
  yVecs <- lapply(tVecs, function(x) v[c(x[1], x[2])])
  
  if (length(tVecs) == 0){
    v[naInd] <- NA
    return(v)
  }
  
  #Chop off zeroes at the beginning and the end of v, as those are not interpolated
  if (last < lenV){
    nonZerov[-(1:last)] <- T
  }
  if (first > 1){
    nonZerov[1:(first-1)] <- T
  }
  
  lapply(1:length(tVecs), 
         function(i) interpolateInterval(tVecs[[i]], yVecs[[i]], toleratedDif)) %>% 
    unlist -> v[!nonZerov]
  v[naInd] <- NA
  return(v)
}

findFirstRealDate <- function(data, nConsec){
  # Find the first non-zero observation followed by nConsec non-zero observations
  # That date has a good chance of being the actual release date
  v <- data[, players]
  # replace NA with 0
  naInd <- is.na(v)
  v[naInd] <- 0
  
  # indentify the positions of non-zero elements
  nonZerov <- v != 0
  lenV <- length(v)
  nonZeroInd <- (1:lenV)[nonZerov]
  
  # Find the first non-zero element followed by a non-zero element
  pos <- match(nConsec, diff(nonZeroInd, nConsec))
  if(is.na(pos)){
    return(findFirstRealDate(data, nConsec - 1))
  }
  
  firstRealObs <- nonZeroInd[pos]
  return(data[, date][firstRealObs])
}

findFirstRealDate2 <- function(data, nConsec){
  # Find the first point at which the player count increased by a factor of > 20
  # after the firstRealDate(data, nConsec)
  
  dateStart <- findFirstRealDate(data, nConsec)
  
  data <- data[date >= dateStart & players > 0, ]
  data[, growth := c(diff(players), 0)]
  data[, g.rate := growth/players]
  candidate <- data[g.rate >= 20, date][1]
  candidate <- data[date >= candidate, date][2]
  ifelse(is.na(candidate), return(anydate("2000-01-01")), return(candidate))
}

remove.dup.prices <- function(dat){
  # Among multiple prices on a given date select the smallest non-zero one.
  f <- dat[price > 0, .(price, discount, start, v = min(price))][price == v,
                                                      .(price, discount, start)]
  ifelse(dim(f)[1] == 0, return(dat[, .(price, discount, start)]), return(f))
}

clean.text <- function(x, lowercase=TRUE, numbers=TRUE, 
                       punctuation=TRUE, spaces=TRUE){
  # x: character string
  
  # lower case
  if (lowercase)
    x = tolower(x)
  # remove numbers
  if (numbers)
    x = gsub("[[:digit:]]", "", x)
  # remove punctuation symbols
  if (punctuation)
    x = gsub("[[:punct:]]", "", x)
  # remove extra white spaces
  if (spaces) {
    x = gsub("[ \t]{2,}", " ", x)
    x = gsub("^\\s+|\\s+$", "", x)
  }
  # return
  x
}

create_ts_reviews = function(all_reviews){
  #  """
  #  Create following columns:
  #
  #    # ~ Uniquely identify game + date ~ #
  #    date 
  #    game_id
  #    
  #    # ~ Type of activation method ~ #
  #    steam_key_activation
  #    free_activation
  #
  #    # ~ Outcome Variables ~ #
  #    positive_review_count
  #    negative_review_count
  #
  #    # ~ Weights ~ #
  #    <TBD>
  #  """
  # Simple aggregate and sum function by
  #  -Date + Game
  #  -Activation Type (steam_key + free)
  agg_data = sample_data[,list(positive_review_by_day=sum(upvote==1),
                               negative_review_by_day=sum(upvote==0),
                               total_review_by_day=length(upvote)),
                         by=c('posted_date','game_id','steam_key_activation',
                              'free_activation')]
  
  # Create date range given starting and ending dates
  # Note: each game may not have all types of reviews 
  #       Ex: game_id==20 has no free_activation reviews
  agg_data = agg_data%>% 
    group_by(game_id) %>%
    complete(posted_date = seq.Date(min(posted_date), max(posted_date), by="day"),
             game_id,steam_key_activation,free_activation)
  
  # Replace NAs with 0s
  agg_data[is.na(agg_data)] = 0
  
  # Return data
  return(agg_data)
  
}

create_reviews_col = function(positive_onSteam,
                              negative_onSteam,
                              total_onSteam,
                              positive_offSteam,
                              negative_offSteam,
                              total_offSteam,
                              keys_reviews_removed){
  if(keys_reviews_removed==F ){
    return(list(sum(positive_onSteam,
                    positive_offSteam),
                sum(negative_onSteam,
                    negative_offSteam),
                sum(total_onSteam,
                    total_offSteam)))
  }else if(keys_reviews_removed==T){
    return(list(positive_onSteam,
                negative_onSteam,
                total_onSteam))
  }
}



# For creating a panel with the daily player count, this function interpolates
# the price data. If a price change took place on day t, fill all the days up to
# the next change with the same price.
fillMissingPrices <- function(vec){
  n <- length(vec)
  price.pos <- (1:n)[!is.na(vec)]
  if(length(price.pos) == 0){
    return(vec)
  }
  else{
    rep.times <- diff(c(price.pos, n+1))
    mapply(rep, vec[price.pos], rep.times) %>% 
      unlist() %>%
      as.vector() %>%
      c(rep(NA, price.pos[1]-1), .) %>%
      return()
  }
}



# Take the vector of prices in, and for every observation give back the panel time 
# of the first price change, if any will take place in the future. Returns NA, else.
timeOfNextPriceChange<- function(vec, t){
  n <- length(vec)
  vec <- c(NA, diff(vec))
  # drop the first observation as it will be NA
  p.change.pos <- t[vec != 0]
  p.change.pos <- p.change.pos[!is.na(p.change.pos)]
  if(length(p.change.pos) == 0){return(rep(NA, n))}
  rep.times <- diff(c(t[1], p.change.pos, t[n]+1))
  mapply(rep, c(p.change.pos, NA), rep.times) %>%
    unlist() %>%
    return()
}