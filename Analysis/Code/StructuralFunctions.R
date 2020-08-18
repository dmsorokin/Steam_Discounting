# Define some variables
one.year <- anydate("2016-01-01") - anydate("2015-01-01")
one.day<- anydate("2014-01-02") - anydate("2014-01-01")
sampleStartDate <- "2017-01-01"
sampleFinalDate <- anydate("2019-01-01")

# The color scheme
my.colors <- c("#1b85b8",  "#cc3399", "#007f66",  "#f27d0c", "#cc2a36")

# The style function has two arguments that specify the location of the legend
# in graphs that need one. The defaults are specified, but feel free to change
# the location if the graph looks better that way
my.theme <- function(leg.x = 0.75, leg.y = 0.1){
  theme_bw() +
    theme(panel.background = element_rect(),
    text = element_text(family="Times",size=11),
    plot.title = element_text(size=10, family="Times", face="bold",
                              #hjust=0.5,
                              lineheight=2),
    axis.title.x = element_text(family="Times",size=11),  
    axis.title.y = element_text(family="Times",size=11),  
    legend.text  = element_text(family="Times",size=10),
    legend.title = element_text(size=10,family="Times",
                                face="bold"),
    legend.position = c(leg.x, leg.y),
    legend.direction = "horizontal",
    legend.background = element_rect(
      fill = "white",
      size = 0.25,
      linetype="solid",
      colour = "black"))
}

splitPriceDuration <- function(x){
  # Break the duration of the price into 7-day periods, and return the vector with
  # the number of days (x7) that needs to be added to the date to make sure each
  # week has a date that we can use to duplicate the price.
  # Example: price duration is 22 days. Return a vector (7, 14, 21) (days), that
  # could then be added to the date of the price change. This way every week
  # will have price information.

  # Add n.weeks rows
  n.weeks <- x[, p.dur] %>% as.integer(.) %/% 7
  switch(as.integer(x[, p.dur]) %% 7 == 0, n.weeks <- n.weeks - 1)
  
  # 7 day step
  step <- anydate("1992-01-08") - anydate("1992-01-01")
  
  # Duplicate the input row, to create new etnries (and later change the dates)
  #ans <- x[rep(1, n.weeks), ]
  steps <- rep(step, n.weeks)

  for(i in 1:n.weeks){
    steps[i] <- steps[i] + (i-1)*step
  }

  return(steps)
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

# Create multiple leads of a variable without assigning. Usage:
# dt[, cbind(v1, v2, multLeads(v3, 3))]
multLeads <- function(var, num){
  data.table(
    matrix(unlist(shift(var, n = 1:num, type = "lead")), 
           ncol = num,
           dimnames = list(NULL, paste(
            paste(deparse(substitute(var)), "lead", sep = "."),
            as.character(1:num),
            sep = "_")
            )))
}

# A convenience function for easier rendering of the file
# It redefines stargazertype to match the desired extension of 
# the uptup file, and restores it back, if there was one to 
# begin with
knitScript <- function(name, extension="html"){
  if(exists("stargazertype")){
    sgt<-stargazertype
  }
  stargazertype <- ifelse(extension=="latext","latex","text")
  render(here("Analysis", "Code", paste(name, "R", sep=".")), 
         output_file = here("Analysis", "Output", "Structural",
                            paste(name, extension, sep=".")))
  if(exists("sgt")){
    stargazertype <- sgt
  }
  else{
    rm(stargazertype)
  }
}

# A convenience function for easier rendering of the file
knitPaper <- function(name){
  render(here("Analysis","Code","Structural", paste(name, "R", sep=".")), 
         output_file = here("Analysis","Code","Structural",
                            paste(name, "pdf", sep=".")))
}

# A convenience function for easier conversion of panel times to real dates
getDate <- function(t, origin = anydate(sampleStartDate)){
  origin + t*one.day
}


plotGame <- function(id){
  p1 <- ggplot(data = panel[ID == id, .(t, y = log(number))]) +
  ggtitle(paste("Player Activity For ID", as.character(id), sep = " = ")) +
  geom_vline(xintercept=as.integer(info[ID==id,releaseDate]-anydate("2017-01-01")),
             linetype = "dashed") +
  geom_vline(xintercept=as.integer(info[ID==id,relDateEA]-anydate("2017-01-01")),
             linetype = "dashed") +
  geom_line(aes(x = t, y = y),
            color = my.colors[1],
            size = 1) +
  scale_x_continuous(name = NULL,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Log Players",
                     minor_breaks = NULL)  +
  my.theme(leg.x = 0.25, leg.y = 0.9)

p2 <- ggplot(data = panel[ID == id, .(t, y = price)]) +
  ggtitle(paste("Prices For ID", as.character(id), sep = " = ")) +
  geom_line(aes(x = t, y = y),
            color = my.colors[1],
            size = 1) +
  scale_x_continuous(name = NULL,
                     minor_breaks = NULL) +
  scale_y_continuous(name = "$",
                     minor_breaks = NULL) +
  my.theme(leg.x = 0.25, leg.y = 0.9)

p3 <- ggplot(data = panel[ID==id, .(t, score)]) +
  ggtitle(paste("Review Score For ID", as.character(id), sep = " = ")) +
  geom_line(aes(x = t, y = score),
            color = my.colors[1],
            size = 1) +
  # geom_line(aes(x = t, y = rsAll),
  #           color = my.colors[2],
  #           size = 0.8,
  #           linetype = "dashed")+
   scale_x_continuous(name = "Days Since 2017-01-01 (t)",
                     minor_breaks = NULL) +
   scale_y_continuous(name = "% of Reviews",
                     minor_breaks = NULL) +
   my.theme(leg.x = 0.25, leg.y = 0.9) +
   scale_size_manual(values=c(1.2, 0.8), name=NULL, labels=NULL)

ggarrange(p1, p2, p3, ncol = 1, nrow = 3, align = "v")}





# take the vector of tags, select the games for which at least one tag is true
getIDsByTags <- function(tagVector, tagTable = tags){
  tagVector <- as.character(tagVector)
  subtags <- tagTable[, c("ID", ..tagVector)]
  subtags[, .(isMult = Reduce(`|`, .SD)), by = ID][isMult == T, ID] %>% return
}

# Get a list of tags that the games in the IDVEctor have
getTags <- function(IDVector, tagTable = tags){
  mTags <- tagTable[ID %in% IDVector, !c("ID")] %>% transpose()
  f <- lapply(mTags, function(x) names(tagTable)[-1][x])
  names(f) <- IDVector
  return(f)
}

# Get translation of tags to English
translateTags <- function(codes, showCodes = T){
  columns <- names(tagDict)[c(showCodes, T)]
  tagDict[code  %in% as.integer(codes), ..columns]
}

# take a data frame with estimation results and create a 
# Texreg object to be later parsed into a nice Latex table
dataFrameToTexreg <- function(regFrame, statsFrame){
  coef.row.indices <- seq(1, nrow(regFrame), 3)
  se.row.indices <- seq(2, nrow(regFrame), 3)
  pval.row.indices <- seq(3, nrow(regFrame), 3)
  gofnames <- statsFrame[, 1]
  texregObjects <- list()

  for (i in 2:ncol(regFrame)){
    coefs <- regFrame[coef.row.indices, i]
    coefnames <- as.character(regFrame[coef.row.indices, 1])
    sevalues <- regFrame[se.row.indices, i]
    pvalues <- regFrame[pval.row.indices, i]
    gofvalues <- statsFrame[, i]
    tr <- createTexreg(coef = coefs, 
                       coef.names = coefnames,
                       se = sevalues,
                       pvalues = pvalues,
                       gof.names = gofnames,
                       gof = gofvalues)
    texregObjects[i - 1] <- list(tr)
  }
  return(texregObjects)
}


plmJuliaToTex <- function(r1, r2, juliaFrame, juliaStats){
  
  # First the Julia regression
  coef.row.indices <- seq(1, nrow(juliaFrame), 3)
  se.row.indices <- seq(2, nrow(juliaFrame), 3)
  pval.row.indices <- seq(3, nrow(juliaFrame), 3)
  gofnames <- c("Weekdays", "Week", "Observations", "R$^2$")
  texregObjects <- list()

  coefs <- juliaFrame[coef.row.indices, 2]
  # I had normalized the age and the score vars for Julia
  coefs[3] <- coefs[3]/100 
  coefs[5] <- coefs[5]/10 
  coefnames <- c("Log Price", "New Discount", "Age", 
                 "Age $\\le 14$", "Score", "No Score",
                 "Negative", "M. Positive", "Positive", 
                 "V. Positive", "Ov. Positive", "Log Reviews",
                 "Seasonal Sale")
  sevalues <- juliaFrame[se.row.indices, 2]
  # s.e. also have to be renormalized
  sevalues[3] <- sevalues[3]/100
  sevalues[5] <- sevalues[5]/10
  pvalues <- juliaFrame[pval.row.indices, 2]
  gofvalues <- c(1., 0., juliaStats[2:3, 2])
  
  # Create a Texreg object with Julia results. Note: goodnesss 
  # of fit has to be numeric
  tr <- createTexreg(coef = coefs, 
                     coef.names = coefnames,
                     se = sevalues,
                     pvalues = pvalues,
                     gof.names = gofnames,
                     gof = gofvalues)
  texregObjects[1] <- list(tr)
  
  
  # The  first plm regression 
  coefnames <- c("Lag Players", "Log Price", "New Discount",
                 "Seasonal Sale",
                 "No Score", "Negative", "M. Positive", "Positive", 
                 "V. Positive", "Ov. Positive", "Log Reviews", 
                 "Score", "Age", "Age $\\le 14$")
  coefs <- r1$coefficients[1:14]
  
  # calculate the s.e. and p. values
  cov<-vcovHC(r1, method="white1")
  test <- coeftest(r1, vcov. = cov)
  sevalues <- test[1:14, 2]
  pvalues <- test[1:14, 4]
  f <- summary(r1)
  
  tr <- createTexreg(coef = coefs,
                     coef.names = coefnames,
                     se = sevalues,
                     pvalues = pvalues,
                     gof.names = gofnames,
                     gof = c(1., 1., length(f$residuals),
                             f$r.squared[1]))
  texregObjects[2] <- list(tr)
  
  
  # The plm regression 
  coefnames <- c("Lag Players", "Log Price", "New Discount",
                 "No Score", "Negative", "M. Positive", "Positive", 
                 "V. Positive", "Ov. Positive", "Log Reviews", 
                 "Score", "Age", "Age $\\le 14$")
  coefs <- r2$coefficients[1:13]
  
  # calculate the s.e. and p. values
  cov<-vcovHC(r2, method="white1")
  test <- coeftest(r2, vcov. = cov)
  sevalues <- test[1:13, 2]
  pvalues <- test[1:13, 4]
  f <- summary(r2)
  
  tr <- createTexreg(coef = coefs,
                     coef.names = coefnames,
                     se = sevalues,
                     pvalues = pvalues,
                     gof.names = gofnames,
                     gof = c(1., 1., length(f$residuals),
                             f$r.squared[1]))
  texregObjects[3] <- list(tr)
  
  return(texregObjects)
}








