###################################################
# PRODUCE ALL TABLES IN THE PAPER
###################################################

## Preamble: set working directory

rm(list = ls())
setwd("D:/ColorFinanceWords")

## packages
require(tibble)
require(dplyr)
require(readr)
require(lfe)
require(stargazer)
require(DescTools)
require(tm)
require(slam)
require(ggplot2)
require(ggthemes)
require(Matrix)

##########################################
## Function to compute sentiment 
##########################################

## compute sentiment
computeSentiment <- function(
    dict,
    dtm,
    normalize = T, 
    winsorize = T
){
  ## compute sentiment
  sent <- row_sums(dtm[, colnames(dtm) %in% dict], na.rm=T) / row_sums(dtm)
  
  ## normalize
  if(normalize){sent <- sent/sd(sent, na.rm = T)}
  
  ## winsorize
  if(winsorize){sent <- DescTools::Winsorize(sent, probs = c(0.01, 0.99), na.rm = T)}
  
  ## output
  return(sent)
}

##########################################
# Function to format (round) output
##########################################

print_dec <- function(r,n){
  res <- format(round(r, n), nsmall=n, big.mark = ",")
  res[is.na(r)] <- ""
  res}

###########################################################
# Table 2: Horse race regression - earnings calls
###########################################################

## We note this function will not run without an updated
## metadata file (relative to what we share in the repository).
## The PERMNO+ information provided in the repository 
## should make this easily available to many researchers.

## Load data
load("data/meta_event.RData")
load('./data/event_dtm.RData')
dtmEve <- dtm
rm(dtm)
load("data/LM_2021.RData")

## ML dictionaries
ML_pos_event <- readLines("dictionary/ML_positive_con.txt")
ML_neg_event <- readLines("dictionary/ML_negative_con.txt")
ML_pos_dxm <- readLines("dictionary/ML_positive_unigram.txt")
ML_neg_dxm <- readLines("dictionary/ML_negative_unigram.txt")

## Winsorize all variables used in the regression
for(var in c("filing.period.excess.return", "size", "bm", 
             "share.turnover", "Pre_FFAlpha")){
  meta[[var]] <- DescTools::Winsorize(meta[[var]], probs = c(0.01, 0.99))}

## Compute sentiment
meta$LM_pos_event <- computeSentiment(setdiff(LMpos, ML_pos_event), dtmEve)
meta$LM_neg_event <- computeSentiment(setdiff(LMneg, ML_neg_event), dtmEve)

meta$LM_pos_dxm <- computeSentiment(setdiff(LMpos, ML_pos_dxm), dtmEve)
meta$LM_neg_dxm <- computeSentiment(setdiff(LMneg, ML_neg_dxm), dtmEve)

meta$ML_pos_event <- computeSentiment(setdiff(ML_pos_event, LMpos), dtmEve)
meta$ML_neg_event <- computeSentiment(setdiff(ML_neg_event, LMneg), dtmEve)

meta$ML_pos_dxm <- computeSentiment(setdiff(ML_pos_dxm, LMpos), dtmEve)
meta$ML_neg_dxm <- computeSentiment(setdiff(ML_neg_dxm, LMneg), dtmEve)

meta$ML_pos_event_dxm <-computeSentiment(setdiff(ML_pos_event, ML_pos_dxm), dtmEve)
meta$ML_neg_event_dxm <-computeSentiment(setdiff(ML_neg_event, ML_neg_dxm), dtmEve)

meta$ML_LM_pos_event <- computeSentiment(intersect(ML_pos_event, LMpos), dtmEve)
meta$ML_LM_neg_event <- computeSentiment(intersect(ML_neg_event, LMneg), dtmEve)

meta$ML_LM_pos_dxm <- computeSentiment(intersect(ML_pos_dxm, LMpos), dtmEve)
meta$ML_LM_neg_dxm <- computeSentiment(intersect(ML_neg_dxm, LMneg), dtmEve)

meta$ML_ML_pos <- computeSentiment(intersect(ML_pos_dxm, ML_pos_event), dtmEve)
meta$ML_ML_neg <- computeSentiment(intersect(ML_neg_dxm, ML_neg_event), dtmEve)


## Regressions

reg <- list()

reg[[1]] <- felm(
  filing.period.excess.return ~
    + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta))

reg[[2]] <- felm(
  filing.period.excess.return ~
    + LM_pos_event + LM_neg_event
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[3]] <- felm(
  filing.period.excess.return ~
    + ML_pos_event + ML_neg_event
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[4]] <- felm(
  filing.period.excess.return ~
    + LM_pos_event + LM_neg_event
    + ML_pos_event + ML_neg_event
    + ML_LM_pos_event + ML_LM_neg_event
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

stargazer(reg, 
          type = "text", 
          digits = 4, 
          align = T
          )

reg <- list()

reg[[1]] <- felm(
  filing.period.excess.return ~
    + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta))

reg[[2]] <- felm(
  filing.period.excess.return ~
    + ML_pos_dxm + ML_neg_dxm
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[3]] <- felm(
  filing.period.excess.return ~
    + ML_pos_event + ML_neg_event
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[4]] <- felm(
  filing.period.excess.return ~
    + ML_pos_dxm + ML_neg_dxm
  + ML_pos_event + ML_neg_event
  + ML_ML_pos + ML_ML_neg
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

stargazer(reg, 
          type = "text", 
          digits = 4, 
          align = T
)

reg <- list()

reg[[1]] <- felm(
  filing.period.excess.return ~
    + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta))

reg[[2]] <- felm(
  filing.period.excess.return ~
    + LM_pos_dxm + LM_neg_dxm
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[3]] <- felm(
  filing.period.excess.return ~
    + ML_pos_dxm + ML_neg_dxm
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[4]] <- felm(
  filing.period.excess.return ~
    + LM_pos_dxm + LM_neg_dxm
  + ML_pos_dxm + ML_neg_dxm
  + ML_LM_pos_dxm + ML_LM_neg_dxm
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

stargazer(reg, 
          type = "text", 
          digits = 4, 
          align = T
)
