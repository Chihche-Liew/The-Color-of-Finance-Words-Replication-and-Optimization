###################################################
# PRODUCE ALL TABLES IN THE PAPER
###################################################

## Preamble: set working directory

rm(list = ls())
setwd()

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
  if(normalize){sent <- sent/sd(sent)}
  
  ## winsorize
  if(winsorize){sent <- DescTools::Winsorize(sent, probs = c(0.01, 0.99))}
  
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

##########################################
## Table 1: Corpus overview 
##########################################

## Because we do not share the full corpus 
## but limit to the top 16K/65K uni/bigrams,
## the numbers displayed below will not match
## the paper exactly.

## Earnings calls
load("data/meta.RData")
load("data/dtm_unigram.RData")

min(meta$callDate)
max(meta$callDate)
length(unique(meta$permno))
nrow(meta)
row_sums(dtm) %>% mean

## 10K 
load("data/meta_10K.RData")
load("data/dtm_unigram_10K.RData")

min(meta$filing_date)
max(meta$filing_date)
length(unique(meta$permno))
nrow(meta)
row_sums(dtm) %>% mean

## WSJ
load("data/meta_WSJ.RData")
load("data/dtm_unigram_WSJ.RData")

min(meta$PublishingDateTradingDay)
max(meta$PublishingDateTradingDay)
length(unique(meta$permno))
sum(meta$nrArticles)
nrow(meta)
row_sums(dtm) %>% mean

## delete
rm(list = ls())

###########################################################
# Table 2: Horse race regression - earnings calls
###########################################################

## We note this function will not run without an updated
## metadata file (relative to what we share in the repository).
## The PERMNO+ information provided in the repository 
## should make this easily available to many researchers.

## Load data
load("data/meta.RData")
load("data/LM_2021.RData") 
load("data/dtm_unigram.RData")
dtmUni <- dtm
load("data/dtm_bigram.RData")
dtmBi <- dtm
rm(dtm)

## ML dictionaries
ML_pos <- readLines("dictionary/ML_positive_unigram_1.txt")
ML_neg <- readLines("dictionary/ML_negative_unigram_1.txt")
ML_pos_bi <- readLines("dictionary/ML_positive_bigram_1.txt")
ML_neg_bi <- readLines("dictionary/ML_negative_bigram_1.txt")


## Winsorize all variables used in the regression
for(var in c("filing.period.excess.return", "size", "bm", 
             "share.turnover", "Pre_FFAlpha")){
  meta[[var]] <- DescTools::Winsorize(meta[[var]], probs = c(0.01, 0.99))}

## Compute sentiment
meta$ML_LM_pos <- computeSentiment(intersect(ML_pos, LMpos), dtmUni)
meta$ML_LM_neg <- computeSentiment(intersect(ML_neg, LMneg), dtmUni)

meta$ML_pos <- computeSentiment(setdiff(ML_pos, LMpos), dtmUni)
meta$ML_neg <- computeSentiment(setdiff(ML_neg, LMneg), dtmUni)

meta$LM_pos <- computeSentiment(setdiff(LMpos, ML_pos), dtmUni)
meta$LM_neg <- computeSentiment(setdiff(LMneg, ML_neg), dtmUni)

meta$ML_pos_bi <- computeSentiment(ML_pos_bi, dtmBi)
meta$ML_neg_bi <- computeSentiment(ML_neg_bi, dtmBi)

## take out data used for training
ix <- meta$callDate > "2015-12-31" 
meta <- meta[ix,]

## Regressions

reg <- list()

reg[[1]] <- felm(
  filing.period.excess.return ~
    + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta))

reg[[2]] <- felm(
  filing.period.excess.return ~
    + LM_pos + LM_neg
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[3]] <- felm(
  filing.period.excess.return ~
    + ML_pos + ML_neg
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[4]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[5]] <- felm(
  filing.period.excess.return ~
    + ML_pos_bi + ML_neg_bi
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[6]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + ML_pos + ML_neg
  + LM_pos + LM_neg
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

reg[[7]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + LM_pos + LM_neg
  + ML_pos_bi + ML_neg_bi
  + log(size) + log(bm) + log(share.turnover) + sue + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + yearQuarter | 0 | FF.49 + yearQuarter,
  data = filter(meta)
)

## regression output
stargazer(reg, 
          type = "latex", 
          keep.stat = c("n"),
          digits = 2, 
          digits.extra = 0,
          align = T, 
          no.space = T,
          report = "vc*t")

##############################################
## Table 3: External validity - 10K filings
##############################################

## Load data
load("data/meta_10K.RData")
load("data/LM_2021.RData") 
load("data/dtm_unigram_10K.RData")
dtmUni <- dtm
load("data/dtm_bigram_10K.RData")
dtmBi <- dtm
rm(dtm)

## ML dictionaries
ML_pos <- readLines("dictionary/ML_positive_unigram.txt")
ML_neg <- readLines("dictionary/ML_negative_unigram.txt")
ML_pos_bi <- readLines("dictionary/ML_positive_bigram.txt")
ML_neg_bi <- readLines("dictionary/ML_negative_bigram.txt")

## compute Sentiment
meta$ML_pos_full <- computeSentiment(ML_pos, dtmUni)
meta$ML_neg_full <- computeSentiment(ML_neg, dtmUni)

meta$ML_LM_pos <- computeSentiment(intersect(ML_pos, LMpos), dtmUni)
meta$ML_LM_neg <- computeSentiment(intersect(ML_neg, LMneg), dtmUni)

meta$ML_pos <- computeSentiment(setdiff(ML_pos, LMpos), dtmUni)
meta$ML_neg <- computeSentiment(setdiff(ML_neg, LMneg), dtmUni)

meta$LM_pos <- computeSentiment(setdiff(LMpos, ML_pos), dtmUni)
meta$LM_neg <- computeSentiment(setdiff(LMneg, ML_neg), dtmUni)

meta$ML_pos_bi <- computeSentiment(ML_pos_bi, dtmBi)
meta$ML_neg_bi <- computeSentiment(ML_neg_bi, dtmBi)

## Winsorize all variables used in the regression
for(var in c("filing.period.excess.return", "size", "bm", 
             "share.turnover", "Pre_FFAlpha")){
  meta[[var]] <- DescTools::Winsorize(meta[[var]], probs = c(0.01, 0.99))}

## Regressions
reg <- list()

reg[[1]] <- felm(
  filing.period.excess.return ~
    + log(size) + log(bm) + log(share.turnover) + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + year_q  | 0  | FF.49 + year_q,
  data = filter(meta)
)

reg[[2]] <- felm(
  filing.period.excess.return ~
    + LM_pos + LM_neg
  + log(size) + log(bm) + log(share.turnover) + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + year_q  | 0  | FF.49 + year_q,
  data = filter(meta)
)

reg[[3]] <- felm(
  filing.period.excess.return ~
    + ML_pos + ML_neg
  + log(size) + log(bm) + log(share.turnover) + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + year_q  | 0  | FF.49 + year_q,
  data = filter(meta)
)

reg[[4]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + log(size) + log(bm) + log(share.turnover) + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + year_q  | 0  | FF.49 + year_q,
  data = filter(meta)
)

reg[[5]] <- felm(
  filing.period.excess.return ~
    + ML_pos_bi + ML_neg_bi
  + log(size) + log(bm) + log(share.turnover) + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + year_q  | 0  | FF.49 + year_q,
  data = filter(meta)
) 

reg[[6]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + ML_pos + ML_neg
  + LM_pos + LM_neg
  + log(size) + log(bm) + log(share.turnover) + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + year_q  | 0  | FF.49 + year_q,
  data = filter(meta)
)

reg[[7]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + LM_pos + LM_neg
  + ML_pos_bi + ML_neg_bi
  + log(size) + log(bm) + log(share.turnover) + Pre_FFAlpha + nasdaq.dummy
  | FF.49 + year_q  | 0  | FF.49 + year_q,
  data = filter(meta)
)

## regression output
stargazer(reg, 
          type = "text", 
          keep.stat = c("n"),
          digits = 2, 
          digits.extra = 0,
          align = T, 
          no.space = T,
          report = "vc*t")

#################################################################
## Table 4: External validity - WSJ articles
#################################################################

## load data
load("data/meta_WSJ.RData")
load("data/LM_2021.RData") 
load("data/dtm_unigram_WSJ.RData")
dtmUni <- dtm
load("data/dtm_bigram_WSJ.RData")
dtmBi <- dtm
rm(dtm)

## ML dictionaries
ML_pos <- readLines("dictionary/ML_positive_unigram.txt")
ML_neg <- readLines("dictionary/ML_negative_unigram.txt")
ML_pos_bi <- readLines("dictionary/ML_positive_bigram.txt")
ML_neg_bi <- readLines("dictionary/ML_negative_bigram.txt")

## compute Sentiment
meta$ML_pos_full <- computeSentiment(ML_pos, dtmUni)
meta$ML_neg_full <- computeSentiment(ML_neg, dtmUni)

meta$ML_LM_pos <- computeSentiment(intersect(ML_pos, LMpos), dtmUni)
meta$ML_LM_neg <- computeSentiment(intersect(ML_neg, LMneg), dtmUni)

meta$ML_pos <- computeSentiment(setdiff(ML_pos, LMpos), dtmUni)
meta$ML_neg <- computeSentiment(setdiff(ML_neg, LMneg), dtmUni)

meta$LM_pos <- computeSentiment(setdiff(LMpos, ML_pos), dtmUni)
meta$LM_neg <- computeSentiment(setdiff(LMneg, ML_neg), dtmUni)

meta$ML_pos_bi <- computeSentiment(ML_pos_bi, dtmBi)
meta$ML_neg_bi <- computeSentiment(ML_neg_bi, dtmBi)

## Winsorize all variables used in the regression
for(var in c("filing.period.excess.return")){
  meta[[var]] <- DescTools::Winsorize(meta[[var]], probs = c(0.01, 0.99))}

## Regressions
reg <- list()

reg[[1]] <- felm(
  filing.period.excess.return ~ 1
  | weekday + yearMonth + permno  | 0  | yearMonth + permno, 
  data = meta
)

reg[[2]] <- felm(
  filing.period.excess.return ~
    + LM_pos + LM_neg
  | weekday + yearMonth + permno  | 0  | yearMonth + permno, 
  data = meta
)

reg[[3]] <- felm(
  filing.period.excess.return ~
    + ML_pos + ML_neg
  | weekday + yearMonth + permno  | 0  | yearMonth + permno, 
  data = filter(meta)
)

reg[[4]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  | weekday + yearMonth + permno  | 0  | yearMonth + permno, 
  data = filter(meta)
)

reg[[5]] <- felm(
  filing.period.excess.return ~
    + ML_pos_bi + ML_neg_bi
  | weekday + yearMonth + permno  | 0  | yearMonth + permno, 
  data = filter(meta)
) 

reg[[6]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + ML_pos + ML_neg
  + LM_pos + LM_neg
  | weekday + yearMonth + permno  | 0  | yearMonth + permno, 
  data = filter(meta)
)

reg[[7]] <- felm(
  filing.period.excess.return ~
    ML_LM_pos + ML_LM_neg
  + LM_pos + LM_neg
  + ML_pos_bi + ML_neg_bi
  | weekday + yearMonth + permno  | 0  | yearMonth + permno, 
  data = filter(meta)
)

## regression output
stargazer(reg[-1], 
          type = "text", 
          keep.stat = c("n"),
          digits = 2, 
          digits.extra = 0,
          align = T, 
          no.space = T,
          report = "vc*t")

######################################################
## Table 5: Dictionary Breadth
######################################################

## ML dictionaries
ML_pos <- readLines("dictionary/ML_positive_unigram.txt")
ML_neg <- readLines("dictionary/ML_negative_unigram.txt")
ML_pos_bi <- readLines("dictionary/ML_positive_bigram.txt")
ML_neg_bi <- readLines("dictionary/ML_negative_bigram.txt")

## LM dictionary
load("data/LM_2021.RData")

## Earnings calls
load("data/dtm_unigram.RData")
dtmUni <- dtm
load("data/dtm_bigram.RData")
dtmBi <- dtm
rm(dtm)

LMposCovCalls <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(LMpos, ML_pos)]) / sum(dtmUni$v)
LMnegCovCalls <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(LMneg, ML_neg)]) / sum(dtmUni$v)
MLposCovCalls <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(ML_pos, LMpos)]) / sum(dtmUni$v)
MLnegCovCalls <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(ML_neg, LMneg)]) / sum(dtmUni$v)
LMMLposCovCalls <- sum(dtmUni[,dtmUni$dimnames$Terms %in% intersect(LMpos, ML_pos)]) / sum(dtmUni$v)
LMMLnegCovCalls <- sum(dtmUni[,dtmUni$dimnames$Terms %in% intersect(LMneg, ML_neg)]) / sum(dtmUni$v)
MLposBiCovCalls <- sum(dtmBi[,dtmBi$dimnames$Terms %in% ML_pos_bi]) / sum(dtmBi$v)
MLnegBiCovCalls <- sum(dtmBi[,dtmBi$dimnames$Terms %in% ML_neg_bi]) / sum(dtmBi$v)

## 10-Ks
load("data/dtm_unigram_10K.RData")
dtmUni <- dtm
load("data/dtm_bigram_10K.RData")
dtmBi <- dtm
rm(dtm)

LMposCov10K <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(LMpos, ML_pos)]) / sum(dtmUni$v)
LMnegCov10K <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(LMneg, ML_neg)]) / sum(dtmUni$v)
MLposCov10K <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(ML_pos, LMpos)]) / sum(dtmUni$v)
MLnegCov10K <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(ML_neg, LMneg)]) / sum(dtmUni$v)
LMMLposCov10K <- sum(dtmUni[,dtmUni$dimnames$Terms %in% intersect(LMpos, ML_pos)]) / sum(dtmUni$v)
LMMLnegCov10K <- sum(dtmUni[,dtmUni$dimnames$Terms %in% intersect(LMneg, ML_neg)]) / sum(dtmUni$v)
MLposBiCov10K <- sum(dtmBi[,dtmBi$dimnames$Terms %in% ML_pos_bi]) / sum(dtmBi$v)
MLnegBiCov10K <- sum(dtmBi[,dtmBi$dimnames$Terms %in% ML_neg_bi]) / sum(dtmBi$v)

## WSJ
load("data/dtm_unigram_WSJ.RData")
dtmUni <- dtm
load("data/dtm_bigram_WSJ.RData")
dtmBi <- dtm
rm(dtm)

LMposCovWSJ <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(LMpos, ML_pos)]) / sum(dtmUni$v)
LMnegCovWSJ <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(LMneg, ML_neg)]) / sum(dtmUni$v)
MLposCovWSJ <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(ML_pos, LMpos)]) / sum(dtmUni$v)
MLnegCovWSJ <- sum(dtmUni[,dtmUni$dimnames$Terms %in% setdiff(ML_neg, LMneg)]) / sum(dtmUni$v)
LMMLposCovWSJ <- sum(dtmUni[,dtmUni$dimnames$Terms %in% intersect(LMpos, ML_pos)]) / sum(dtmUni$v)
LMMLnegCovWSJ <- sum(dtmUni[,dtmUni$dimnames$Terms %in% intersect(LMneg, ML_neg)]) / sum(dtmUni$v)
MLposBiCovWSJ <- sum(dtmBi[,dtmBi$dimnames$Terms %in% ML_pos_bi]) / sum(dtmBi$v)
MLnegBiCovWSJ <- sum(dtmBi[,dtmBi$dimnames$Terms %in% ML_neg_bi]) / sum(dtmBi$v)

## Output positive dictionaries
rbind(
  c("LM positive", 
    print_dec(length(setdiff(LMpos, ML_pos)), 0), 
    print_dec(LMposCovCalls*100, 1), 
    print_dec(LMposCov10K*100, 1), 
    print_dec(LMposCovWSJ*100, 1))
  , 
  c("ML positive", 
    print_dec(length(setdiff(ML_pos, LMpos)), 0), 
    print_dec(MLposCovCalls*100, 1), 
    print_dec(MLposCov10K*100, 1), 
    print_dec(MLposCovWSJ*100, 1))
  ,
  c("ML \\& LM positive", 
    print_dec(length(intersect(LMpos, ML_pos)), 0), 
    print_dec(LMMLposCovCalls*100, 1), 
    print_dec(LMMLposCov10K*100, 1), 
    print_dec(LMMLposCovWSJ*100, 1))
  ,
  c("ML positive bigram", 
    print_dec(length(ML_pos_bi), 0), 
    print_dec(MLposBiCovCalls*100, 1), 
    print_dec(MLposBiCov10K*100, 1), 
    print_dec(MLposBiCovWSJ*100, 1))
) -> tempPos

## Output negative dictionaries
rbind(
  c("LM negative", 
    print_dec(length(setdiff(LMneg, ML_neg)), 0), 
    print_dec(LMnegCovCalls*100, 1), 
    print_dec(LMnegCov10K*100, 1), 
    print_dec(LMnegCovWSJ*100, 1))
  , 
  c("ML negative", 
    print_dec(length(setdiff(ML_neg, LMneg)), 0), 
    print_dec(MLnegCovCalls*100, 1), 
    print_dec(MLnegCov10K*100, 1), 
    print_dec(MLnegCovWSJ*100, 1))
  ,
  c("ML \\& LM negative", 
    print_dec(length(intersect(LMneg, ML_neg)), 0), 
    print_dec(LMMLnegCovCalls*100, 1), 
    print_dec(LMMLnegCov10K*100, 1), 
    print_dec(LMMLnegCovWSJ*100, 1))
  ,
  c("ML negative bigram", 
    print_dec(length(ML_neg_bi), 0), 
    print_dec(MLnegBiCovCalls*100, 1), 
    print_dec(MLnegBiCov10K*100, 1), 
    print_dec(MLnegBiCovWSJ*100, 1))
) -> tempNeg

## Add %
tempPos[,3:5] <- apply(tempPos[,3:5], 2, paste0, "%") 
tempNeg[,3:5] <- apply(tempNeg[,3:5], 2, paste0, "%") 

## Output
tempPos 
tempNeg 

##############################################
## Prepare Table 6-8
##############################################

## Robust MNIR
MNIRest <- read_csv("robustMNIR/ML_score_unigram.csv")
MNIRestBi <- read_csv("robustMNIR/ML_score_bigram.csv")

## Recalculate frequency
MNIRest %>% 
  mutate(freqBp = print_dec(exp(freqRelLog)* 10000, 1)) -> MNIRest
                            
## Identify unigrams
MNIRestBi %>% 
  mutate(word1 = gsub("\\s\\w+$", "", word),
         word2 = gsub("^\\w+\\s", "", word)) -> MNIRestBi

## Dictionaries
load("data/LM_2021.RData")
ML_pos <- readLines("dictionary/ML_positive_unigram.txt")
ML_neg <- readLines("dictionary/ML_negative_unigram.txt")
ML_pos_bi <- readLines("dictionary/ML_positive_bigram.txt")
ML_neg_bi <- readLines("dictionary/ML_negative_bigram.txt")

##########################################################
## Table 6: Top LM unigrams and ML scores
##########################################################

## positive side
MNIRest %>% 
  filter(word %in% LMpos) %>% 
  arrange(-freqRelLog) %>% 
  select(-freqRelLog, -freq) 

## negative side
MNIRest %>% 
  filter(word %in% LMneg) %>% 
  arrange(-freqRelLog) %>% 
  select(-freqRelLog, -freq) 

##########################################################
## Table 7: Top ML unigrams by frequency
##########################################################

## positive side
MNIRest %>% 
  filter(word %in% ML_pos) %>% 
  arrange(-freq) %>% 
  select(-freq, -freqRelLog)  

## negative side
MNIRest %>% 
  filter(word %in% ML_neg) %>% 
  arrange(-freq) %>% 
  select(-freq, -freqRelLog)  


##########################################################
## Table 8: Disambiguating unigrams 
##########################################################

## Indicate which panel in Table 8 should be produced
term <- "improve"
## term <- "confident"
## term <- "solid"
## term <- "soft"
## term <- "cash"
## term <- "continue"

## positive
MNIRestBi %>% 
  filter(word1 %in% term | word2 %in% term) %>% 
  mutate(relFreq = print_dec(freq / sum(freq) * 100, 2)) %>% 
  mutate(D_score = positive - negative) %>% 
  filter(D_score > .2) %>% 
  arrange(-freq) %>%
  select(word, relFreq, D_score) %>% 
  head(5)

## negative 
MNIRestBi %>% 
  filter(word1 %in% term | word2 %in% term) %>% 
  mutate(relFreq = print_dec(freq / sum(freq) * 100, 2)) %>% 
  mutate(D_score = positive - negative) %>% 
  filter(D_score < -.2) %>% 
  arrange(-freq) %>%
  select(word, relFreq, D_score) %>% 
  head(5)

############################################################
## Table 9: Disambiguation: bigrams starting with not
############################################################

term <- "not"

## positive
MNIRestBi %>% 
  filter(word1 %in% term ) %>% 
  mutate(relFreq = print_dec(freq / sum(freq) * 100, 2)) %>% 
  mutate(D_score = positive - negative) %>% 
  arrange(D_score) %>%
  head(25) %>% 
  arrange(-freq) %>% 
  select(word, relFreq, D_score) %>% 
  as.matrix()

## negative 
MNIRestBi %>% 
  filter(word1 %in% term ) %>% 
  mutate(relFreq = print_dec(freq / sum(freq) * 100, 2)) %>% 
  mutate(D_score = positive - negative) %>% 
  arrange(-D_score) %>%
  head(25) %>% 
  arrange(-freq) %>% 
  select(word, relFreq, D_score) %>% 
  head(25) %>% 
  as.matrix()
