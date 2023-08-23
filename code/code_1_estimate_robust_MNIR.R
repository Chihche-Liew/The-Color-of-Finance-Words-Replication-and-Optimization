###################################################
# ESTIMATE ROBUST MNIR AND STRUCTURE OUTPUT       #
###################################################
rm(list = ls())

## Number of iterations for the robust MNIR algorithm
no.iterations <- 500

## Preamble: set working directory
setwd("D:/ColorFinanceWords")

## Load packages
require(tibble)
require(dplyr)
require(readr)
require(parallel)
require(textir)
require(tm)
require(slam)
require(DescTools)
require(Matrix)
require(RPostgres)
require(bizdays)
require(tm)

##########################################
# Function to estimate MNIR 
##########################################

getMnirLoadings <- function(meta = meta, # Data with Y variable
                            filter = T, # Additional filter
                            dtm =  dtm,  # Document term Matrix
                            n.terms = 2^16, # number of terms used
                            nr.clusters = 20 # number of clusters used for MNIR implementation
){

  ## This function just needs a meta object
  ## with a filing.period.excess.return variable.
  ## The PERMNO+ information provided in the repository 
  ## should make this easily available to many researchers.
  ## But we note this function will not run without an updated
  ## metadata file (relative to what we share in the repository).
    
  ## initiate cluster
  cl <- makeCluster(nr.clusters)
 
  ## only take the top X terms
  if(ncol(dtm) > n.terms){dtm <- dtm[,1:n.terms]}
  
  ## Filter 1: limit with provided filter
  meta <- meta[filter, ]
  dtm <- dtm[filter, ]
  
  ## Filter 2: take out empty documents
  filter2 <- row_sums(dtm) != 0
  meta <- meta[filter2, ]
  dtm <- dtm[filter2, ]
  
  ## Filter 3: take out empty terms
  filter3 <- col_sums(dtm) != 0
  dtm <- dtm[, filter3]
  
  #print(meta)
  ## Winsorize at 1-99%
  meta$filing.period.excess.return <- Winsorize(meta$filing.period.excess.return, probs = c(0.01, 0.99))
  
  ## Fit the MNIR model
  fits <- dmr(cl,
              meta$filing.period.excess.return,
              dtm,
              bins = NULL,
              gamma = 0, 
              nlambda = 10,
              verb = 2)
  
  ## Extract MNIR coefs
  mnir.coef <- sort(coef(fits)[2,])
  
  ## end cluster
  stopCluster(cl)
  
  ## output
  return(mnir.coef)
}


##########################################
## Function to structure robust MNIR ouput
##########################################

structureRobustMnirOutput <- function(MNIRest = MNIRest, # output of function getMnirLoadings
                                      wordCount, # word count 
                                      filePath = NULL # output destination
){
  
  # Adjust colname
  names(MNIRest) <- 1:length(MNIRest)
  
  # make into a tibble
  lapply(names(MNIRest), function(cn){
    temp <- tibble(word = names(MNIRest[[cn]]))
    temp[[cn]] <- 0
    temp[[cn]][0 > as.vector(MNIRest)[[cn]]] <- -1
    temp[[cn]][0 < as.vector(MNIRest)[[cn]]] <- 1
    temp
  }) -> MNIRest
  
  # aggregate
  out <- MNIRest[[1]]
  for(i in 2:length(MNIRest)){out <- full_join(out, MNIRest[[i]])}
  MNIRest <- out
  rm(out) 
  
  # summarise 
  MNIRest[,-1] %>% 
    apply(., 1, function(x){
      c(positive = sum(x == 1, na.rm=T), 
        negative = sum(x == -1, na.rm=T), 
        missing = sum(is.na(x)))
    }) %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(word = MNIRest[,1]) -> MNIRest
  MNIRest$word <- unlist(MNIRest$word)
  names(MNIRest$word) <- NULL
  
  # Add information 
  MNIRest %>%
    left_join(wordCount) %>%
    arrange(-(positive-negative)) %>%
    mutate(positive = positive / no.iterations) %>%
    mutate(negative = negative / no.iterations) %>%
    select(word, positive, negative, freq) -> MNIRest
  
  # save
  write.csv(MNIRest, file = filePath, row.names = F)
  
  # end
  return(NULL)}

##########################################################
## Estimate Robust MNIR: top 16K unigrams, full sample 
##########################################################

## load data
#load("data/meta.RData")
meta <- read_csv('./data/con_arousal_happy_20230803.csv')
#load("data/dtm_unigram.RData")


## calculate the excess return
n_pull <- -1

wrds_user <- "lzy2lzz"
wrds_password <- "playerpassword"

wrds <- dbConnect(Postgres(),
                  host     = "wrds-pgdata.wharton.upenn.edu",
                  port     = 9737,
                  user     = wrds_user,
                  password = wrds_password,
                  dbname   = "wrds",
                  sslmode  = "require")


res <- dbSendQuery(wrds, "select date, permno, prc, ret
                          from crsp.dsf 
                          where date between '01/31/2005' and '12/31/2020'")

price <- dbFetch(res, n = n_pull); dbClearResult(res)

res <- dbSendQuery(wrds, "select date, vwretd
                          from crsp.dsi
                          where date between '01/31/2005' and '12/31/2020'")

index <- dbFetch(res, n = n_pull); dbClearResult(res)

date_sample <- index['date'] %>% distinct()
cal <- create.calendar('cal', weekdays = c("saturday", "sunday"), financial = T)

date_1 <- meta['callDate'] %>% 
  distinct() %>% 
  mutate(date = callDate - 1) %>% 
  mutate(date = preceding(date, cal))

date_2 <- date_1 %>% 
  mutate(date = callDate)

date_3 <- date_1 %>% 
  mutate(date = callDate + 1) %>% 
  mutate(date = following(date, cal))

date_4 <- date_1 %>% 
  mutate(date = callDate + 2) %>% 
  mutate(date = following(date, cal))

tw <- date_1 %>% 
  bind_rows(date_2, date_3, date_4)

meta <- meta %>% 
  left_join(tw, by='callDate') %>% 
  distinct() %>% 
  left_join(price, by=c('permno', 'date')) %>% 
  left_join(index, by=c('date')) %>% 
  mutate(ret = ret + 1,
         vwretd = vwretd + 1) %>% 
  group_by(permno, callDate) %>% 
  summarise(filing.period.excess.return = prod(ret, na.rm = T) - prod(vwretd, na.rm = T))

## Transforming the document-term-matrix
## This step is included to transform the document-term. 
## matrix to speed up the MNIR calculation. The downside
## of this transformation is that more memory space is
## allocated to holding it in memory. Also, this transformation 
## will take time, in particular for bigrams dtms. 
dtm <- Matrix::as.matrix(dtm)

## estimate robust MNIR
MNIRest <- list()
for(i in 1:no.iterations){
    cat("Iteration", i, "\n")

    # randomly select 5000 observations for each iteration
    filter.sample <- (1:nrow(meta)) %in% (sample(1:nrow(meta), 5000))

    # Call getMnirLoadings function
    MNIRest[[i]] <- getMnirLoadings(meta = meta,
                                    filter = filter.sample,
                                    dtm =  dtm,
                                    n.terms = 2^14,
                                    nr.clusters = 4)
}

## Get word counts
load("data/dtm_unigram.RData")
wordCount <- tibble(word = colnames(dtm), freq = col_sums(dtm))

## structure and save
structureRobustMnirOutput(MNIRest = MNIRest,
                          wordCount = wordCount,
                          filePath = "robustMNIR/ML_score_unigram_1.csv")

save(meta, file = './data/meta_1.RData')
##########################################################
## Estimate Robust MNIR: top 16K unigrams, pre 2016
##########################################################

## load data
load("data/meta.RData")
load("data/dtm_unigram.RData")

## Transforming the document-term-matrix
dtm <- Matrix::as.matrix(dtm)

## limit to pre 2016
ix <- meta$callDate <= as.Date("2015-12-31")
meta <- meta[ix, ]
dtm <- dtm[ix, ]

## estimate robust MNIR
for(i in 1:no.iterations){
    cat("Iteration", i, "\n")

    # randomly select 5000 observations for each iteration
    filter.sample <- (1:nrow(meta)) %in% (sample(1:nrow(meta), 5000))

    # Call getMnirLoadings function
    MNIRest[[i]] <- getMnirLoadings(meta = meta,
                                    filter = filter.sample,
                                    dtm =  dtm,
                                    n.terms = 2^14,
                                    nr.clusters = 4)
}

## Get word counts
load("data/dtm_unigram.RData")
wordCount <- tibble(word = colnames(dtm), freq = col_sums(dtm))

## structure and save
structureRobustMnirOutput(MNIRest = MNIRest,
                          wordCount = wordCount,
                          filePath = "robustMNIR/ML_score_unigram_20151231_1.csv")

##########################################################
## Estimate Robust MNIR: top 16K bigrams, full sample
##########################################################

## load data
load("data/meta.RData")
load("data/dtm_bigram.RData")

## Transforming the document-term-matrix
dtm <- Matrix::as.matrix(dtm)

## estimate robust MNIR
for(i in 1:no.iterations){
    cat("Iteration", i, "\n")

    # randomly select 5000 observations for each iteration
    filter.sample <- (1:nrow(meta)) %in% (sample(1:nrow(meta), 5000))

    # Call getMnirLoadings function
    MNIRest[[i]] <- getMnirLoadings(meta = meta,
                                    filter = filter.sample,
                                    dtm =  dtm,
                                    n.terms = 2^16,
                                    nr.clusters = 4)
}

## Get word counts
load("data/dtm_bigram.RData")
wordCount <- tibble(word = colnames(dtm), freq = col_sums(dtm))

## structure and save
structureRobustMnirOutput(MNIRest = MNIRest,
                          wordCount = wordCount,
                          filePath = "robustMNIR/ML_score_bigram.csv")

##########################################################
## Estimate Robust MNIR: top 16K bigrams, pre 2016
##########################################################

## load data
load("data/meta.RData")
load("data/dtm_bigram.RData")

## Transforming the document-term-matrix
dtm <- Matrix::as.matrix(dtm)

## limit to pre 2016
ix <- meta$callDate <= as.Date("2015-12-31")
meta <- meta[ix, ]
dtm <- dtm[ix, ]

## estimate robust MNIR
for(i in 1:no.iterations){
    cat("Iteration", i, "\n")

    # randomly select 5000 observations for each iteration
    filter.sample <- (1:nrow(meta)) %in% (sample(1:nrow(meta), 5000))

    # Call getMnirLoadings function
    MNIRest[[i]] <- getMnirLoadings(meta = meta,
                                    filter = filter.sample,
                                    dtm =  dtm,
                                    n.terms = 2^16,
                                    nr.clusters = 4)
}

## Get word counts
load("data/dtm_bigram.RData")
wordCount <- tibble(word = colnames(dtm), freq = col_sums(dtm))

## structure and save
structureRobustMnirOutput(MNIRest = MNIRest,
                          wordCount = wordCount,
                          filePath = "robustMNIR/ML_score_bigram_20151231.csv")

