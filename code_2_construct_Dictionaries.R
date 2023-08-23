####################################################
## CONSTRUCT DICTIONARIES                          #
####################################################

# Preamble, setting working directory

rm(list = ls())
setwd()

## packages
require(tibble)
require(dplyr)

##############################################
## ML unigram, full sample 
##############################################

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_unigram_1.csv") %>% 
  filter(positive-negative >= 0.8) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_unigram_1.txt")

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_unigram_1.csv") %>% 
  filter(negative-positive >= 0.8) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_negative_unigram_1.txt")

##############################################
## ML unigram, pre 2016 
##############################################

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_unigram_20151231.csv") %>% 
  filter(positive-negative >= 0.8) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_unigram_20151231.txt")

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_unigram_20151231.csv") %>% 
  filter(negative-positive >= 0.8) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_negative_unigram_20151231.txt")

##############################################
## ML bigram, full sample 
##############################################

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_bigram_1.csv") %>% 
  filter(positive-negative > 0.4499999) %>%  
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_bigram_1.txt")

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_bigram_1.csv") %>% 
  filter(negative-positive >= 0.45) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_negative_bigram_1.txt")

##############################################
## ML bigram, pre 2016 
##############################################

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_bigram_20151231.csv") %>% 
  filter(positive-negative >= 0.45) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_bigram_20151231.txt")

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_bigram_20151231.csv") %>% 
  filter(negative-positive >= 0.45) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_negative_bigram_20151231.txt")






