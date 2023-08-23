####################################################
## CONSTRUCT DICTIONARIES                          #
####################################################

# Preamble, setting working directory

rm(list = ls())
setwd()

## packages
require(tibble)
require(dplyr)

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_con.csv") %>% 
  filter(positive-negative >= 0.25) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_positive_con.txt")

## construct and save positive dictionary
read_csv("robustMNIR/ML_score_con.csv") %>% 
  filter(negative-positive >= 0.25) %>% 
  pull(word) %>% 
  write_lines(file = "dictionary/ML_negative_con.txt")

df = read_csv("robustMNIR/ML_score_con.csv") 
