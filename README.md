Project in BC with Miao Liu and Yang Cao, Replication of García et al. (2023, JFE) and changes on conversation data and model.

The contents of the folder encompass a collection of scripts and utilities central to this research endeavor. To provide context, the following delineation elucidates the constituent elements:

# R Code Files from 0 to 3  
`code_0`: Comprises my personal implementation designed for the meticulous construction of variables. This intricate procedure closely adheres to the meticulous data structure outlined by García et al.

`code_1` to `code_3`: These modules are extracted from the comprehensive dataset and code compilation presented by García et al, which is hosted on their dedicated website: https://leeds-faculty.colorado.edu/garcia/data.html.

# R Code Files with '.1' Suffix  
Supplementary to the primary code corpus, the repository also encompasses R code files denoted by a '.1' suffix. These files have been tailored to cater to novel conversation datasets, thereby extending the repository's applicability beyond its original scope.

# Text Preprocessing for Conversations  
The `TextPreprocessingConversation` script occupies a pivotal role in the preprocessing pipeline. It orchestrates the transformation of raw textual data into an organized, structured format, facilitated through the creation of tidy dataframes.

# Fine-Tuning BERT 
The `FineTuneBert` is at the center of our current research push, we are trying to fine tune Google Bert to achieve more accurate text-based real-time earnings predictions.

# Explaining BERT by Shapley Value
The `BertShap` uses shap library to explain the in-sample bert attention/word importance.
