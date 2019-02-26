################## Library Imports #########
library(tidyverse)
library(readr)
library(stringr)
library(wordVectors)
library(magrittr)
library(rebus)
library(rlist)
library(marray)
############################################


############### word2vec function ###################
word2vec <- function(fileName) {
  if (grepl('.txt', fileName, fixed=T)) {
    # Convert test.txt to test.bin.
    binaryFileName <- gsub('.txt', '.bin', fileName, fixed=T)
  }
  else {
    binaryFileName <- paste0(fileName, '.bin')
  }
  
  # Train word2vec model.
  if (!file.exists(binaryFileName)) {
    # Lowercase and setup ngrams.
    prepFileName <- 'temp.prep'
    prep_word2vec(origin=fileName, destination=prepFileName, lowercase=T, bundle_ngrams=2)
    
    # Train word2vec model.
    # vectors = 200 in the ORIGINAL paper. However, it can vary.
    model <- train_word2vec(prepFileName, binaryFileName, vectors=150, threads=4, window=12, iter=5, negative_samples=0)
    
    # Cleanup.
    unlink(prepFileName)
  }
  model}
##################### Don't change this unless you want to increase the vector size, say, from 100 to 200 ###############

#####READ IN CORPUS HERE###########
corpus  <- readChar('data_sample.txt', file.info('data_sample.txt')$size)
model <- word2vec('data_sample.txt')
saveRDS(model, 'Trained_model')

# create a look-up table for each word's embeddings in the training corpus
# look_ups is an R object that works similar to matrices
look_ups =  read.vectors("data_sample.bin")

# Save the embeddings here
saveRDS(look_ups, 'LOOKUP_TABLE')

# Load the embeddings if you have trained your corpus already
# Load the trained model object
embed_table = readRDS('LOOKUP_TABLE')
model = readRDS('Trained_model')
vocabs = rownames(embed_table)


###########INPUT: a list of neutral word, e.g., "Powerful" list##############
###########OUTPUT: return a cleaned list of words without *'s -- for example, ambitio* is changed into ambitio, etc. ###############
remove_star <- function(neutral_word_list) {
  # clean up a neutral list with stars
  clean_list = vector('list', length(neutral_word_list))
  i = 1 
  for (word in neutral_word_list ){
    if (grepl('[^[:alnum:]]', word)){
      clean_list[i] <- substr(word, 1, nchar(word)-1)
    } else {clean_list[i] <- word}
    i = i + 1
  }
  CLEANED = unlist(list.clean(clean_list, function(x) x == "", TRUE))
  return(CLEANED)
}
###############remove_star()#################################################

###########INPUT: a list of neutral word, e.g., "Powerful" list##############
###########OUTPUT: return the average embedding of this list of words########
return_ave_embed <- function(neutral_list){
  #obtain a cleaned list of words
  all_index <- NULL
  for (word in remove_star(neutral_list)){
    new_index = which(str_detect(vocabs, word))
    all_index = append(all_index, new_index)
  
  }
  all_index = unique(all_index)
  related_vocabs = vocabs[all_index]
  ave_embed = embed_table[[related_vocabs]]
  return(ave_embed)
}
###############return_ave_embed()#############################################


#####################INPUT: a word from each list - string type#############################################
#####################e.g., return_bias_score('ambition', 'female_words'(default), 'male_words'(defualt))#########
########################OUTPUT: Bias score between female-ambition and male-ambition#############################
########################if bias_female_male > 0, then female is more closely related to the neutral words than male############
return_bias_score <-function(word, female_header="female_words", male_header="male_words"){
  female_embed = return_ave_embed(word_lists[[female_header]])
  male_embed = return_ave_embed(word_lists[[male_header]])
  word_embed = embed_table[[word]]
  bias_female_male = cosineSimilarity(female_embed, word_embed) - cosineSimilarity(male_embed, word_embed)
  return(bias_female_male)
}

#####################return_bias_score(arg1, arg2, arg3)############################################################

####INPUT: a column of cleaned neutral words#####
list_to_scores <- function(a_word_list){
    a_word_list = unlist(a_word_list, use.names=FALSE)
    a_word_list = remove_star(a_word_list)
    bias = sapply(a_word_list, return_bias_score)
    return(bias)
}

#############INPUT: Neutral word list#################
#############OUTPUT: Bias scores for each neutral word lists
create_bias_scores <- function(neutral_words){

  neutral_words = lapply(neutral_words, remove_star)
  bias_scores = lapply(neutral_words, list_to_scores)
  # bias_scores = rbindlist(bias_scores, idcol=TRUE)
  return(bias_scores)
}
###################create_bias_scores()##############

#Outut a csv file of bias scores. 
#Read in NEUTRAL word list
word_lists = read.csv("word_lists.csv", header = TRUE)
word_lists <- word_lists[,apply(word_lists, 2, function(x) { sum(!is.na(x)) > 0 })]


##############USAGE#################

# USAGE 1: if you input a column of words, it gives you each bias score of the corresponding word
# Sample Test on Bias scores for 'Powerful' column

list_to_scores(word_lists[['Powerful']])
list_to_scores(word_lists[["competence"]])


# USAGE 2: if you input a word, it gives you each bias score of this word
# Sample Test on Bias scores for 'Power' words 
return_bias_score('ambition')

# USAGE 3: If you input an excel file of all the neutral words, with each header clearly specified
# You can directly use the readRDS file next time without rerunning the code.
bias_all = create_bias_scores(word_lists)
saveRDS(bias_all, file="bias_scores.RData")
readRDS("bias_scores.RData")




