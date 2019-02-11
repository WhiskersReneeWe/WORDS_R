################## Library Imports #########
library(tidyverse)
library(readr)
library(stringr)
library(wordVectors)
library(magrittr)
library(rebus)
library(rlist)
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
    model <- train_word2vec(prepFileName, binaryFileName, vectors=100, threads=4, window=12, iter=5, negative_samples=0)
    
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
###########OUTPUT: return a cleaned list of words without *'s ###############
remove_star <- function(neutral_word_list) {
  # clean up a neutral list with stars
  clean_list = vector('list', length(neutral_word_list))
  i = 1 
  for (word in neutral_word_list ){
    if (grepl('*', word)){
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


#####################INPUT: ALL headers have to be passed in strings#############################################
#####################e.g., return_bias_score('Powerful', 'female_words'(default), 'male_words'(defualt))#########
########################OUTPUT: Bias score between female-Powerful and male-Powerful#############################
########################if bias_female_male > 0, then female is more closely related to the neutral words than male############
return_bias_score <-function(words_header, female_header="female_words", male_header="male_words"){
  female_embed = return_ave_embed(word_lists[[female_header]])
  male_embed = return_ave_embed(word_lists[[male_header]])
  ave_embed_words= return_ave_embed(word_lists[[words_header]])
  bias_female_male = cosineSimilarity(female_embed, ave_embed_words) - cosineSimilarity(male_embed, ave_embed_words)
  return(bias_female_male)
}
#####################return_bias_score(arg1, arg2, arg3)############################################################

#############INPUT: Neutral word lis#################
#############OUTPUT: Bias scores for each neutral word lists
create_bias_scores <- function(neutral_word_file){
bias_scores <- NULL
for(neu_words in colnames(neutral_word_lists)){
  bias_scores = append(bias_scores, return_bias_score(neu_words))
}
bias_f_m = as.data.frame(bias_scores)
row.names(bias_f_m) = colnames(neutral_word_lists)
return(bias_f_m)
}
###################create_bias_scores()##############

#Outut a csv file of bias scores. 
#Read in NEUTRAL word list
word_lists = read.csv("word_lists.csv", header = TRUE)
#remove empty columns from the CSV file
word_lists <- word_lists[,apply(word_lists, 2, function(x) { sum(!is.na(x)) > 0 })]
write.csv(create_bias_scores(word_lists),'bias_scores.csv')
