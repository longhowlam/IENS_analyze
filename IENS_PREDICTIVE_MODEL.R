library(dplyr)
library(text2vec)
library(stringr)
library(glmnet)
library(xgboost)

#### import scraped iens reviews #######################################################

iens = readRDS("IensReviews.rds")

#### Cleaning up text ####################################################

### remove empty descriptions, short descriptions outlying prices 
iens = iens %>%
  filter(
    taaldet  == "dutch"
  ) %>% 
  mutate(
    Nwoorden = str_count(Review, pattern = '[\\w]+'),
    Review = str_to_lower(Review)
  ) %>%
  filter(
    Nwoorden > 20,
    Nwoorden < 500,
    !is.na(eten)
  )

# create an index column
iens$id = 1:dim(iens)[1]


#### Text mining part ###########################################################

stopwoorden_nl =c(
  stopwords::stopwords( language = "nl"),
  letters,
  c("we", "eten")
  )

prep_fun = function(x) {
  x %>% 
    str_to_lower %>% 
    str_replace_all("[^[:alnum:]]", " ") %>% 
    str_replace_all("\\s+", " ")
}

## clean with prep_fun, tokennize, remove stopw and create pruned vocab
iter = iens$Review %>% 
  word_tokenizer() %>%  
  itoken(
    preprocessor = prep_fun,
    progressbar = TRUE
  )

pruned_vocab = iter %>% 
  create_vocabulary(
    stopwords = stopwoorden_nl,
    ngram = c(ngram_min = 1L, ngram_max = 3L)
  ) %>% 
  prune_vocabulary(
    doc_count_min = 250  
  )

#### create the document term matrix and td-idf
vectorizer = vocab_vectorizer(pruned_vocab)
dtm = create_dtm(iter, vectorizer)

tfidf = TfIdf$new()
# fit model to train data and transform train data with fitted model
dtm_tfidf = fit_transform(dtm, tfidf)

#### Fit predictive model for eten  ############################################################

## split into test and train
Ntrain = floor(0.8*nrow(dtm_tfidf))
tr.idx = sample(1:nrow(dtm_tfidf), size = Ntrain)

dtm_train = dtm[tr.idx,]
dtm_test = dtm[-tr.idx,]
target_train = iens$eten[tr.idx]
target_test = iens$eten[-tr.idx]

#### xgboost ###################################################################

param = list(
  max_depth = 15,
  nthread = 8
)

### use the xgb.train so that we can monitor a 'watchlist'
# but we need then an xgb.dmatrix object
  
dtrain <- xgb.DMatrix(dtm_train, label = target_train)
dtest <- xgb.DMatrix(dtm_test, label = target_test)
watchlist <- list(eval = dtest, train = dtrain)

xgbmodel = xgb.train(
  params = param,
  data = dtrain,
  nrounds = 150,
  watchlist = watchlist,
  print_every_n = 5L
)


## R squared calculation on the hold out test set
test_eten = predict(xgbmodel, newdata =  dtm_test)
R2_xgb = 1 - sum((target_test - test_eten)^2) / sum((target_test - mean(target_test))^2)
R2_xgb

## some model diagnostics 
varimp = xgb.importance(colnames(dtm_train), model = xgbmodel)
head(varimp, 25)


##########  predict complete new texts #########################
mijnreviews = data.frame(
  review = c("wat een klote plaats het eten is koud en slecht, ik kom niet meer terug",
             "ik heb erg lekker gegeten en het eten was smakelijk"),
  id = 1,
  stringsAsFactors = FALSE
)

it_test = mijnreviews$review %>% 
  prep_fun %>% 
  word_tokenizer() %>% 
  itoken(ids = mijnreviews$id)

dtm_mijnreview = create_dtm(it_test, vectorizer)

predict(xgbmodel, newdata =  dtm_mijnreview)

