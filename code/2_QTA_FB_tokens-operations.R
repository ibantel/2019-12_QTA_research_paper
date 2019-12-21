#.################################.#
#. Ivo Bantel (bantel@ipz.uzh.ch) .#
#.################################.#

## 2_QTA_FB_tokens-operations.R ##
#  tokenization & cleaning
#  
#  

# 0-HANDOVER----
source("./code/1_QTA_FB_corpus-operations.R")

stopifnot(exists("corp_de") & exists("corp_en")) # check existence of data

# 1-TOKENS OPERATIONS----
# all performed separately per language corpus
source("./data_raw/words_cleaning.R") # source vectors of additional words to remove

# 1.1--construct tokens objects

toks_fbposts_de <- corp_de %>% tokens(what = "word", remove_punct = T,  # also removes §
                                      remove_numbers = T, remove_symbols = T, # also removes §
                                      remove_separators = T, remove_twitter = T, remove_url = T) %>%
  tokens_tolower() %>% tokens_remove(stopwords("de")) %>%
  tokens_wordstem(language = "german") %>%
  tokens_remove(pattern = c(cities, words, de_outlets, de_words, de_locations))

toks_fbposts_en <- corp_en %>% tokens(what = "word", remove_punct = T,  # also removes §
                                      remove_numbers = T, remove_symbols = T, # also removes §
                                      remove_separators = T, remove_twitter = T, remove_url = T) %>%
  tokens_tolower() %>% tokens_remove(stopwords("en")) %>%
  tokens_wordstem(language = "english") %>%
  tokens_remove(pattern = cities) %>% 
  tokens_remove(pattern = words) %>%
  tokens_remove(pattern = en_outlets) %>%
  tokens_remove(pattern = en_words)

rm(cities, words, de_outlets, de_words, en_outlets, en_words, de_locations, en_locations)
rm(corp_de, corp_en)

# Z-HANDOVER----
stopifnot(exists("toks_fbposts_de") & exists("toks_fbposts_en")) # check existence of data
