#.################################.#
#. Ivo Bantel (bantel@ipz.uzh.ch) .#
#.################################.#

## 3_QTA_FB_dfm-operations.R ##
#  tokenization & cleaning
#  
#  

# 0-HANDOVER----
source("./code/2_QTA_FB_tokens-operations.R")

stopifnot(exists("toks_fbposts_de") & exists("toks_fbposts_en")) # check existence of data

# 1-DFM CREATION
dfm_fbposts_de <- toks_fbposts_de %>% dfm(stem = TRUE) %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 2) # keep terms appearing 3 times and more and in 2 documentsand more
dfmgrouped_fbposts_de <- dfm_fbposts_de %>% dfm(groups = "islamist")

dfm_fbposts_en <- toks_fbposts_en %>% dfm(stem = TRUE) %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 2) # keep terms appearing 3 times and more and in 2 documentsand more
dfmgrouped_fbposts_en <- dfm_fbposts_en %>% dfm(groups = "islamist")

# Z-HANDOVER----
rm(toks_fbposts_de, toks_fbposts_en)
stopifnot(exists("dfm_fbposts_de") & exists("dfm_fbposts_en") & 
          exists("dfmgrouped_fbposts_de") & exists("dfmgrouped_fbposts_en")) # check existence of data
