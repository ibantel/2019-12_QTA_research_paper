#.################################.#
#. Ivo Bantel (bantel@ipz.uzh.ch) .#
#.################################.#

## 0_QTA_FB-posts-cleaning.R ##
#  import of data as data.frame
#  string cleaning

# 0-HANDOVER----
pacman::p_load(quanteda, tidyverse, dplyr, readtext, tidyr, readr, caret, stargazer) # imports

# 1-IMPORTS & SETUP----
pacman::p_load(quanteda, tidyverse, dplyr, readtext, tidyr, readr, caret, stargazer)

data_folder <- "C:/Users/bantel/switchdrive/WORK/3_Side-projects/3_2019-08_facebook posts/data_raw"
filename <- "posts_analysis_export.csv"
encoding <- c("UTF-8-BOM", "UTF-8", "ISO-8859-1", "ASCII")

# 2-READ DATA----
df_fbposts <- read.csv(paste0(data_folder, "/", filename), stringsAsFactors = F, encoding = encoding[1]) %>%
  mutate(doc_id = paste(as.character(attack_date),  substr(as.character(attacker_categorized), 1, 2), X, sep = "_")) # write new column doc_id
rm(filename, encoding, data_folder)

# 3-CLEAN DATA----
# https://www.utf8-chartable.de/unicode-utf8-table.pl?start=8192&number=128&utf8=string-literal

df_fbposts <- df_fbposts %>%
  mutate(status_message = str_replace_all(status_message, "\\\\xc3\\\\xa4", "ä")) %>%  # ä
  mutate(status_message = str_replace_all(status_message, "\\\\xc3\\\\x84", "Ä")) %>%  # Ä
  mutate(status_message = str_replace_all(status_message, "\\\\xc3\\\\xb6", "ö")) %>%  # ö
  mutate(status_message = str_replace_all(status_message, "\\\\xc3\\\\x96", "Ö")) %>%  # Ö
  mutate(status_message = str_replace_all(status_message, "\\\\xc3\\\\xbc", "ü")) %>%  # ü
  mutate(status_message = str_replace_all(status_message, "\\\\xc3\\\\x9c", "Ü")) %>%  # Ü
  mutate(status_message = str_replace_all(status_message, "\\\\xc3\\\\x9f", "ß")) %>%  # ß
  
  mutate(status_message = str_replace_all(status_message, "\\\\xe2\\\\x80\\\\x93", "–")) %>% # – (en-dash)
  
  mutate(status_message = str_replace_all(status_message, "\\\\'", "'")) %>% # ' (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\"', "'")) %>% # ' (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x80\\\\x9c', "'")) %>% # ' (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x80\\\\x9d', "'")) %>% # ' (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x80\\\\x9e', "'")) %>% # ' (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x80\\\\x9f', "'")) %>% # ' (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xc2\\\\xab', "'")) %>% # « (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xc2\\\\xbb', "'")) %>% # » (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x80\\\\x98', "'")) %>% # ' (quotation marks)
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x80\\\\x99', "'")) %>% # ' (quotation marks)
  
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x80\\\\x99', "'")) %>% # ' (apostrophe)
  
  mutate(status_message = str_replace_all(status_message, '\\\\xc2\\\\xa3', "£")) %>% # £ (Pound sign)
  mutate(status_message = str_replace_all(status_message, '\\\\xe2\\\\x82\\\\xac', "€")) %>% # € (Euro sign)
  mutate(status_message = str_replace_all(status_message, '\\\\xc2\\\\xa7', "§")) %>% # § (paragraph sign)
  
  mutate(status_message = str_replace_all(status_message, "b\'", "'")) %>% # b'
  
  mutate(status_message = str_replace_all(status_message, "\\\\n", " ")) %>% # \\n
  
  mutate(status_message = str_replace_all(status_message, "^(\\')|(\\')$", "")) %>% # remove leading and trailing ' signs
  mutate(status_message = str_replace_all(status_message, "  ", ""))

# df_fbposts$status_message[grepl(".net/-g", df_fbposts[["status_message"]])] # "GEPC"

df_fbposts <- df_fbposts %>%
  mutate(status_message = str_replace_all(status_message, "faz\\.net\\/-g\\w{2}\\-[0-9a-z]{4,5}#GEPC;s6", "")) %>% # URLs 1
  mutate(status_message = str_replace_all(status_message, "faz\\.net\\/-g\\w{2}\\-[0-9a-z]{4,5}", "")) %>% # URLs 2
  mutate(status_message = str_replace_all(status_message, "faz\\.net\\/-g\\w{2}\\-[0-9a-z]{4,5}\\?GEPC=s6", "")) # URLs 3


# 4-EXPLORE DATA----
# plot reaction numbers (to posts) and casualties (of attacks)

#ggplot(data = df_fbposts[df_fbposts$reactions_comb < 30000, ], aes(x = as.Date(attack_date), y = reactions_comb, color = attacker_categorized)) + geom_point() + theme(axis.text.x = element_text(angle = 10, hjust = 1))
#ggplot(data = df_fbposts[df_fbposts$reactions_comb < 30000, ], aes(x = as.Date(attack_date), y = fatalities,  color = attacker_categorized)) + geom_point() + theme(axis.text.x = element_text(angle = 10, hjust = 1))

#df_fbposts[100:200,13]
#df_fbposts$status_message[1662]



# Z-HANDOVER----
stopifnot(exists("df_fbposts")) # check existence of data
