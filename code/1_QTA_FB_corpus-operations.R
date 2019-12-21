#.################################.#
#. Ivo Bantel (bantel@ipz.uzh.ch) .#
#.################################.#

## 1_QTA_FB_corpus-operations.R ##
#  creating corpus
#  docvar operations
#  split corpus by language
#  filter on keywords

# 0-HANDOVER----
source("./code/0_QTA_FB_posts-cleaning.R")
stopifnot(exists("df_fbposts")) # check existence of data

# 1-CORPUS OPERATIONS----
corp_fbposts <- df_fbposts %>% # construct corpus
  corpus(docid_field = "doc_id", text_field = "status_message")
rm(df_fbposts)

# 2-DOCVAR OPERATIONS----
docvars(corp_fbposts, field = "islamist") <- # set docvars perpetrator category
  ifelse(docvars(corp_fbposts, field = "attacker_categorized") == "ISLAMIST", 1, 0)

docvars(corp_fbposts, field = "magnitude") <- # small attacks: 0-9 fatalities; medium: 10-19; large: 20+
  ifelse(docvars(corp_fbposts, field = "fatalities") > 19, 2, # large = 2
         ifelse(docvars(corp_fbposts, field = "fatalities") <= 9, 1, 0)) # medium = 1, small = 0

source("./data_raw/mapping.R")
docvars(corp_fbposts, field = "lang") <- # map language by newspaper language
  ifelse(docvars(corp_fbposts, field = "page_name") %in% de, "de",
         ifelse(docvars(corp_fbposts, field = "page_name") %in% en, "en", NA))

docvars(corp_fbposts, field = "outlet_pol_leaning") <- # map newspaper leaning (left = -3, right = 3)
  newspapers_mapping[docvars(corp_fbposts, field = "page_name")]
rm(de, en, newspapers_mapping)

docvars(corp_fbposts, field = "int_isl_lean") <- # interaction newspaper leaning, islamist attacks
  docvars(corp_fbposts, field = "islamist") * docvars(corp_fbposts, field = "outlet_pol_leaning")

docvars(corp_fbposts, field = "int_isl_magn") <- # interaction islamist attacks, magnitude
  docvars(corp_fbposts, field = "islamist") * docvars(corp_fbposts, field = "magnitude")

docvars(corp_fbposts, field = "int_isl_lean_magn") <- # interaction newspaper leaning, islamist attacks, magnitude
  docvars(corp_fbposts, field = "islamist") * docvars(corp_fbposts, field = "outlet_pol_leaning") * docvars(corp_fbposts, field = "magnitude")

docvars(corp_fbposts, field = "attack_date") <- as.Date(docvars(corp_fbposts, field = "attack_date"))

docvars(corp_fbposts, c("X", "post_publication_datetime", "page_id", "status_id", "status_type_ordinal__decreasing_complexity", "YYYYMM_post",
                        "num_likes", "num_love", "num_wow", "num_haha", "num_sad", "num_angry", "num_comments", "num_shares", "reactions_comb",
                        "attacker_categorized", "fatalities", "injured",
                        "page_name")) <- NULL # drop docvars

#names(docvars(corp_fbposts))  # check remaining docvars

# 3-SPLIT CORPUS BY LANGUAGE----
corp_de <- corp_fbposts %>%
  corpus_subset(lang == "de")
docvars(corp_de, "id_numeric_de") <- 1:ndoc(corp_de) # for later sampling/ train test-split

corp_en <- corp_fbposts %>%
  corpus_subset(lang == "en")
docvars(corp_en, "id_numeric_en") <- 1:ndoc(corp_en) # for later sampling/ train test-split

rm(corp_fbposts)


# 4-FILTER----
# filter on keywords
dict_de <- dictionary(list(terror = "terror*",
                           attack = c("*attacke", "*angriff", "*anschlag", "anschläg*"),
                           islamism = c("islamis*", "Jihad", "Dschihad"),
                           neutral_wording = c("*explosion*", "schiess*", "schieß*", "messer*"),
                           mental = c("mental", "famil*")))

tmp_df_toks_fbposts_de <- corp_de %>% tokens() %>% tokens_lookup(dictionary = dict_de) %>% # look up terms
  dfm() %>% quanteda::convert(to = "data.frame") # convert to dfm() and to data.frame

dict_en <- dictionary(list(terror = "terror*",
                           attack = "*attack",
                           islamism = c("islamis*", "jihad*"),
                           neutral_wording = c("*explosion*", "shooting", "stabb*", "knife"),
                           mental = c("mental", "famil*")))

tmp_df_toks_fbposts_en <- corp_en %>% tokens() %>% tokens_lookup(dictionary = dict_en) %>% # look up terms
  dfm() %>% quanteda::convert(to = "data.frame") # convert to dfm() and to data.frame

# assign counts as docvars
docvars(corp_de, "terror") <- tmp_df_toks_fbposts_de$terror # count of mentionings of "terror*"
docvars(corp_en, "terror") <- tmp_df_toks_fbposts_en$terror

docvars(corp_de, "attack") <- tmp_df_toks_fbposts_de$attack # count of mentionings of category "attack"
docvars(corp_en, "attack") <- tmp_df_toks_fbposts_en$attack

docvars(corp_de, "islamism") <- tmp_df_toks_fbposts_de$islamism # count of mentionings of category "islamism"
docvars(corp_en, "islamism") <- tmp_df_toks_fbposts_en$islamism

docvars(corp_de, "neutral_wording") <- tmp_df_toks_fbposts_de$neutral_wording # count of mentionings of category "neutral_wording"
docvars(corp_en, "neutral_wording") <- tmp_df_toks_fbposts_en$neutral_wording

docvars(corp_de, "mental") <- tmp_df_toks_fbposts_de$mental # count of mentionings of category "mental"
docvars(corp_en, "mental") <- tmp_df_toks_fbposts_en$mental

docvars(corp_de, "sum_wording") <- tmp_df_toks_fbposts_de$terror + tmp_df_toks_fbposts_de$attack + tmp_df_toks_fbposts_de$islamism +
                                   tmp_df_toks_fbposts_de$neutral_wording + tmp_df_toks_fbposts_de$mental # combined count

docvars(corp_en, "sum_wording") <- tmp_df_toks_fbposts_en$terror + tmp_df_toks_fbposts_en$attack + tmp_df_toks_fbposts_en$islamism +
                                   tmp_df_toks_fbposts_en$neutral_wording + tmp_df_toks_fbposts_en$mental # combined count

corp_de <- corp_de %>% corpus_subset(sum_wording > 0)
corp_en <- corp_en %>% corpus_subset(sum_wording > 0)

rm(dict_de, dict_en, tmp_df_toks_fbposts_de, tmp_df_toks_fbposts_en)

# Z-HANDOVER----
stopifnot(exists("corp_de") & exists("corp_en")) # check existence of data
