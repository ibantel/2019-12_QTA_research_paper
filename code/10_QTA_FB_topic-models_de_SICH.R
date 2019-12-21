#.################################.#
#. Ivo Bantel (bantel@ipz.uzh.ch) .#
#.################################.#

## 10_QTA_FB_analysis.R ##
#  
#  
#  

# 0-HANDOVER----
pacman::p_load(stm, furrr, purrr, tidytext, ggthemes, scales, quanteda) # imports
library(extrafont)
set.seed(314159)

source("./code/3_QTA_FB_dfm-operations.R")

stopifnot(exists("dfm_fbposts_en") & exists("dfmgrouped_fbposts_en")) # check existence of data

# 1-PREPARATION TOPIC MODELS----
stm_fbposts_de <- dfm_fbposts_de %>% # convert to stm object
  quanteda::convert(to = "stm", docvars = docvars(dfm_fbposts_de))

# 2-TOPIC MODELS----
# from https://www.r-bloggers.com/training-evaluating-and-interpreting-topic-models

# 2.1 TRAIN MANY MODELS WITH DIFFERENT Ks----
ks <- c(4, 5, 10, 15, 30, 40, 50, 60, 70, 80, 100, 150, 200, 250) ### input needed

#load(paste0("./data/", list.files("./data")[[1]])) # load most current .RData file 
train_stms_timed <- function(ks = ks){
  plan(multiprocess)
  start_time <- Sys.time()
  many_models <- data_frame(K = ks) %>% mutate(topic_model = future_map(K, ~stm(documents = stm_fbposts_de$documents, vocab = stm_fbposts_de$vocab, data = stm_fbposts_de$meta,
                                                                                prevalence = ~ islamist + magnitude + outlet_pol_leaning + int_isl_lean + int_isl_magn + int_isl_lean_magn, K = ., seed = 314159, verbose = FALSE)))
  end_time <- Sys.time()
  duration <- end_time - start_time
  print(duration)
  return(many_models)
}

many_models <- train_stms_timed(ks = c(30,40,50))


# 2.2 EVALUATE TOPIC MODELS: HOW MANY TOPICS ARE GOOD / BEST----
heldout <- make.heldout(stm_fbposts_de$documents, vocab = stm_fbposts_de$vocab) # finds k_result & allows evaluating models trained on a dfm and digging into the model diagnostics

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, stm_fbposts_de$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, stm_fbposts_de$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "Best number of topics is where held-out likelihood is highest and residuals are lowest") + 
  theme_base()

topic_num <- 40 ### input needed

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% ks[(ks > topic_num - 30) & (ks < topic_num + 30)]) %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")

topic_model <- k_result %>% 
  filter(K == topic_num) %>% 
  pull(topic_model) %>% 
  .[[1]]
#topic_model

# 2.3 EXPLORE THE TOPIC MODEL----
td_beta <- tidy(topic_model)

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(stm_fbposts_de$documents))

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols = c(terms))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.09),
                     labels = percent_format()) +
  theme_tufte(ticks = FALSE) +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the corpus",
       subtitle = "With the top words that contribute to each topic")

topics <- c(18, 36, # terror & islam*
            9, 7, # terror
            39, 2, 31, 15, 33, 27, # anschlag
            25, # islamist
            23, 11, 17, # terroranschlag
            14, # angriff
            21, # terrorist
            3) # terrorang ### input needed

gamma_terms %>% # topics ordered by prevalence
  select(topic, gamma, terms) %>%
  knitr::kable(digits = 3, 
               col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

rm(cols, gamma_terms, td_beta, td_gamma, top_terms, heldout, ks)

# 2.4 ESTIMATE EFFECTS OF COVARIATES----

effects_topic_model <- estimateEffect(topics ~ islamist + magnitude + outlet_pol_leaning + int_isl_lean + int_isl_magn + int_isl_lean_magn,
                                      stmobj = topic_model, meta = stm_fbposts_de$meta, uncertainty = "Global")

# plot effects of covariates
pdf(paste0("./output/plot_", topic_num, "_topics_coeffs.pdf"))
plot(effects_topic_model, # plot impact of dummy categorical variables
     #covariate = "islamist", # islamist
     #cov.value1 = 0, # islamist
     #cov.value2 = 1, # islamist
     # 15 negatively significant
     #covariate = "magnitude", # magnitude
     #cov.value1 = 0, # magnitude
     #cov.value2 = 2, # magnitude
     # 15 negatively significant
     #covariate = "outlet_pol_leaning",
     #cov.value1 = -3, # leaning
     #cov.value2 = 3, # leaning
     # 39 negatively significant 
     #covariate = "int_isl_lean", # int_isl_lean
     #cov.value1 = -3, # int_isl_lean
     #cov.value2 = 3, # int_isl_lean
     # none significant
     #covariate = "int_isl_magn", # int_isl_magn
     #cov.value1 = 0, # int_isl_magn
     #cov.value2 = 2, # int_isl_magn
     # 15 positively significant
     covariate = "int_isl_lean_magn", # int_isl_lean_magn
     cov.value1 = -6, # int_isl_lean_magn
     cov.value2 = 6, # int_isl_lean_magn
     # none significant
     topics = topics,
     model = topic_model,
     method = "difference",
     #xlim = c(-.2, .2),
     labeltype = "custom", custom.labels = topics, #seq(1:topic_num), #labels, # pay attention to labelling!
     main = "Effects",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()

# Z-CLEANUP----
save(stm_fbposts_de, many_models, k_result, topic_model, effects_topic_model, # save as Rdata since it takes long to calculate this
     file = paste0("./data/many_topic_models_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".Rdata"))

rm(k_result, topic_num, effects_topic_model)

stopifnot(exists("topic_model"))
