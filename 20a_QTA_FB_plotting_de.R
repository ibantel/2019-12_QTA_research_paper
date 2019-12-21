#.################################.#
#. Ivo Bantel (bantel@ipz.uzh.ch) .#
#.################################.#

## 20a_QTA_FB_plotting_de.R ##
#  
#  
#  

# 0-HANDOVER----
pacman::p_load(stm, furrr, purrr, tidytext, ggthemes, scales, quanteda) # imports
library(extrafont)

load(file = "./data/all_de_data.Rdata")

# 1 PLOT EFFECTS OF COVARIATES
# 1.a plot effects of islamist--------------0 significant----
#pdf(paste0("./output/13a-coeffplot_de_", topic_num_de, "_topics_coeffs_islamist_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_de,
     covariate = "islamist", # islamist
     cov.value1 = 0, # islamist
     cov.value2 = 1, # islamist
     # none significant
     topics = topics_de,
     model = topic_model_de,
     method = "difference",
     xlim = c(-.3, .2),
     labeltype = "custom", custom.labels = topics_de, #labels, # pay attention to labelling!
     main = "Effects (de)",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()

# 1.b plot effects of magnitude-------------0 significant----
#pdf(paste0("./output/13b-coeffplot_de_", topic_num_de, "_topics_coeffs_magnitude_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_de,
     covariate = "magnitude", # magnitude
     cov.value1 = 0, # magnitude
     cov.value2 = 2, # magnitude
     # 15 negatively significant
     topics = topics_de,
     model = topic_model_de,
     method = "difference",
     xlim = c(-.6, .3),
     labeltype = "custom", custom.labels = topics_de, #labels, # pay attention to labelling!
     main = "Effects (de)",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()

# 1.c plot effects of outlet_pol_leaning----1 significant----
pdf(paste0("./output/13c-coeffplot_de_", topic_num_de, "_topics_coeffs_outlet_pol_leaning_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_de,
     covariate = "outlet_pol_leaning",
     cov.value1 = -3, # leaning
     cov.value2 = 3, # leaning
     # 39 negatively significant 
     topics = 39,
     model = topic_model_de,
     method = "difference",
     xlim = c(-.45, .025),
     labeltype = "custom", custom.labels = "Topic 39", #labels, # pay attention to labelling!
     main = "Effects (de)\n(outlet political leaning)",
     xlab = "more left-leaning vs. more right-leaning outlet") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()

# 1.d plot effects of int_isl_lean----------0 significant----
#pdf(paste0("./output/13d-coeffplot_de_", topic_num_de, "_topics_coeffs_int_isl_lean", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_de,
     covariate = "int_isl_lean", # int_isl_lean
     cov.value1 = -3, # int_isl_lean
     cov.value2 = 3, # int_isl_lean
     # none significant
     topics = topics_de,
     model = topic_model_de,
     method = "difference",
     xlim = c(-.5, .35),
     labeltype = "custom", custom.labels = topics_de, #labels, # pay attention to labelling!
     main = "Effects (de)",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()

# 1.e plot effects of int_isl_magn----------0 significant----
#pdf(paste0("./output/13e-coeffplot_de_", topic_num_de, "_topics_coeffs_int_isl_magn_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_de,
     covariate = "int_isl_magn", # int_isl_magn
     cov.value1 = 0, # int_isl_magn
     cov.value2 = 2, # int_isl_magn
     # 15 positively significant
     topics = topics_de,
     model = topic_model_de,
     method = "difference",
     xlim = c(-.3, .55),
     labeltype = "custom", custom.labels = topics_de, #labels, # pay attention to labelling!
     main = "Effects (de)",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()
# 1.f plot effects of int_isl_lean_magn-----0 significant----
#pdf(paste0("./output/13f-coeffplot_de_", topic_num_de, "_topics_coeffs_int_isl_lean_magn_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_de,
     covariate = "int_isl_lean_magn", # int_isl_lean_magn
     cov.value1 = -6, # int_isl_lean_magn
     cov.value2 = 6, # int_isl_lean_magn
     # none significant
     topics = topics_de,
     model = topic_model_de,
     method = "difference",
     xlim = c(-.42, .42),
     labeltype = "custom", custom.labels = topics_de, #labels, # pay attention to labelling!
     main = "Effects (de)",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()
# 1.x plot effects of ALL [not plotable]----
pdf(paste0("./output/13x-coeffplot_de_", topic_num_de, "_topics_coeffs__", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_de,
     #covariate = "int_isl_magn", # int_isl_magn
     #cov.value1 = 0, # int_isl_magn
     #cov.value2 = 2, # int_isl_magn
     # 15 positively significant
     #covariate = "int_isl_lean_magn", # int_isl_lean_magn
     #cov.value1 = -6, # int_isl_lean_magn
     #cov.value2 = 6, # int_isl_lean_magn
     # none significant
     topics = topics_de,
     model = topic_model_de,
     method = "difference",
     #xlim = c(-.2, .2),
     labeltype = "custom", custom.labels = topics_de, #labels, # pay attention to labelling!
     main = "Effects (de)",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()