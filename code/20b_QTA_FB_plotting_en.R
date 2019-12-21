#.################################.#
#. Ivo Bantel (bantel@ipz.uzh.ch) .#
#.################################.#

## 20b_QTA_FB_plotting_en.R ##
#  
#  
#  

# 0-HANDOVER----
pacman::p_load(stm, furrr, purrr, tidytext, ggthemes, scales, quanteda) # imports
library(extrafont)

load(file = "./data/all_en_data.Rdata")

# 1 PLOT EFFECTS OF COVARIATES
# 1.a plot effects of islamist--------------5 significant----
pdf(paste0("./output/3a-coeffplot_en_", topic_num_en, "_topics_coeffs_islamist_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_en,
     covariate = "islamist", # islamist
     cov.value1 = 0, # islamist
     cov.value2 = 1, # islamist
     # 5 topics significant
     topics = c(2, 6, 20, 22, 29),
     model = topic_model_en,
     method = "difference",
     xlim = c(-.2, .2),
     labeltype = "custom", custom.labels = paste0("Topic ", c(2, 6, 20, 22, 29)), #labels, # pay attention to labelling!
     main = "Effects (en)\n(non-/ Islamist attacks)",
     xlab = "non-Islamist vs. Islamist attack") # given an Islamist attack, we're more/ less likely to read about the topic X
dev.off()

# 1.b plot effects of magnitude-------------3 significant----
pdf(paste0("./output/3b-coeffplot_en_", topic_num_en, "_topics_coeffs_magnitude_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_en,
     covariate = "magnitude", # magnitude
     cov.value1 = 0, # magnitude
     cov.value2 = 2, # magnitude
     # 3 topics significant
     topics = c(2, 20, 29),
     model = topic_model_en,
     method = "difference",
     xlim = c(-.2, .3),
     labeltype = "custom", custom.labels = paste0("Topic ", c(2, 20, 29)), #labels, # pay attention to labelling!
     main = "Effects (en)\n(attack magnitude)",
     xlab = "smaller vs. larger attacks") # e.g. given a larger attack, we're more likely to read about the topic X
dev.off()

# 1.c plot effects of outlet_pol_leaning----0 significant----
#pdf(paste0("./output/3c-coeffplot_en_", topic_num_en, "_topics_coeffs_outlet_pol_leaning_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_en,
     covariate = "outlet_pol_leaning",
     cov.value1 = -3, # leaning
     cov.value2 = 3, # leaning
     # none significant 
     topics = topics_en,
     model = topic_model_en,
     method = "difference",
     xlim = c(-.4, .25),
     labeltype = "custom", custom.labels = topics_en, #labels, # pay attention to labelling!
     main = "Effects (en)",
     xlab = "more left- vs. more right-wing outlet") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()

# 1.d plot effects of int_isl_lean----------3 significant!----
pdf(paste0("./output/3d-coeffplot_en_", topic_num_en, "_topics_coeffs_int_isl_lean", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_en,
     covariate = "int_isl_lean", # int_isl_lean
     cov.value1 = -3, # int_isl_lean
     cov.value2 = 3, # int_isl_lean
     # none significant
     topics = c(9, 23, 28),
     model = topic_model_en,
     method = "difference",
     xlim = c(-.1, .55),
     labeltype = "custom", custom.labels = paste0("Topic ", c(9, 23, 28)), #labels, # pay attention to labelling!
     main = "Effects (en)\n(Islamist attack & media outlet leaning)",
     xlab = "non-/Islamist attack, left-leaning media vs.\n Islamist attack, right-leaning media") # an Islamist attack & right-wing media, newspaper, we're more likely to read about the topic X
dev.off()

# 1.e plot effects of int_isl_magn----------8 significant!----
pdf(paste0("./output/3e-coeffplot_en_", topic_num_en, "_topics_coeffs_int_isl_magn_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_en,
     covariate = "int_isl_magn", # int_isl_magn
     cov.value1 = 0, # int_isl_magn
     cov.value2 = 2, # int_isl_magn
     # 15 positively significant
     topics = c(2, 6, 13, 20, 22, 29, 30, 35),
     model = topic_model_en,
     method = "difference",
     xlim = c(-.45, .35),
     labeltype = "custom", custom.labels = paste0("Topic ", c(2, 6, 13, 20, 22, 29, 30, 35)), #labels, # pay attention to labelling!
     main = "Effects (en)\n(interaction Islamist, magnitude)",
     xlab = "small non-Islamist vs. large Islamist attacks") # e.g. given a large Islamist attack, we're more/ less likely to read about the topic X
dev.off()
# 1.f plot effects of int_isl_lean_magn-----5 significant!----
pdf(paste0("./output/3f-coeffplot_en_", topic_num_en, "_topics_coeffs_int_isl_lean_magn_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_en,
     covariate = "int_isl_lean_magn", # int_isl_lean_magn
     cov.value1 = -6, # int_isl_lean_magn
     cov.value2 = 6, # int_isl_lean_magn
     # none significant
     topics = c(9, 14, 19, 23, 28),
     model = topic_model_en,
     method = "difference",
     xlim = c(-.75, .52),
     labeltype = "custom", custom.labels = paste0("Topic ", c(9, 14, 19, 23, 28)), #labels, # pay attention to labelling!
     main = "Effects (en)\n(interaction Islamist, leaning, magnitude)",
     xlab = "non-Islamist, small attacks in left-leaning outlets vs.\n Islamist, large attacks in right-leaning outlets")
     # given a large Islamist attack and a more right-leaning outlet, we're more likely to read about the topic X
dev.off()
# 1.x plot effects of ALL [not plotable]----
pdf(paste0("./output/3x-coeffplot_en_", topic_num_en, "_topics_coeffs__", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".pdf"))
plot(effects_topic_model_en,
     #covariate = "int_isl_magn", # int_isl_magn
     #cov.value1 = 0, # int_isl_magn
     #cov.value2 = 2, # int_isl_magn
     # 15 positively significant
     #covariate = "int_isl_lean_magn", # int_isl_lean_magn
     #cov.value1 = -6, # int_isl_lean_magn
     #cov.value2 = 6, # int_isl_lean_magn
     # none significant
     topics = topics_en,
     model = topic_model_en,
     method = "difference",
     #xlim = c(-.2, .2),
     labeltype = "custom", custom.labels = topics_en, #labels, # pay attention to labelling!
     main = "Effects (en)",
     xlab = "more ... vs. less ...") # e.g. given an Islamist attack, we're more likely to read about the topic "terror, angriff"
dev.off()
