library(extrafont)
library(ggthemes)
library(tidyverse)
loadfonts()

# Different directories for different experiments
# NB: only the last directory which is not commented out will be used when the script runs
RDS_DIRECTORY <- "ordinal-power-analysis/variable-thresholds/"
RDS_DIRECTORY <- "ordinal-power-analysis/fixed-thresholds/"
RDS_DIRECTORY <- "ordinal-power-analysis/fixed-thresholds-more-effect-sizes/"
RDS_DIRECTORY <- "ordinal-power-analysis/fixed-thresholds-more-effect-sizes-smaller-random-effects/"
RDS_DIRECTORY <- "ordinal-power-analysis/debugged/"
RDS_DIRECTORY <- "ordinal-power-analysis/debugged-actual-variance/"
RDS_DIRECTORY <- "ordinal-power-analysis/debugged-actual-variance-varied-thresholds/"
RDS_DIRECTORY <- "ordinal-power-analysis/better-design-matrix/"
RDS_DIRECTORY <- "ordinal-power-analysis/better-design-matrix-actual-variance/"
RDS_DIRECTORY <- "ordinal-power-analysis/full-experiment-low-variance/"
RDS_DIRECTORY <- "ordinal-power-analysis/more-variance-settings/"

get_data_from_files <- function(rds_directory) {
files <- dir(rds_directory)

results <- NULL
for (filename in files) {
  if (endsWith(filename, ".Rds")) {
    
  p_values <- readRDS(file.path(rds_directory, filename))
  p_values <- p_values %>%
    replace(is.na(.), 10)
  row <- cbind(filename,
               lme_fe=sum(p_values$lme_fe < 0.05, na.rm=TRUE)/length(p_values$lme_fe),
               clm_fe=sum(p_values$clm_fe < 0.05, na.rm=TRUE)/length(p_values$clm_fe),
               lme_re=sum(p_values$lme_re < 0.05, na.rm=TRUE)/length(p_values$lme_re),
               clm_re=sum(p_values$clm_re < 0.05, na.rm=TRUE)/length(p_values$clm_re))
  if (is.null(results)) {
    results <- row
  } else {
    results <- rbind(results, row)
  }
  }
}
return(as.data.frame(results))
}

results <- get_data_from_files(RDS_DIRECTORY)

# results <- rbind(get_data_from_files("ordinal-power-analysis/full-experiment-low-variance/"),
#                  get_data_from_files("ordinal-power-analysis/full-experiment-high-variance/"),
#                  get_data_from_files("ordinal-power-analysis/full-experiment-general-variance/"))

results <- results %>%
  separate(filename, 
           c("prefix", "condition", "filler", 
             "num_simulations", "num_participants", "num_items", "effect_size"), 
           sep="_") %>%
  select(-prefix, -filler) %>%
  mutate(num_simulations=as.factor(as.numeric(str_extract(num_simulations, "[0-9]+"))),
         num_participants=as.factor(as.numeric(str_extract(num_participants, "[0-9]+"))),
         num_items=as.factor(as.numeric(str_extract(num_items, "[0-9]+"))),
         effect_size=as.numeric(str_extract(effect_size, "[0-9]+.[0-9]+"))) %>%
  gather(model, power, lme_re:clm_re) %>%
  mutate(power=as.numeric(power),
         condition=factor(condition, 
                          levels=c("extra-low-variance", 
                                               "low-variance",
                                               "representative-variance",
                                               "high-variance",
                                               "extra-high-variance"),
                          ordered=TRUE)
         )


write.csv(results, file="ordinal-power-simulations_results_nsims100.csv")
ggplot(filter(results, num_participants %in% c(3, 10)) %>%
         mutate(num_participants = as.factor(paste("Num. Workers:", num_participants)),
                num_participants = fct_relevel(num_participants, "Num. Workers: 3", "Num. Workers: 10")),
       aes(x=effect_size, y=power, group=num_items, colour=num_items)) + geom_line() +
  facet_grid(model ~ num_participants) +
  xlab("mean difference") + ylab("power") + theme_bw(10) +
  xlim(0, .8) + ylim(0,1) + geom_point(alpha=.2) +
  theme(legend.position = "bottom")


ggplot(filter(results, num_participants %in% c(3, 10)) %>%
         mutate(num_participants = as.factor(paste("# Ratings / Text:", num_participants)),
                num_participants = fct_relevel(num_participants, "# Ratings / Text: 3", "# Ratings / Text: 10"),
                facet_grouping = as.factor(paste(num_participants, num_items, model))),
       aes(x=effect_size, y=power, group=facet_grouping, colour=num_items, linetype=model)) + geom_line() +
  facet_grid(condition ~ num_participants, 
             labeller = labeller(condition = c("representative-variance" = "General",
                                               "high-variance" = "High Variance",
                                               "low-variance" = "Low Variance",
                                               "extra-high-variance" = "Extreme High Variance",
                                               "extra-low-variance" = "Extreme Low Variance"))) +
  scale_color_manual(values=c("#000000", "#E69F00", "#0072B2", "#56B4E9", "#009E73",
                              "#F0E442", "#D55E00", "#CC79A7")) +
  xlab("Difference in Means (in latent variable space)") + 
  ylab("Power") + 
  theme_bw(10) +
  xlim(0, .65) + ylim(0,1) + geom_point(alpha=.2) +
  theme(legend.position = "bottom", 
        legend.box = "vertical",
        # legend.box.margin = margin(),
        # legend.box.spacing = unit(0, "cm"),
        legend.spacing = unit(0, "cm"),
        legend.margin = unit(0, 'cm'),
        text=element_text(size=10)) + 
  guides(colour = guide_legend("Number of Items"),
         linetype = guide_legend("Model"))

ggsave(filename=paste0("figures/ordinal-power-simulations_",
                       "more-variance-settings",
                       "_results_nsims100.pdf"), 
       width=5, height=8)
embed_fonts(paste0("figures/ordinal-power-simulations_",
                   "more-variance-settings",
                   "_results_nsims100.pdf"))


ggplot(filter(results, num_participants %in% c(3, 10)) %>%
         filter(condition %in% c("low-variance", "representative-variance", "high-variance")) %>%
         mutate(num_participants = as.factor(paste("# Ratings / Text:", num_participants)),
                num_participants = fct_relevel(num_participants, "# Ratings / Text: 3", "# Ratings / Text: 10"),
                facet_grouping = as.factor(paste(num_participants, num_items, model))),
       aes(x=effect_size, y=power, 
           group=facet_grouping, 
           colour=num_items, 
           linetype=model)) + 
  geom_line() +
  facet_grid(condition ~ num_participants, 
             labeller = labeller(condition = c("representative-variance" = "General",
                                               "high-variance" = "High Variance",
                                               "low-variance" = "Low Variance"))) +
  scale_color_manual(values=c("#000000", "#E69F00", "#0072B2", "#56B4E9", "#009E73",
                              "#F0E442", "#D55E00", "#CC79A7")) +
  xlab("Difference in Means (in latent variable space)") + 
  ylab("Power") + 
  theme_bw(10) +
  xlim(0, .65) + ylim(0,1) + geom_point(alpha=.2) +
  theme(legend.position = "bottom", 
        legend.box = "vertical",
        # legend.box.margin = margin(),
        # legend.box.spacing = unit(0, "cm"),
        legend.spacing = unit(0, "cm"),
        legend.margin = unit(0, 'cm'),
        text=element_text(size=10)) + 
  guides(colour = guide_legend("Number of Items"),
         linetype = guide_legend("Model"))

ggsave(filename=paste0("figures/ordinal-power-simulations_",
                       "more-variance-settings-but-basic",
                       "_results_nsims100_7x10.pdf"), 
       width=3.5, height=5)

embed_fonts(paste0("figures/ordinal-power-simulations_",
                   "more-variance-settings-but-basic",
                   "_results_nsims100_7x10.pdf"))




