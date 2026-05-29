#### Libraries ####
library(dplyr)
library(lme4)
library(emmeans)
library(ggplot2)
library(ggsignif)

#### Load data ####
all_connections <- read.csv("/Users/hao/Desktop/PhD_1/github/result1/present_structural_connections_scale250.csv")

# Factor and relevel for model (not implanted as intercept)
all_connections$group <- factor(all_connections$group,
                                levels = c("Not Implanted", "Not Involved", "Involved"))


#### Logistic regression model ####
model <- glm(
  gm_or_wm_connected ~ group * one_year_outcome,
  data = all_connections,
  family = binomial
)
summary(model) # model summary

#### Estimated marginal means (predicted probabilities) ####
emm_results_outcome <- emmeans(model, ~ group | one_year_outcome, type = "response")
emm_df <- as.data.frame(emm_results_outcome)

# Convert to factors, relabel & relevel
emm_df$group <- factor(emm_df$group,
                       levels = c("Not Implanted", "Not Involved", "Involved"))

emm_df$one_year_outcome <- factor(emm_df$one_year_outcome, 
                                          levels = c("good","bad"),
                                          labels = c("Good 1-Year Post-Surgical Outcome",
                                                     "Bad 1-Year Post-Surgical Outcome"))


#### Pairwise contrasts within each outcome ####
contrast <- contrast(
  emm_results_outcome,
  method = "pairwise",
  by = "one_year_outcome",
  adjust = "none"
) %>% as.data.frame()

#### Contrast between outcomes ####
emm_outcome <- emmeans(model, ~ one_year_outcome)
contrast(emm_outcome, "pairwise")

#### Annotations for plot ####
contrast <- contrast %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "n.s."
    )
  )


annotations_good <- c(
  contrast$stars[which(contrast$one_year_outcome == "good" &
                                contrast$contrast == "Not Involved / Involved")],
  contrast$stars[which(contrast$one_year_outcome == "good" &
                                contrast$contrast == "Not Implanted / Involved")],
  contrast$stars[which(contrast$one_year_outcome == "good" &
                                contrast$contrast == "Not Implanted / Not Involved")]
)

annotations_bad <- c(
  contrast$stars[which(contrast$one_year_outcome == "bad" &
                                contrast$contrast == "Not Involved / Involved")],
  contrast$stars[which(contrast$one_year_outcome == "bad" &
                                contrast$contrast == "Not Implanted / Involved")],
  contrast$stars[which(contrast$one_year_outcome == "bad" &
                                contrast$contrast == "Not Implanted / Not Involved")]
)

y_good <- c(0.41, 0.38, 0.35)
y_bad  <- y_good

comparisons_list <- list(
  c("Not Implanted", "Involved"),
  c("Not Implanted", "Not Involved"),
  c("Not Involved", "Involved")
)

#### Bar Plot ####

ggplot(emm_df, aes(x = group, y = prob, fill = group)) +
  geom_bar(stat = "identity", color = "black", size = 0.4, width = 0.6) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  facet_wrap(~ one_year_outcome) +
  labs(x = "Connection Between",
       y = "Probability of Structural Connectivity") +
  scale_fill_manual(values = c("#0076c0", "#a30234", "#67771a")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        #panel.grid.minor.y = element_line(color = "grey95"),
        legend.position = "none") +
  
  # bad outcome annotations
  geom_signif(
    data = emm_df %>% filter(one_year_outcome == "Bad 1-Year Post-Surgical Outcome"),
    comparisons = comparisons_list,
    annotations = annotations_bad,
    y_position = y_bad,
    tip_length = 0.02,
    textsize = 5,
    fontface = "bold"
  ) +
  
  # good outcome annotations
  geom_signif(
    data = emm_df %>% filter(one_year_outcome == "Good 1-Year Post-Surgical Outcome"),
    comparisons = comparisons_list,
    annotations = annotations_good,
    y_position = y_good,
    tip_length = 0.02,
    textsize = 5,
    fontface = "bold"
  )
