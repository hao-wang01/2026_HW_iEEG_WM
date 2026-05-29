#### Load libraries ####
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(ggsignif)
library(dplyr)

#### Load data ####
z_scores <- read_csv("result2/z_score_meanMD_subs_scale250.csv")

# Factor and relevel for model (not implanted as intercept)
z_scores$group <- factor(z_scores$group,
                                levels = c("Not Implanted", "Not Involved", "Involved"))

#### LME model ####
model <- lmer(abnormality_zscore_connection ~ group * one_year_outcome + (1 | ID), 
                  data = z_scores)
summary(model) # model summary

# Compute estimated marginal means and contrasts
emm_options(lmer.df = "satterthwaite") 
emm <- emmeans(model, ~ group | one_year_outcome, lmerTest.limit = 443344)
emm_df <- as.data.frame(emm)

# Convert to factors, relabel & relevel
emm_df$group <- factor(emm_df$group,
                              levels = c("Not Implanted", "Not Involved", "Involved"))

emm_df$one_year_outcome <- factor(emm_df$one_year_outcome, 
                                         levels = c("good", "bad"),
                                         labels = c("Good 1-Year Post-Surgical Outcome",
                                                    "Bad 1-Year Post-Surgical Outcome"))

#### Pairwise contrasts within each outcome ####
contrast <- contrast(emm, 
                         method = "pairwise", 
                         by = "one_year_outcome", 
                         adjust = "none") %>% as.data.frame()

#### Contrast between outcomes ####
emm_outcome <- emmeans(model, ~ one_year_outcome, lmerTest.limit = 443344)
contrast(emm_outcome, "pairwise")

#### Annotations for plot ####
contrast <- contrast %>%
  mutate(stars = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ "n.s."
  ))

annotations_good <- c(
  contrast$stars[which(contrast$one_year_outcome == "good" &
                         contrast$contrast == "Not Involved - Involved")],
  contrast$stars[which(contrast$one_year_outcome == "good" &
                         contrast$contrast == "Not Implanted - Involved")],
  contrast$stars[which(contrast$one_year_outcome == "good" &
                         contrast$contrast == "Not Implanted - Not Involved")]
)

annotations_bad <- c(
  contrast$stars[which(contrast$one_year_outcome == "bad" &
                         contrast$contrast == "Not Involved - Involved")],
  contrast$stars[which(contrast$one_year_outcome == "bad" &
                          contrast$contrast == "Involved - Not Implanted")],
  contrast$stars[which(contrast$one_year_outcome == "bad" &
                         contrast$contrast == "Not Involved - Not Implanted")]
)

y_bad  <- c(1.5, 1.6, 1.7)
y_good <- y_bad

comparisons <- list(
  c("Not Involved", "Involved"),
  c("Not Implanted", "Not Involved"),
  c("Not Implanted", "Involved")
)

#### Bar Plot ####
ggplot(emm_df, aes(x = group, y = emmean, fill = group)) +
  geom_bar(stat = "identity", color = "black", size = 0.4, width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  facet_wrap(~ one_year_outcome) +
  labs(x = "Connection Between",
       y = "dMRI connection abnormality") +
  scale_fill_manual(values = c("Not Implanted" = "#0076c0",
                               "Not Involved" = "#a30234",
                               "Involved" = "#67771a")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        legend.position = "none") +
  
  # bad outcome annotations
  geom_signif(data = emm_df %>% filter(one_year_outcome == "Bad 1-Year Post-Surgical Outcome"),
              comparisons = comparisons,
              annotations = annotations_bad,
              y_position = y_bad,
              tip_length = 0.02,
              textsize = 5,
              fontface = "bold") +
  
  # good outcome annotations
  geom_signif(data = emm_df %>% filter(one_year_outcome == "Good 1-Year Post-Surgical Outcome"),
              comparisons = comparisons,
              annotations = annotations_good,
              y_position = y_good,
              tip_length = 0.02,
              textsize = 5,
              fontface = "bold")
