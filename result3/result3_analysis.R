#### Load libraries ####
library(dplyr)
library(ggplot2)
library(pROC)
library(tidyr)

#### Load data ####
patient_summary <- read.csv("result3/patient_meanMD_summary_scale250.csv")

# Convert to factor and relevel
patient_summary$one_year_outcome <- factor(patient_summary$one_year_outcome, levels = c("good","bad"))

#### Wilcoxon rank-sum tests ####
wilcox_involved      <- wilcox.test(involved ~ one_year_outcome, data = patient_summary)
wilcox_not_implanted <- wilcox.test(not_implanted ~ one_year_outcome, data = patient_summary)
wilcox_not_involved  <- wilcox.test(not_involved ~ one_year_outcome, data = patient_summary)

# One-tailed p-values
cat("Wilcoxon p (Involved):      ", wilcox_involved$p.value/2, "\n")
cat("Wilcoxon p (Not Implanted): ", wilcox_not_implanted$p.value/2, "\n")
cat("Wilcoxon p (Not Involved):  ", wilcox_not_involved$p.value/2, "\n")

#### Calculate AUC from mean z-scores ####
roc_a_full <- roc(patient_summary$one_year_outcome, patient_summary$involved)
roc_b_full <- roc(patient_summary$one_year_outcome, patient_summary$not_implanted)
roc_c_full <- roc(patient_summary$one_year_outcome, patient_summary$not_involved)

cat("\nAUC (Involved):      ", auc(roc_a_full), "\n")
cat("AUC (Not Implanted): ", auc(roc_b_full), "\n")
cat("AUC (Not Involved):  ", auc(roc_c_full), "\n")

#### Summary table ####
data.frame(
  Model = c("Involved", "Not Implanted", "Not Involved"),
  AUC = c(auc(roc_a_full), auc(roc_b_full), auc(roc_c_full)),
  Wilcoxon_p = c(wilcox_involved$p.value/2, wilcox_not_implanted$p.value/2, wilcox_not_involved$p.value/2)
) %>% mutate(across(c(AUC), ~round(., 3)),
             Wilcoxon_p = round(Wilcoxon_p, 4))

#### Violin plot setup : three predictors by outcome ####
plot_data <- patient_summary %>%
  select(ID, one_year_outcome, involved, not_implanted, not_involved) %>%
  pivot_longer(cols = c(involved, not_implanted, not_involved),
               names_to = "connection_type", values_to = "mean_z") %>%
  mutate(
    one_year_outcome = factor(one_year_outcome,
                              levels = c("good", "bad"),
                              labels = c("ILAE 1-2", "ILAE 3+")),
    connection_type = factor(connection_type,
                             levels = c("not_implanted", "not_involved", "involved"),
                             labels = c("Not Implanted", "Not Involved", "Involved"))
  )

# Compute medians per outcome per connection type
medians <- plot_data %>%
  group_by(connection_type, one_year_outcome) %>%
  summarise(median_val = median(mean_z, na.rm = TRUE), .groups = "drop")

plot_data <- plot_data %>%
  mutate(fill_group = interaction(connection_type, one_year_outcome))

#### Violin Plot ####
ggplot(plot_data, aes(x = one_year_outcome, y = mean_z)) +
  
  geom_violin(aes(fill = fill_group), trim = FALSE, scale = "width",
              color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("Involved.ILAE 1-2" = "#67771a",
                               "Involved.ILAE 3+" = "#67771a",
                               "Not Implanted.ILAE 1-2" = "#0076c0",
                               "Not Implanted.ILAE 3+" = "#0076c0",
                               "Not Involved.ILAE 1-2" = "#a30234",
                               "Not Involved.ILAE 3+" = "#a30234"))+
  guides(fill = "none") +
  
  geom_jitter(shape = 21, fill = "white", color = "black", stroke = 1,
              width = 0.08, size = 3, alpha = 0.8) +
  
  geom_segment(data = medians,
               aes(x = as.numeric(factor(one_year_outcome)) - 0.3,
                   xend = as.numeric(factor(one_year_outcome)) + 0.3,
                   y = median_val, yend = median_val),
               inherit.aes = FALSE,
               color = "black", linewidth = 1) +
  
  facet_wrap(~ connection_type) +
  
  labs(x = "1-Year Post-Surgical Outcome",
       y = "Average dMRI connection abnormality") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(face = "bold", size = 9),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 12),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey95"),
        panel.grid.minor.y = element_blank())

#### ROC Curves Plot ####
par(pty = 's')
plot(roc_b_full, col = "#0076c0", lwd = 5,
     legacy.axes = TRUE,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     main = "ROC Curves: Predicting 1-Year Surgical Outcome")
plot(roc_a_full, col = "#67771a", lwd = 5, add = TRUE)
plot(roc_c_full, col = "#a30234", lwd = 5, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "grey50")
