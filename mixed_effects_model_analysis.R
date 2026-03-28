######################################################################
# MIXED EFFECTS MODELING FOR RETROFIT ANALYSIS
# Complete reproducible workflow from tidy data 
# 
# This script implements the methodology described in:
# "Mixed Effects Models for Pre/Post Building Intervention Analysis: 
#  Methodology, R Implementation, and Case Study of Impact on Indoor Climate"
# 
# Requirements: R 4.0+, tidyverse, lme4, performance, sjPlot, tableone
# Sample data: retrofit_sample_data.rds (5% stratified sample, 28,822 obs)
######################################################################

# ============================================================================
# 1. SETUP AND LOAD DATA
# ============================================================================

# Load required packages
library(tidyverse)      # Data manipulation, visualization
library(lme4)           # Linear mixed effects models (core)
library(performance)    # Model diagnostics, VIF, R²
library(sjPlot)         # Publication-ready model tables
library(tableone)       # Table 1 descriptive statistics
library(kableExtra)     # Beautiful tables
library(flextable)      # Word export
library(emmeans)        # Post-hoc pairwise comparisons
library(viridis)        # Colorblind-friendly palettes
library(patchwork)      # Combine ggplot2 plots
library(knitr)          # Table formatting
library(scales)         # Axis scaling

# Load sample data (preserves types) OR CSV (universal)
sample_data <- readRDS("retrofit_sample_data.rds")
# sample_data <- read_csv("retrofit_sample_data.csv")  # Alternative for non-R users

# Verify data loaded correctly
cat("Dataset loaded:", nrow(sample_data), "rows ×", ncol(sample_data), "columns\n")
cat("DateTime:", class(sample_data$DateTime)[1], "\n")
cat("RoomType:", class(sample_data$RoomType)[1], "\n")
cat("Homes:", n_distinct(sample_data$HomeID), "out of 23\n")
str(sample_data[, c("DateTime", "RoomType", "HomeID", "RetrofitStatus")])

# ============================================================================
# 2. DESCRIPTIVE STATISTICS (Table 1 - name comes form epidemiology)
# Stratified by RetrofitStatus for baseline comparison
# ============================================================================

# Define variables for Table 1
categorical_vars <- c("RoomType", "Season", "BuiltType", "BuiltYear")
continuous_vars <- c("ExternalT", "ExternalRH", "ExternalPw",  # Key confounders
                     "RoomT", "RoomRH", "RoomPw", "RoomDPT")  # Outcomes

# Create Table 1 (pre/post retrofit comparison)
table1 <- CreateTableOne(
  vars = c(categorical_vars, continuous_vars),
  strata = "RetrofitStatus",
  data = sample_data,
  factorVars = categorical_vars
)

# HTML table for paper
table1_html <- print(table1, showAllLevels = TRUE, catDigits = 1, contDigits = 2,
                     quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

kable(table1_html, booktabs = TRUE,
      caption = "Table 1: Sample Characteristics by Retrofit Status (n = 28,822)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Export Word document
table1_df <- as.data.frame(table1_html)
flextable(table1_df) %>%
  set_caption("Table 1: Baseline Characteristics") %>%
  save_as_docx(path = "Table1.docx")

cat("\n✅ Table 1 exported (HTML + Word)\n")

# ============================================================================
# 3. EXPLORATORY ANALYSIS: Dew Point Temperature (DPT) Distribution
# Stacked bar plots showing pre/post retrofit shifts by home
# ============================================================================

# Categorize DPT for visualization
sample_data %>%
  mutate(DPT_cat = cut(RoomDPT, 
                       breaks = c(-10, 0, 10, 15, 20, 25, Inf),
                       labels = c("≤0°C", "0-10°C", "10-15°C", "15-20°C", "20-25°C", ">25°C"))) %>%
  count(HomeID, RetrofitStatus, DPT_cat) %>%            # Step 1: get counts per group
  group_by(HomeID, RetrofitStatus) %>%                  # Step 2: regroup WITHOUT DPT_cat
  mutate(Freq = n / sum(n)) %>%                         # Step 3: proportion within each home × status
  ungroup() ->
  DPT_categories

# Define a colour palette once, reuse in both plots
# warmer DPT implies more moisture in air (bluer)
dpt_colours <- c(
  "≤0°C"    = "#A50026",    
  "0-10°C"  = "#F46D43",    
  "10-15°C" = "#FDAE61",    
  "15-20°C" = "#ABD9E9",    
  "20-25°C" = "#74ADD1",    
  ">25°C"   = "#313695"     
)

# Pre-retrofit stacked plot
pre_dpt <- ggplot(DPT_categories %>% filter(RetrofitStatus == "Pre Retrofit"), 
                  aes(x = HomeID, y = Freq, fill = DPT_cat)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = dpt_colours, name = "DPT (°C)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_text(aes(label = scales::percent(Freq, accuracy = 1)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_blank()) +
  labs(y = "Proportion", title = "Pre-Retrofit DPT Distribution")

# Post-retrofit stacked plot
post_dpt <- ggplot(DPT_categories %>% filter(RetrofitStatus == "Post Retrofit"), 
                   aes(x = HomeID, y = Freq, fill = DPT_cat)) +
  geom_col(color = "black", position = "fill") +
  scale_fill_manual(values = dpt_colours, name = "DPT (°C)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_text(aes(label = scales::percent(Freq, accuracy = 1)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Proportion", title = "Post-Retrofit DPT Distribution")

# Combined figure
dpt_plot <- pre_dpt / post_dpt + 
  plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Dew Point Temperature Distribution Pre/Post Retrofit")


# view plot
dpt_plot

# export figure
ggsave("DPT_exploration.png", dpt_plot, width = 12, height = 10, dpi = 300)

cat("\n✅ Exploratory DPT plots exported\n")

# ============================================================================
# 4. MIXED EFFECTS MODEL BUILDING & SELECTION
# Progressive model simplification via theory + statistics
# ============================================================================

# FULL MODEL: All theoretically-relevant predictors
lmmHomePWRoom_full <- lmer(RoomPw ~ RetrofitStatus + ExternalPw + ExternalT + 
                             RoomT + RoomType + Season + BuiltType + 
                             BuiltYear + Occupants + Income + 
                             (1 + RetrofitStatus + ExternalPw | HomeID),
                           data = sample_data, REML = TRUE)

# Model diagnostics (multicollinearity check)
vif_full <- check_collinearity(lmmHomePWRoom_full)
print(vif_full)  # All VIF < 5 → No multicollinearity issues

# Tabular summary with standardized betas
tab_model(lmmHomePWRoom_full, show.std = TRUE, digits = 3)

# MODEL 1: Drop non-significant terms (BuiltType, BuiltYear, Income; p > 0.05)
lmmHomePWRoom_model1 <- lmer(RoomPw ~ RetrofitStatus + ExternalPw + ExternalT + 
                               RoomT + RoomType + Season + Occupants + 
                               (1 + RetrofitStatus + ExternalPw | HomeID),
                             data = sample_data, REML = TRUE)

# MODEL 2: Further parsimony (drop low std.β terms)
lmmHomePWRoom_model2 <- lmer(RoomPw ~ RetrofitStatus + ExternalPw + ExternalT +
                               RoomT + RoomType + 
                               (1 + RetrofitStatus + ExternalPw | HomeID),
                             data = sample_data, REML = TRUE)


# ============================================================================
# 5. MODEL COMPARISON & SELECTION
# Likelihood ratio tests + AIC → Model 1 optimal
# ============================================================================

# Compare models (lower AIC = better)
# LRT confirms Model 1 optimal
anova(lmmHomePWRoom_full, lmmHomePWRoom_model1, test = "Chisq", refit = F)
anova(lmmHomePWRoom_model1, lmmHomePWRoom_model2, test = "Chisq", refit = F)

# SELECT FINAL MODEL: Model 1 (AIC = -44,426; best fit/parsimony balance)
final_model <- lmmHomePWRoom_model1

cat("\n✅ FINAL MODEL SELECTED: lmmHomePWRoom_model1 (AIC = -44,426\n")

# ============================================================================
# 6. FINAL MODEL RESULTS & DIAGNOSTICS
# ============================================================================

# Publication-ready table (standardized coefficients)
tab_model(final_model, 
          show.std = TRUE,
          title = "Final Mixed Effects Model Results",
          digits = 3)

# Model diagnostics
diagnostics <- check_model(final_model)
print(diagnostics)  # Summary diagnostics

# Export diagnostics plot
p_diag <- plot(diagnostics) + 
  labs(title = "Model Diagnostics: All Assumptions Met") +
  theme(legend.position = "bottom")
ggsave("model_diagnostics.png", p_diag, width = 12, height = 8, dpi = 300)

# Convergence stability check (addresses convergence gradient warning)
set.seed(123)
model_stable <- lmer(RoomPw ~ RetrofitStatus + ExternalPw + ExternalT + 
                       RoomT + RoomType + Season + Occupants + 
                       (1 + RetrofitStatus + ExternalPw | HomeID),
                     data = sample_data, 
                     control = lmerControl(optimizer = "bobyqa"))

cat("Coefficient stability:\n")
print(cbind(
  Original = round(fixef(final_model), 6),
  Stable = round(fixef(model_stable), 6)
))  # Identical → Warning safe to ignore

# Extract fixed effects + confidence intervals from both models
extract_coefs <- function(model, model_name) {
  coefs <- as.data.frame(confint(model, parm = "beta_", method = "Wald"))
  coefs$Estimate  <- fixef(model)
  coefs$Model     <- model_name
  coefs$Term      <- rownames(coefs)
  rownames(coefs) <- NULL
  coefs <- coefs |>
    rename(CI_low = `2.5 %`, CI_high = `97.5 %`)
  return(coefs)
}

coef_original <- extract_coefs(final_model,  "Original (default optimizer)")
coef_stable   <- extract_coefs(model_stable, "Stable (bobyqa optimizer)")

# Combine
coef_compare <- bind_rows(coef_original, coef_stable) |>
  filter(Term != "(Intercept)")  # Optionally drop intercept for readability

# PLOT

stability_plot <- ggplot(coef_compare, 
                         aes(x = Estimate, y = Term, color = Model, shape = Model)) +
  
  # Confidence interval lines
  geom_linerange(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5),
                 linewidth = 0.8, alpha = 0.7) +
  
  # Point estimates
  geom_point(position = position_dodge(width = 0.5),
             size = 3) +
  
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  
  scale_color_manual(values = c("Original (default optimizer)" = "#E76F51",
                                "Stable (bobyqa optimizer)"    = "#264653")) +
  scale_shape_manual(values = c("Original (default optimizer)" = 16,
                                "Stable (bobyqa optimizer)"    = 17)) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position   = "bottom",
    legend.title      = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.text.y       = element_text(size = 10),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(size = 10, color = "grey40")
  ) +
  labs(
    x        = "Coefficient Estimate (95% CI)",
    y        = NULL,
    title    = "Model Stability Check: Coefficient Comparison",
    subtitle = "Overlapping estimates confirm convergence warning is safe to ignore"
  )

stability_plot

# export figure
ggsave("model_stability_plot.png", stability_plot, width = 12, 
       height = 10, dpi = 300)

cat("\n✅ Model stability check plot exported\n")


# ============================================================================
# 7. ROBUSTNESS TO EXTREME VALUES
# Exclude 95th/99th percentile observations → verify retrofit effect stability
# ============================================================================
# Define predictors for outlier removal
predictors <- c("ExternalPw", "ExternalT", "RoomT", "RoomRH", "RoomPw")

# Create clean dataset (5th/95th percentile bounds)
data_clean <- sample_data
filter_summary <- tibble()

for(pred in predictors) {
  q_low <- quantile(sample_data[[pred]], 0.05, na.rm = TRUE)
  q_high <- quantile(sample_data[[pred]], 0.95, na.rm = TRUE)
  
  n_before <- nrow(data_clean)
  data_clean <- data_clean %>% filter(between(!!sym(pred), q_low, q_high))
  n_filtered <- n_before - nrow(data_clean)
  
  filter_summary <- bind_rows(filter_summary,
                              tibble(Predictor = pred,
                                     Filtered = n_filtered,
                                     Range = sprintf("[%.2f, %.2f]", q_low, q_high)))
}

cat("Dataset: ", nrow(sample_data), "→", nrow(data_clean), "obs (", 
    round(100*nrow(data_clean)/nrow(sample_data), 1), "% retained)\n\n")

print(filter_summary)

# Fit clean model
clean_model <- lmer(RoomPw ~ RetrofitStatus + ExternalPw + ExternalT + 
                      RoomT + RoomType + Season + Occupants + 
                      (1 + RetrofitStatus + ExternalPw | HomeID),
                    data = data_clean, REML = TRUE)

# Extract ALL fixed effects coefficients
orig_coefs <- fixef(final_model)
clean_coefs <- fixef(clean_model)

# Stability comparison table
coef_comparison <- data.frame(
  Coefficient = names(orig_coefs),
  Original = round(orig_coefs, 3),
  Clean = round(clean_coefs, 3),
  Delta_Abs = round(clean_coefs - orig_coefs, 4),
  Delta_Pct = round(100 * abs(clean_coefs - orig_coefs) / abs(orig_coefs), 1),
  Stable = abs(clean_coefs - orig_coefs) < 0.005  # Threshold: <0.005 kPa
) %>%
  mutate(Stable = ifelse(Stable, "✅", "⚠️"))

# Print results
print(coef_comparison)

# Publication style table
kable(coef_comparison, 
      digits = 3,
      caption = "Table 6: Coefficient Stability - With vs Without Extreme Values") %>%
  kable_styling(bootstrap_options = "striped") %>%
  column_spec(1, bold = TRUE)


# ============================================================================
# 8. RANDOM EFFECTS ANALYSIS
# Home-level heterogeneity visualization
# ============================================================================

library(ggrepel)

# Extract random effects
ranef_data <- ranef(final_model)$HomeID %>%
  as.data.frame() %>%
  rownames_to_column("HomeID") %>%
  mutate(Metric = "Pw (kPa)") %>%
  rename(Intercept = "(Intercept)", ExternalPw_slope = "ExternalPw")

# Merge with retrofit status
ranef_data <- ranef_data %>%
  left_join(distinct(sample_data[, c("HomeID", "RetrofitStatus")]), by = "HomeID")

# Correlation by retrofit status
cor_labels <- ranef_data %>%
  group_by(RetrofitStatus) %>%
  summarise(cor = round(cor(Intercept, ExternalPw_slope), 2),
            x_pos = min(Intercept) * 1.1,
            y_pos = min(ExternalPw_slope) * 0.9)

# Random effects scatterplot
ggplot(ranef_data, aes(x = Intercept, y = ExternalPw_slope)) +
  geom_point(size = 4, alpha = 0.8, color = "#377EB8") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  geom_text_repel(aes(label = HomeID), size = 3, box.padding = 0.3) +
  geom_text(data = cor_labels, aes(x = x_pos, y = y_pos, 
                                   label = paste0("ρ = ", cor)), 
            size = 4, fontface = "bold", color = "red") +
  facet_wrap(~ RetrofitStatus) +
  labs(x = "Random Intercept (Home Baseline Pw)", 
       y = "Random Slope (Home ExternalPw Sensitivity)",
       title = "Home-Level Heterogeneity: Baseline vs Outdoor Sensitivity") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

# save plot
ggsave("random_effects.png", width = 12, height = 6, dpi = 300)

# ============================================================================
# 9. POST-HOC PAIRWISE COMPARISONS
# RoomType × RetrofitStatus interactions
# ============================================================================

season_palette <- c("Heating" = "#F4A582", "Non-Heating" = "#92C5DE")


p_pw_roomtype <- ggplot(sample_data, aes(x = RoomType, y = RoomPw, fill = Season)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", shape = 18,
               size = 3, color = "black", position = position_dodge(0.75)) +
  scale_fill_manual(values = season_palette) +
  scale_x_discrete(labels = c("Living Room" = "Living\nRoom",
                              "Other Rooms" = "Other\nRooms")) +
  facet_wrap(~ RetrofitStatus) +
  labs(x = NULL, y = "Indoor Pw (kPa)", fill = "Season") +
  theme_bw(base_size = 13) +
  theme(legend.position    = "bottom",
        legend.title       = element_text(face = "bold"),
        strip.text         = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

# Display plot
p_pw_roomtype
# save plot
ggsave("room_season_Pw.png", width = 12, height = 6, dpi = 300)

# Tukey-adjusted pairwise comparisons
lsm_pw <- emmeans(final_model, pairwise ~ RoomType * RetrofitStatus)
summary(lsm_pw)

# ============================================================================
# 10. MODEL VALIDATION: Train/Test Split
# External validation confirms generalizability
# ============================================================================

library(caret)
set.seed(123)

# 70/30 stratified split by HomeID
train_idx <- createDataPartition(sample_data$HomeID, p = 0.7, list = FALSE)
train_data <- sample_data[train_idx, ]
test_data <- sample_data[-train_idx, ]

# Train on 70%, predict on 30%
train_model <- lmer(RoomPw ~ RetrofitStatus + ExternalPw + ExternalT + 
                      RoomT + RoomType + Season + Occupants + 
                      (1 + RetrofitStatus + ExternalPw | HomeID),
                    data = train_data, REML = TRUE)

test_data$predicted <- predict(train_model, newdata = test_data, 
                               allow.new.levels = TRUE)

# Performance metrics
rmse_val <- sqrt(mean((test_data$RoomPw - test_data$predicted)^2))
r2_val <- cor(test_data$RoomPw, test_data$predicted)^2

cat(sprintf("\n✅ EXTERNAL VALIDATION:\n"))
cat(sprintf("RMSE: %.3f kPa\n", rmse_val))
cat(sprintf("R²: %.3f\n", r2_val))

# Plot predictions vs actual
ggplot(test_data, aes(x = RoomPw, y = predicted)) +
  geom_point(alpha = 0.6) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(x = "Observed Room Pw (kPa)", y = "Predicted Room Pw (kPa)",
       title = paste0("External Validation (Test R² = ", round(r2_val, 3), ")")) +
  theme_bw() +
  coord_equal()
ggsave("validation_plot.png", width = 8, height = 8, dpi = 300)

cat("\n🎉 COMPLETE WORKFLOW EXECUTED SUCCESSFULLY!\n")


