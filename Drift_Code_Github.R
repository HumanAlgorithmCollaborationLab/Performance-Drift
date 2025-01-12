# Load necessary packages
library(ggplot2)
library(reshape)
library(dplyr)
library(matrixStats)
library(MASS)
library(stddiff)
library(stringr)
library(readxl)
library(plyr)

# Merge data across thresholds
perf <- as.data.frame(cbind(perf_90, perf_all[, -c(1, 2)], perf_9899))
perf <- perf[, c("yr", "X_FREQ_", "TP_50", "TP_70", "TP_90", "TP_95", "TP_98", "TP_99", "TN_50", "TN_70", "TN_90", "TN_95", "TN_98", "TN_99", "FP_50", "FP_70", "FP_90", "FP_95", "FP_98", "FP_99", "FN_50", "FN_70", "FN_90", "FN_95", "FN_98", "FN_99")]
names(perf)[2] <- "N"

# Performance Metric calculations: 
#   TPR = #TP / (#TP + #FN)
#   FPR = #FP / (#FP + #TN)
#   PPV = #TP / (#TP + #FP)
#   NPV = #TN / (#FN + #TN)
#   Accuracy = (#TP + #TN)/ (#TP + #TN + #FP + #FN)
#   F1 score = (2 * PPV * TPR) / (PPV+ TPR)

# Calculate performance metrics
Year <- perf$yr
TPR <- perf[, 3:8] / (perf[, 3:8] + perf[, 21:26])
FPR <- perf[, 15:20] / (perf[, 15:20] + perf[, 9:14])
PPV <- perf[, 3:8] / (perf[, 3:8] + perf[, 15:20])
NPV <- perf[, 9:14] / (perf[, 9:14] + perf[, 21:26])
Accuracy <- (perf[, 3:8] + perf[, 9:14]) / (perf[, 3:8] + perf[, 9:14] + perf[, 15:20] + perf[, 21:26])
F1 <- (2 * TPR * PPV) / (TPR + PPV)

metrics <- as.data.frame(cbind(Year, TPR, FPR, PPV, NPV, Accuracy, F1))
names(metrics) <- c("Year", "TPR_50", "TPR_70", "TPR_90", "TPR_95", "TPR_98", "TPR_99", "FPR_50", "FPR_70", "FPR_90", "FPR_95", "FPR_98", "FPR_99", "PPV_50", "PPV_70", "PPV_90", "PPV_95", "PPV_98", "PPV_99", "NPV_50", "NPV_70", "NPV_90", "NPV_95", "NPV_98", "NPV_99", "Accuracy_50", "Accuracy_70", "Accuracy_90", "Accuracy_95", "Accuracy_98", "Accuracy_99", "F1_50", "F1_70", "F1_90", "F1_95", "F1_98", "F1_99")

### Drift calculations
metrics[6, 2:ncol(metrics)] <- metrics[5, 2:ncol(metrics)] - metrics[1, 2:ncol(metrics)]
metrics[7, 2:ncol(metrics)] <- metrics[3, 2:ncol(metrics)] - metrics[1, 2:ncol(metrics)]
metrics[8, 2:ncol(metrics)] <- metrics[5, 2:ncol(metrics)] - metrics[4, 2:ncol(metrics)]
metrics[6:8, 1] <- c("2016-2020[2017-2021]", "2016-2018[2017-2019]", "2019-2020[2020-2021]")
metrics_melt <- melt(metrics, id = "Year")
metrics_melt[c("Metric", "Threshold")] <- str_split_fixed(metrics_melt$variable, '_', 2)

# Quantify drift with confidence intervals
boot_grad <- apply(boot_grad, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
boot_grad <- as.data.frame(t(boot_grad))
colnames(boot_grad) <- c("LB", "UB")
boot_grad[c("Metric", "Year", "Threshold")] <- str_split_fixed(rownames(boot_grad), '_', 3)
boot_grad$variable <- rownames(boot_grad)

boot_grad_9899 <- apply(boot_grad_9899, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
boot_grad_9899 <- as.data.frame(t(boot_grad_9899))
colnames(boot_grad_9899) <- c("LB", "UB")
boot_grad_9899[c("Metric", "Year", "Threshold")] <- str_split_fixed(rownames(boot_grad_9899), '_', 3)
boot_grad_9899$variable <- rownames(boot_grad_9899)

boot_grad <- rbind(boot_grad, boot_grad_9899)

# Combine all confidence interval data
combined <- rbind(boot_grad, boot_sud, boot_over, boot_2017)
combined$Metric_2 <- combined$Metric
combined$Metric[combined$Metric == "Overall"] <- combined$Year[combined$Metric == "Overall"]
combined$Metric[combined$Metric == "Gradual"] <- combined$Year[combined$Metric == "Gradual"]
combined$Metric[combined$Metric == "Sudden"] <- combined$Year[combined$Metric == "Sudden"]

combined$Year[combined$Metric_2 == "Overall"] <- "2016-2020[2017-2021]"
combined$Year[combined$Metric_2 == "Gradual"] <- "2016-2018[2017-2019]"
combined$Year[combined$Metric_2 == "Sudden"] <- "2019-2020[2020-2021]"

combined <- combined[order(combined$Year, combined$Metric, combined$Threshold),]
metrics_melt <- metrics_melt[order(metrics_melt$Year, metrics_melt$Metric, metrics_melt$Threshold),]

metrics_melt$Lower_Bound <- combined$LB
metrics_melt$Upper_Bound <- combined$UB
metrics_melt[, c("value", "Lower_Bound", "Upper_Bound")] <- metrics_melt[, c("value", "Lower_Bound", "Upper_Bound")] * 100

# Exporting supplemental tables
#eTable 4 in Supplement 1
performance <- metrics_melt[!(metrics_melt$Year %in% c("2016-2020[2017-2021]", "2016-2018[2017-2019]", "2019-2020[2020-2021]")), -2]
performance <- performance[, c("Year", "Metric", "Threshold", "value", "Lower_Bound", "Upper_Bound")]
write.table(performance, "~/Desktop/20241203_performance_metrics.txt", sep = "\t", quote = FALSE)

#eTable 5 in Supplement 1
drift <- metrics_melt[(metrics_melt$Year %in% c("2016-2020[2017-2021]", "2016-2018[2017-2019]", "2019-2020[2020-2021]")), -2]
drift <- drift[, c("Year", "Metric", "Threshold", "value", "Lower_Bound", "Upper_Bound")]
drift$Year <- revalue(drift$Year, c("2016-2020[2017-2021]" = "Overall", "2016-2018[2017-2019]" = "Gradual", "2019-2020[2020-2021]" = "Sudden"))
write.table(drift, "~/Desktop/20241203_drift.txt", sep = "\t", quote = FALSE)

# Generate figures for drift analysis
# Figure 1: Performance drift across time periods at 90th percentile threshold
ggplot(drift[(drift$Threshold == 90),], aes(x = Year, y = value, group = Year, color = Year, shape = Year)) +
  geom_point() +
  facet_wrap(~Metric) +
  theme_bw() +
  labs(x = "Time Period", y = "Absolute Change (%)") +
  theme(axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid = element_blank()) +
  scale_shape_manual(values = c(1, 16, 17)) +
  scale_color_manual(values = c("red", "orange", "black")) +
  geom_hline(yintercept = 0, color = "red", linetype = 1) +
  geom_errorbar(aes(ymin = Lower_Bound, ymax = Upper_Bound), width = 0.2)

#eFigure 1 in Supplement 1: Performance drift across time periods, at 50th, 70th, and 95th, 98th and 99th percentile threshold 
ggplot(drift[!(drift$Threshold == 90),], aes(x=Year, y=value, group_by = Year, color = Year, shape = Year)) + 
  geom_point()+
  facet_grid(vars(Metric), vars(Threshold)) + 
  theme_bw() + 
  labs(x= "Time Period", y =  "Absolute Change (%)")+ 
  theme(axis.title = element_text(size=10, face = "bold"), 
        axis.text=element_text(size = 10, face ="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"), 
        axis.line = element_line(size = 1), axis.ticks = element_line(size = 0.5), 
        panel.grid = element_blank()) + 
  scale_shape_manual(values = c(1,16, 17)) +
  scale_color_manual(values = c("red", "orange", "black")) +
  geom_hline(yintercept = 0, color = "red", linetype = 1)+ 
  geom_errorbar(aes(ymin = Lower_Bound, ymax = Upper_Bound), width = 0.2)


###Quantifying changes in false positives and true positives identified
# Calculate changes in false positives
controls_2020 <- perf$FP_90[perf$yr == 2020] + perf$TN_90[perf$yr == 2020]
controls_2018 <- perf$FP_90[perf$yr == 2018] + perf$TN_90[perf$yr == 2018]
drift_fpr <- drift[drift$Metric == "FPR",]
drift_fpr$control <- c(rep(controls_2018, 6), rep(controls_2020, 6), rep(controls_2020, 6))
drift_fpr$missed <- drift_fpr$control * (drift_fpr$value / 100)
drift_fpr$missed_lb <- drift_fpr$control * (drift_fpr$Lower_Bound / 100)
drift_fpr$missed_ub <- drift_fpr$control * (drift_fpr$Upper_Bound / 100)

# Generate bar plot for false positives
# eFigure 2 in Supplement 1: Change in false positives for all thresholds
ggplot(drift_fpr, aes(x = Year, y = missed)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = missed_lb, ymax = missed_ub), width = 0.2, color = "orange") +
  labs(y = "Change in False Positives Identified", x = "Period*") +
  facet_wrap(~Threshold) +
  theme_bw() +
  theme(axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid = element_blank()) +
  geom_hline(yintercept = 0, color = "red", linetype = 1)

# Calculate changes in true positives
cases_2020 <- perf$TP_90[perf$yr == 2020] + perf$FN_90[perf$yr == 2020]
cases_2018 <- perf$TP_90[perf$yr == 2018] + perf$FN_90[perf$yr == 2018]
drift_tpr <- drift[drift$Metric == "TPR",]
drift_tpr$case <- c(rep(cases_2018, 6), rep(cases_2020, 6), rep(cases_2020, 6))
drift_tpr$missed <- drift_tpr$case * (drift_tpr$value / 100)
drift_tpr$missed_lb <- drift_tpr$case * (drift_tpr$Lower_Bound / 100)
drift_tpr$missed_ub <- drift_tpr$case * (drift_tpr$Upper_Bound / 100)

# Generate bar plot for true positives
# eFigure 3: Change in true positives for all thresholds
ggplot(drift_tpr, aes(x = Year, y = missed)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = missed_lb, ymax = missed_ub), width = 0.2, color = "orange") +
  labs(y = "Change in True Positives Identified", x = "Period*") +
  facet_wrap(~Threshold) +
  theme_bw() +
  theme(axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 0.5),
        panel.grid = element_blank()) +
  geom_hline(yintercept = 0, color = "red", linetype = 1)


### Quantifying Reclassification Rates across Thresholds (50th, 70th, 90th, 95th, 98th, and 99th percentiles)

# Calculate classification rates across thresholds
Year <- perf$yr
N <- perf$N
TP <- perf[, 3:8] / perf$N
FP <- perf[, 15:20] / perf$N
TN <- perf[, 9:14] / perf$N
FN <- perf[, 21:26] / perf$N
Cases <- (perf[, 3:8] + perf[, 21:26]) / perf$N
Controls <- (perf[, 15:20] + perf[, 9:14]) / perf$N

# Combine data into one dataset and reshape for analysis
classification <- as.data.frame(cbind(Year, N, TP, FP, TN, FN, Cases[, 1]))
names(classification)[7] <- "Prevalence"
classification <- melt(classification, id = c("Year", "N"))

# Calculate 95% confidence intervals
classification$Lower_Bound <- classification$value - 1.96 * sqrt((classification$value * (1 - classification$value)) / classification$N)
classification$Upper_Bound <- classification$value + 1.96 * sqrt((classification$value * (1 - classification$value)) / classification$N)

# Convert values to percentages
classification[, 4:6] <- classification[, 4:6] * 100

# Split "variable" column into "Metric" and "Threshold"
classification[c("Metric", "Threshold")] <- str_split_fixed(classification$variable, '_', 2)

# Rearrange and rename columns
classification <- classification[, c(-2, -3)]
names(classification)[2] <- "Classification_Percentage"
classification <- classification[, c("Year", "Metric", "Threshold", "Classification_Percentage", "Lower_Bound", "Upper_Bound")]

# Table 2: Classification rates using 90th percentile threshold
classification_90 <- classification[classification$Threshold == 90,]
write.table(classification_90, "~/Desktop/20241203_reclassification_percentage_90.txt", sep = "\t", quote = FALSE)

#eTable 6 in Supplement 1: 
classification_other <- classification[!(classification$Threshold == 90),]
write.table(classification_other, "~/Desktop/20241203_reclassification_percentage_other.txt", sep = "\t", quote = FALSE)

### Quantifying Covariate Shifts
#Reference: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3472075/

# Remove the extra variable "dxChrAirObstNEC730_c" from the covariate dataset
cov <- cov[, !names(cov) %in% "dxChrAirObstNEC730_c"]

# Convert columns to prevalence rates
cov[, 3:ncol(cov)] <- cov[, 3:ncol(cov)] / cov[, 2]

# Calculate Standardized Mean Difference (SMD) for different time periods
cov[6, ] <- (cov[5, ] - cov[1, ]) / (sqrt(((cov[5, ] * (1 - cov[5, ])) + (cov[1, ] * (1 - cov[1, ]))) / 2))  # Overall SMD
cov[7, ] <- (cov[3, ] - cov[1, ]) / (sqrt(((cov[3, ] * (1 - cov[3, ])) + (cov[1, ] * (1 - cov[1, ]))) / 2))  # Gradual SMD
cov[8, ] <- (cov[5, ] - cov[4, ]) / (sqrt(((cov[5, ] * (1 - cov[5, ])) + (cov[4, ] * (1 - cov[4, ]))) / 2))  # Sudden SMD

cov$yr <- c(2016:2020, "Overall_SMD", "Gradual_SMD", "Sudden_SMD")

# Prepare SMD dataset
smd <- cov[c(-1:-5), -2]
rownames(smd) <- smd$yr
smd <- smd[, -1]
names(smd) <- tolower(names(smd))

# Organize coefficient dataset for merging with variable type and odds ratio
coeff$Variable <- tolower(coeff$Variable)
smd <- smd[, coeff$Variable]  # Rearrange columns to match coefficient order
smd[4, ] <- coeff$Type
smd[5, ] <- coeff$Odds_Ratio
smd[6, ] <- coeff$Description
smd[7, ] <- coeff$ID
rownames(smd) <- c(rownames(smd)[1:3], "Type", "Odds_Ratio", "Description", "ID")

# Transform SMD dataset
smd <- as.data.frame(t(smd))
smd$Overall_SMD <- as.numeric(smd$Overall_SMD)
smd$Gradual_SMD <- as.numeric(smd$Gradual_SMD)
smd$Sudden_SMD <- as.numeric(smd$Sudden_SMD)
smd$Odds_Ratio <- as.numeric(smd$Odds_Ratio)

# Label significant covariates
smd$ID_Gradual <- ""
smd$ID_Gradual[(smd$Gradual_SMD < -0.1) | (smd$Gradual_SMD > 0.1)] <- smd$ID[(smd$Gradual_SMD < -0.1) | (smd$Gradual_SMD > 0.1)]
smd$ID_Gradual[(smd$Odds_Ratio < 0.5) | (smd$Odds_Ratio > 1.5)] <- smd$ID[(smd$Odds_Ratio < 0.5) | (smd$Odds_Ratio > 1.5)]
smd$ID_Gradual[is.na(smd$ID_Gradual)] <- ""

smd$ID_Sudden <- ""
smd$ID_Sudden[(smd$Sudden_SMD < -0.1) | (smd$Sudden_SMD > 0.1)] <- smd$ID[(smd$Sudden_SMD < -0.1) | (smd$Sudden_SMD > 0.1)]
smd$ID_Sudden[(smd$Odds_Ratio < 0.5) | (smd$Odds_Ratio > 1.5)] <- smd$ID[(smd$Odds_Ratio < 0.5) | (smd$Odds_Ratio > 1.5)]

smd$ID_Overall <- ""
smd$ID_Overall[(smd$Overall_SMD < -0.1) | (smd$Overall_SMD > 0.1)] <- smd$ID[(smd$Overall_SMD < -0.1) | (smd$Overall_SMD > 0.1)]
smd$ID_Overall[(smd$Odds_Ratio < 0.5) | (smd$Odds_Ratio > 1.5)] <- smd$ID[(smd$Odds_Ratio < 0.5) | (smd$Odds_Ratio > 1.5)]

#Export table, Significant Covariates for each time period, description, odds ratio, SMD, lower bound and upper bound, removing all extra lines that do not include covariate shifts
#eTable 7 in Supplement 1
smd_subset <- smd[!((smd$ID_Sudden == "") & (smd$ID_Gradual == "") & (smd$ID_Overall == "")), ]
write.table(smd_subset, "~/Desktop/20240829_smd_subset.txt", sep = "\t", quote = FALSE)

# Figure 2: Shift in Overall Time period
ggplot(smd, aes(x = Odds_Ratio, y = Overall_SMD, color = Type, label = ID_Overall, shape = Type)) +
  geom_point(size = 2.5) +
  facet_wrap(~ Type) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(0.5, 1.5), linetype = "dotted", color = "red") +
  geom_hline(yintercept = c(0.1, -0.1), linetype = "solid", color = "red") +
  theme_bw() +
  labs(x = "Model Odds Ratio (OR)", y = "Standardized Mean Difference 2016-2020 [2017-2021]") +
  geom_text(aes(label = ID_Overall), vjust = -0.5) +
  theme(axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        strip.text.x = element_text(size = 12)) +
  scale_shape_manual(values = c(16, 17, 15, 18, 7))

# eFigure 4 in Supplement 1, Shift in Sudden Time period
ggplot(smd, aes(x = Odds_Ratio, y = Sudden_SMD, color = Type, label = ID_Sudden, shape = Type)) +
  geom_point(size = 2.5) +
  facet_wrap(~ Type) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(0.5, 1.5), linetype = "dotted", color = "red") +
  geom_hline(yintercept = c(0.1, -0.1), linetype = "solid", color = "red") +
  theme_bw() +
  labs(x = "Model Odds Ratio (OR)", y = "Standardized Mean Difference 2019-2020 [2020-2021]") +
  geom_text(aes(label = ID_Sudden), vjust = -0.5) +
  theme(axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        strip.text.x = element_text(size = 12)) +
  scale_shape_manual(values = c(16, 17, 15, 18, 7))

#eFigure 5 in Supplement 1, Shift in Gradual Time period
ggplot(smd, aes(x = Odds_Ratio, y = Gradual_SMD, color = Type, label = ID_Gradual, shape = Type)) +
  geom_point(size = 2.5) +
  facet_wrap(~ Type) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(0.5, 1.5), linetype = "dotted", color = "red") +
  geom_hline(yintercept = c(0.1, -0.1), linetype = "solid", color = "red") +
  theme_bw() +
  labs(x = "Model Odds Ratio (OR)", y = "Standardized Mean Difference 2016-2018 [2017-2019]") +
  geom_text(aes(label = ID_Gradual), vjust = -0.5) +
  theme(axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5, face = "bold"),
        panel.grid = element_blank(),
        strip.text.x = element_text(size = 12)) +
  scale_shape_manual(values = c(16, 17, 15, 18, 7))

# Validation of Drivers by Removing Drivers and Recalculating Drift
# Perform validation only at the 90th threshold
validation <- validation[, c("yr", "X_FREQ_", "TP_90_nodriv", "TP_90_nolab", "TP_90_nodem", "TP_90_noutil", "TP_90_recal19", "TP_90_recal18", 
                             "TN_90_nodriv", "TN_90_nolab", "TN_90_nodem", "TN_90_noutil", "TN_90_recal19", "TN_90_recal18", 
                             "FP_90_nodriv", "FP_90_nolab", "FP_90_nodem", "FP_90_noutil", "FP_90_recal19", "FP_90_recal18", 
                             "FN_90_nodriv", "FN_90_nolab", "FN_90_nodem", "FN_90_noutil", "FN_90_recal19", "FN_90_recal18")]
Year <- validation$yr
TPR <- validation[, 3:8] / (validation[, 3:8] + validation[, 21:26])
FPR <- validation[, 15:20] / (validation[, 15:20] + validation[, 9:14])
PPV <- validation[, 3:8] / (validation[, 3:8] + validation[, 15:20])
NPV <- validation[, 9:14] / (validation[, 9:14] + validation[, 21:26])
Accuracy <- (validation[, 3:8] + validation[, 9:14]) / (validation[, 3:8] + validation[, 21:26] + validation[, 9:14] + validation[, 15:20])
F1 <- (2 * TPR * PPV) / (TPR + PPV)

# Check overall drift for all metrics
(TPR[5, ] - TPR[1, ]) * 100  # Overall TPR drift
(FPR[5, ] - FPR[1, ]) * 100  # Overall FPR drift
(PPV[5, ] - PPV[1, ]) * 100  # Overall PPV drift
(NPV[5, ] - NPV[1, ]) * 100  # Overall NPV drift
(Accuracy[5, ] - Accuracy[1, ]) * 100  # Overall Accuracy drift
(F1[5, ] - F1[1, ]) * 100  # Overall F1 drift

#eTable 8 in Supplement 1 was generated from listing results in the section above in a seperate Excel document


### Clinical Impact studies 
#risk_counts and pall_counts datasets
pall_counts <- pall_counts[pall_counts$fiscal == 1, c(1,4,5,8, 12, 16,20,24)]
pall_counts <- pall_counts[c(-1:-7),]
risk_counts_subset <- risk_counts[,c(1,3,4,7,11,15,19,23)]
risk_counts_subset <- risk_counts_subset[c(-1:-3),]

##Table 4
#Quality metric, using 90th percentile risk threshold: 
pall_hr_tp_fp <- (pall_counts[,5] + pall_counts[,6])/ (risk_counts_subset[,5] + risk_counts_subset[,6])

#False positive rate among high-risk Veterans, using 90th percentile risk threshold:
hr_fp <-  risk_counts_subset[,6]/ (risk_counts_subset[,5] + risk_counts_subset[,6])

#False positive rate among high-risk Veterans who received palliative care, using 90th percentile risk threshold: 
fp_in_pall <- pall_counts$FP_90/pall_counts$risk_90

##eTable 9: Sensitivity analysis at 98th and 99th percentile thresholds
#quality metric: Percentage of high-risk pateints who receive palliative care
qm_98 <- pall_counts_9899$FP_98[pall_counts_9899$fiscal==1]/risk_counts_9899$risk_98[pall_counts_9899$fiscal==1]
qm_99 <- pall_counts_9899$FP_99[pall_counts_9899$fiscal==1]/pall_counts_9899$risk_99[pall_counts_9899$fiscal==1]

#False positive rate among high-risk Veterans
fp_hr_98 <- risk_counts_9899$FP_98/ (risk_counts_9899$FP_98 + risk_counts_9899$TP_98)
fp_hr_99 <- risk_counts_9899$FP_99/ (risk_counts_9899$FP_99 + risk_counts_9899$TP_99)

#False positive rate among high-risk Veterans who received palliative care 
fp_in_pall_98 <- pall_counts_9899$FP_98[pall_counts_9899$fiscal==1]/pall_counts_9899$risk_98[pall_counts_9899$fiscal==1]
fp_in_pall_99 <- pall_counts_9899$FP_99[pall_counts_9899$fiscal==1]/pall_counts_9899$risk_99[pall_counts_9899$fiscal==1]




