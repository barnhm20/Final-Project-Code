# Libraries and create data

library(causaldata)
library(dagitty)
library(ggdag)
library(tidyverse)
library(tableone)
library(broom)
library(halfmoon)
library(gt)
library(gtsummary)
library(survey)
library(propensity)
library(mice)
library(sandwich)
library(tipr)
library(patchwork)
library(knitr)
proj_data<-causaldata::nsw_mixtape
proj_data<-proj_data[,-1]

## Table 1

proj_data |> 
  tbl_summary(
    by = treat, 
    include = c(age, educ, black, hisp, marr, nodegree, re74, re75)
  ) |> 
  add_overall(last = TRUE) |> 
  modify_caption("Characteristics by Treatment Group") |> 
  as_kable()

## DAG

my_dag<-dagify(
  re78~treat+age+educ+black+hisp+marr+nodegree+re74+re75,
  treat~age+educ+black+hisp+re74+re75,
  nodegree~educ,
  marr~age,
  re75~educ+nodegree+re74,
  re74~educ+nodegree,
  labels = c("re78" = "Real Earnings 1978", 
             "treat" = "Program", 
             "age" = "Age", 
             "educ" = "Education",
             "black" = "Black", 
             "hisp" = "Hispanic", 
             "marr" = "Married", 
             "nodegree" = "No Degree",
             "re74" = "Real Earnings 1974", 
             "re75" = "Real Earnings 1975"
  )
)

ggdag(my_dag, layout = "star",use_labels = "label", text=F) + 
  labs(caption = "Figure 1.1: Proposed DAG of Known Confounders") + theme_void()

## Adjustment Set

ggdag_adjustment_set(my_dag, exposure = "treat", outcome="re78", use_labels = "label", text = F)+ labs(caption = "Figure 1.2: Adjustment Set for Proposed DAG") + theme_void()

## Unweighted Propensity Score Model

ps_model<-glm(treat  ~ age + black + educ + re74 + re75 + hisp,
              data = proj_data,
              family = binomial()
)

proj_data_with_ps <- augment(ps_model, type.predict = "response")

## Unweighted Distributions of Propesity Scores by group

ggplot(proj_data_with_ps,aes(x=.fitted
                             ,group=factor(treat)
                             ,fill=factor(treat))) + 
  geom_mirror_histogram(bins=15) + 
  scale_y_continuous(labels = abs) + 
  theme(legend.position = "bottom") + 
  labs(caption = "Figure 2.1: Unweighted Distribution of Propensity Scores", x = "Propensity Score", y="Count", fill = "Program Placement")

## Unweighted ecdfs of continuous variables 

plot1 <- ggplot(proj_data_with_ps, aes(x = as.numeric(age), color = factor(treat))) +
  geom_ecdf() +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Age",
    y = "Proportion <= x",
    title = "Age"
  )

plot2 <- ggplot(proj_data_with_ps, aes(x = as.numeric(educ), color = factor(treat))) +
  geom_ecdf() +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Education",
    y = "Proportion <= x",
    title = "Education"
  )

plot3 <- ggplot(proj_data_with_ps, aes(x = as.numeric(re74), color = factor(treat))) +
  geom_ecdf() +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Real Earnings 1974",
    y = "Proportion <= x",
    title = "Real Earnings 1974"
  )

plot4 <- ggplot(proj_data_with_ps, aes(x = as.numeric(re75), color = factor(treat))) +
  geom_ecdf() +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Real Earnings 1975",
    y = "Proportion <= x",
    title = "Real Earnings 1975"
  )

(plot1 + theme(axis.text = element_text(size = 10), 
               axis.title = element_text(size = 12), 
               plot.title = element_text(size = 14)) |
    plot2 + theme(axis.text = element_text(size = 10), 
                  axis.title = element_text(size = 12), 
                  plot.title = element_text(size = 14))) /
  (plot3 + theme(axis.text = element_text(size = 6), 
                 axis.title = element_text(size = 12), 
                 plot.title = element_text(size = 14)) |
     plot4 + theme(axis.text = element_text(size = 6), 
                   axis.title = element_text(size = 12), 
                   plot.title = element_text(size = 14))) + 
  plot_annotation(caption = "Figure 2.2: Unweighted CDFs of Continuous Variables by Program Placement")

## Weighted propensity score model

proj_data_weight <-
  glm(treat  ~ age + black + educ + re74 + re75 + hisp,
      data = proj_data,
      family = binomial()) |> augment(type.predict = "response", data = proj_data) |>
  mutate(w_ato = wt_ato(.fitted, treat, exposure_type = "binary"))

## Weighted Distributions of Propesity Scores by group

ggplot(proj_data_weight, aes(x = .fitted, group = factor(treat), fill = factor(treat))) + 
  geom_mirror_histogram(bins = 15, aes(weight = w_ato)) +
  scale_y_continuous(labels = abs) + 
  theme(legend.position = "bottom") + 
  labs(caption = "Figure 2.3: Weighted Distribution of Propensity Scores", x = "Propensity Score", fill = "Program Placement")

## Weighted ecdfs of continuous variables 

plot1 <- ggplot(proj_data_weight, aes(x = age, color = factor(treat))) +
  geom_ecdf(aes(weights = w_ato)) +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Age",
    y = "Proportion <= x",
    title = "Age"
  )

plot2 <- ggplot(proj_data_weight, aes(x = educ, color = factor(treat))) +
  geom_ecdf(aes(weights = w_ato)) +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Education",
    y = "Proportion <= x",
    title = "Education"
  )

plot3 <- ggplot(proj_data_weight, aes(x = re74, color = factor(treat))) +
  geom_ecdf(aes(weights = w_ato)) +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Real Earnings 1974",
    y = "Proportion <= x",
    title = "Real Earnings 1974"
  )

plot4 <- ggplot(proj_data_weight, aes(x = re75, color = factor(treat))) +
  geom_ecdf(aes(weights = w_ato)) +
  scale_color_manual(
    "Program Placement",
    values = c("#5154B8", "#5DB854"),
    labels = c("Yes", "No")
  ) +
  labs(
    x = "Real Earnings 1975",
    y = "Proportion <= x",
    title = "Real Earnings 1975"
  )

(plot1 + theme(axis.text = element_text(size = 10), 
               axis.title = element_text(size = 12), 
               plot.title = element_text(size = 14)) |
    plot2 + theme(axis.text = element_text(size = 10), 
                  axis.title = element_text(size = 12), 
                  plot.title = element_text(size = 14))) /
  (plot3 + theme(axis.text = element_text(size = 6), 
                 axis.title = element_text(size = 12), 
                 plot.title = element_text(size = 14)) |
     plot4 + theme(axis.text = element_text(size = 6), 
                   axis.title = element_text(size = 12), 
                   plot.title = element_text(size = 14))) + 
  plot_annotation(caption = "Figure 2.4: Unweighted CDFs of Continuous Variables by Program Placement")

## Love plot

smds<- proj_data_weight |> tidy_smd(.vars = c("age", "educ", "re74", "re75", "black", "hisp"),.group = treat, .wts = w_ato)

ggplot(data = smds,
       aes(x = abs(smd), 
           y = variable, 
           group = method, 
           color = method)) +  
  geom_love() + labs(caption = "Figure 2.5: Love Plot comparing standard mean differences before and after weighting")

## Estimate causal effect

lm(re78 ~ treat, data = proj_data_weight, weights = w_ato) |>
  tidy() |>
  mutate(p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) |>
  kable(
    caption = "Weighted Linear Regression Results"
  )

## Confidence Interval

weighted_mod <- lm(re78 ~ treat,data = proj_data_weight,weights = w_ato)
robust_var <- sandwich(weighted_mod)[2, 2]
point_est <- coef(weighted_mod)[2]
lb <- point_est - 1.96 * sqrt(robust_var)
ub <- point_est + 1.96 * sqrt(robust_var)

kable(
  data.frame("Lower_Bound" = round(lb,2), "Upper_Bound" = round(ub,2)),
  caption = "95% Confidence Interval"
)

## Sensitivity analyses table

result<-tip_coef(1679.39, 
                 confounder_outcome_effect = seq(2,20,2))

kable(
  data.frame("confounder_outcome_effect"=result$confounder_outcome_effect,
             "exposure_confounder_effect"=result$exposure_confounder_effect),
  caption = "Required Exposure-Confounder Associations to Explain Away the Effect"
)

## Sensitivity analyses graph

adjust_df <- adjust_coef(
  effect_observed = 1679.39,
  exposure_confounder_effect = rep(seq(0, -300, by = -15), each = 7),
  confounder_outcome_effect = rep(seq(-1, -7, by = -1), times = 21),
  verbose = FALSE
)

ggplot(
  adjust_df,
  aes(
    x = exposure_confounder_effect,
    y = effect_adjusted,
    group = confounder_outcome_effect
  )
) +
  geom_hline(yintercept = 1679.39, lty = 2) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_point() +
  geom_line() +
  geom_label(
    data = adjust_df[141:147, ],
    aes(
      x = exposure_confounder_effect,
      y = effect_adjusted,
      label = confounder_outcome_effect
    )
  ) +
  labs(
    x = "Exposure - unmeasured confounder effect",
    y = "Adjusted Effect",
    caption = "Figure 3.1: Sensitivity of Effect Estimate to Unmeasured Confounding."
  )