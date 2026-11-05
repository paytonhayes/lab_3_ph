#load packages
install.packages(c(
  "carData", "dplyr", "tidyr", "ggplot2",
  "broom", "performance", "gt"
))

library(carData)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)
library(gt)

# 1.3: save attitude data frame as 'dat' and look at only rating, complaints, learning
#and raise, save number of rows as 'n'
dat <- attitude |>
  select(rating, complaints, learning, raises)

n <- nrow(dat)

# 1.4 explore data: head(), str(), summary(), hist() in console

# 1.4: Describe the data


# **Part 1:**
#Descriptive Statistics for Rating, Complaints, Learning, and Raises


#A formatted table reporting the mean, standard deviation, minimum, and maximum for each analysis variable

desc_stats <- dat |>
  summarise(
    across(
      everything(),
      list(
        Mean = mean,
        SD = sd,
        Min = min,
        Max = max
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "_"
  )
desc_stats |>
  gt()|>
  fmt_number(
    columns = c(Mean, SD, Min, Max),
    decimals = 2
  )


# **A Histogram of Rating, with an Overlaid Density Curve**


ggplot(dat, aes(x = rating)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 15,
    fill = "#669BBC",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(
    linewidth = 0.9,
    color = "#9A3038"
  ) +
  labs(
    x = "Occupational workplace rating score",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)



#At least two scatterplots showing relationships between rating and predictors, with fitted regression lines.


# **Relationship Between Workplace Rating and Complaints**


ggplot(dat, aes(x = complaints, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A"
  ) +
  labs(
    x = "Handling of employee complaints",
    y = "Overall workplace rating"
  ) +
  theme_minimal(base_size = 12)



# **Relationship Between Workplace Rating and Raises**


ggplot(dat, aes(x = raises, y = rating)) +
  geom_point(alpha = 0.75) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#27638A"
  ) +
  labs(
    x = "Pay increases",
    y = "Overall workplace rating"
  ) +
  theme_minimal(base_size = 12)


#A short paragraph describing the most important patterns you observe.


#The average overall workplace rating across departments was 64.63. According to the scatter-plots, there is a positive relationship between overall workplace rating and both predictive factors of raises and handling of complaints. However, workplace ratings scores and handling of complaints demonstrates a stronger correlation. There is moderate scatter around the regression line for workplace ratings and raises, suggesting a weaker correlation. I would have expected the opposite to be true, because I think a lot of people feel like they "don't get paid enough for this".

# **Part 2:** 

# **Developing Predictions**

#Handling of workplace complaints may be associated with workplace ratings because if people are more likley to be heard about theri concerns, they may feel more satisfied with the quality of managment and the workplace overall.

#I think that opportunities for learning could potentially explain additional variation beyond complaint handling because if you feel like you can 'move up' in the company and advance your skills with them, you might have a better outlook on the workplace as a whole.

#Still I think pay increases could explain further variation after accounting for complaints and learning because if employees feel like they aren't getting compensated enough for their time and worth, they may become unsatistifed in their career.

# 2.2 Specify four nested models

# Intercept-only model
mod0 <- lm(rating ~ 1, data = dat)

# Complaints model
mod1 <- lm(rating ~ complaints, data = dat)

# Complaints + Learning model
mod2 <- lm(rating ~ complaints + learning, data = dat)

# Expanded mode: Complaints, learning, raises
mod3 <- lm(rating ~ complaints + learning + raises, data = dat)


# **Interpretation of the complaints coefficient changes between M1, M2, and M3**

#Complaints coefficient M1: 0.75461
#Complaints coefficient M2: 0.6435 
#Complaints coefficient M3: 0.65349

#The complaints coefficient for M1 through M3 suggest that complaint handling remains a positive, and fairly strong predictor of workplace rating, even when controlling for predictors like learning and raises. All three complaint coefficients were statistically significant, even though there was a dip between m1 and m2 

# **Part 3**
# **Conduct Nested-Model F-tests**

#Use anova() to compare all four models in a single call, in the order M0, M1, M2, M3

ftests <- anova(mod0, mod1, mod2, mod3)

ftests.final <- ftests |>
 select(Res.Df, Df, F, `Pr(>F)`) |>
  rename(
    Residual.df = Res.Df,
    Nested.Df = Df,
    Nested.F.stat = F,
    Nested.p.value = `Pr(>F)`
  )

# **3.2. Extract model-level performance statistics**
# Use glance() from the broom package to extract model-level statistics for each of the four fitted models.

glances <- bind_rows(
  "M0: Null" = glance(mod0),
  "M1: Complaints" = glance(mod1),
  "M2: Complaints + Learning" = glance(mod2),
  "M3: Expanded" = glance(mod3),
  .id = "Model"
)

comparison_table <- cbind(
  glances,
  ftests.final
)

#3.3. Calculate changes in explained variance
#Calculate the change in R² between successive models

comparison_table <- comparison_table |>
  mutate(
    Delta.R2 = r.squared - lag(r.squared),
    
    RMSE = c(
      sqrt(mean(residuals(mod0)^2)),
      sqrt(mean(residuals(mod1)^2)),
      sqrt(mean(residuals(mod2)^2)),
      sqrt(mean(residuals(mod3)^2))
    )
  ) 
comparison_table <- comparison_table |>
  select(
    Model,
    r.squared,
    adj.r.squared,
    RMSE,
    Delta.R2,
    Residual.df,
    Nested.Df,
    Nested.F.stat,
    Nested.p.value
  )

#3.4 Combine the model-performance statistics with the nested-model test results

comparison_table |>
  gt() |>
  cols_label(
    Model = "Model",
    r.squared = "R²",
    adj.r.squared = "Adjusted R²",
    RMSE = "RMSE",
    Delta.R2 = "ΔR²",
    Residual.df = "Residual df",
    Nested.Df = "Nested df",
    Nested.F.stat = "Nested F",
    Nested.p.value = "Nested p"
  )|>
tab_style(
  style = cell_fill(color = "#E6F2FF"),
  locations = cells_column_labels(
    columns = c(r.squared, adj.r.squared, RMSE)
  ))|>
  fmt_number(
    columns = c(
      r.squared,
      adj.r.squared,
      RMSE,
      Delta.R2,
      Nested.F.stat
    ),
    decimals = 3
  ) |>
  fmt(
    columns = Nested.p.value,
    fns = function(x) {
      ifelse(
        is.na(x),
        "—",
        ifelse(x < .001, "< .001", sprintf("%.3f", x))
      )
    }
  ) |>
  sub_missing(
    columns = c(Delta.R2, Nested.Df, Nested.F.stat),
    missing_text = "—"
  )
#compare model performance
performance_comparison <- compare_performance(
  mod0,
  mod1,
  mod2,
  mod3,
  rank = TRUE
)

##**Part 4
#4.1. Examine the assumptions

#Select the most promising model based on your comparisons in Part 3.
#Use R's standard regression diagnostic plots and/or performance::check_model() to examine its assumptions.
#You may also use check_collinearity() to investigate multicollinearity.

check_model(mod2)
check_collinearity(mod2)

diagnostics <- check_model(mod2)


#4.2. Report your diagnostic findings
#Include at least one set of diagnostic plots in your Quarto report.

op <- par(mfrow = c(2, 2))

plot(mod1)

par(op)


op <- par(mfrow = c(2, 2))

plot(mod1)

par(op)

#5.2. Create a coefficient table
#Use tidy() from broom to extract the regression coefficients from your selected model.

coef_table <- tidy(
  mod2,
  conf.int = TRUE,
  conf.level = 0.95
)

coef_table |>
  select(
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  ) |>
  gt() |>
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "SE",
    conf.low = "95% CI lower",
    conf.high = "95% CI upper",
    p.value = "p"
  ) |>
  fmt_number(
    columns = c(
      estimate,
      std.error,
      conf.low,
      conf.high
    ),
    decimals = 3
  ) |>
  fmt(
    columns = p.value,
    fns = function(x) {
      ifelse(
        x < .001,
        "< .001",
        sprintf("%.3f", x)
      )
    }
  )

