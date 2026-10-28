# Install packages if you haven't already
# install.packages("MASS")
# install.packages("car")

# Load the required libraries
library(MASS)
library(car)

# --- ASSUMPTION CHECK ---
# Check if your variable 'le' has any non-positive values (<= 0).
# Box-Cox only works for strictly positive data (le > 0).
# If it has non-positive values, you must use Yeo-Johnson instead, or adjust the data:
# le <- le + abs(min(le)) + 1  # A way to shift the data to be positive

# Example: Create a positively skewed data vector 'le' for demonstration
set.seed(42)
le <- rchisq(100, df = 5) * 5 # Creates 100 positively skewed data points


# Fit an intercept-only linear model using 'le'
# This is required for the boxcox() function to calculate lambda
le_model <- lm(le ~ 1)

# Use the boxcox function to plot the log-likelihood values and find lambda
bc <- boxcox(le_model)

# --- Extract the optimal lambda ---
# Find the value of lambda that maximizes the log-likelihood
optimal_lambda <- bc$x[which.max(bc$y)]

# Print the optimal lambda
print(paste("The optimal Box-Cox lambda (λ) is:", round(optimal_lambda, 4)))

# Apply the Box-Cox transformation
le_transformed <- if (optimal_lambda == 0) {
  # If lambda is 0, use the natural log (ln)
  log(le)
} else {
  # If lambda is not 0, use the standard Box-Cox formula
  (le^optimal_lambda - 1) / optimal_lambda
}

# Add the transformed variable to your data frame (assuming 'le' is in a data frame named 'df')
# df$le_transformed <- le_transformed

# Display the first few transformed values
head(le_transformed)

df_plots <- data.frame(
  le_original = le,
  le_transformed = le_transformed
)

# --- Check 1: Histograms ---
par(mfrow = c(1, 2)) # Set up a 1 row, 2 column plot layout
hist(le, main = "Original 'le' Variable", xlab = "Original Values")
hist(le_transformed, main = "Transformed 'le' Variable", xlab = paste("Transformed Values (λ =", round(optimal_lambda, 4), ")"))
par(mfrow = c(1, 1)) # Reset plot layout

p_hist_original <- ggplot(df_plots, aes(x = le_original)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "lightblue", color = "black") +
  # Add the Normal Curve: use the mean and SD of the original data
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(df_plots$le_original), sd = sd(df_plots$le_original)), 
    colour = "red", 
    linewidth = 1.2
  ) +
  labs(
    title = "Original Data Distribution",
    x = "Original 'le' Values",
    y = "Density"
  ) +
  theme_minimal()

p_hist_transformed <- ggplot(df_plots, aes(x = le_transformed)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "lightgreen", color = "black") +
  # Add the Normal Curve: use the mean and SD of the transformed data
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(df_plots$le_transformed), sd = sd(df_plots$le_transformed)), 
    colour = "red", 
    linewidth = 1.2
  ) +
  labs(
    title = paste0("Transformed Data (λ = ", round(optimal_lambda, 3), ")"),
    x = "Transformed 'le' Values",
    y = "Density"
  ) +
  theme_minimal()

histograms_combined <- p_hist_original + p_hist_transformed
print(histograms_combined)

# A. Q-Q Plot for ORIGINAL 'le'
p_qq_original <- ggplot(df_plots, aes(sample = le_original)) +
  geom_qq(color = "darkblue") +
  geom_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "Q-Q Plot: Original Data",
    x = "Theoretical Normal Quantiles",
    y = "Sample Quantiles (Original 'le')"
  ) +
  theme_minimal()

# B. Q-Q Plot for TRANSFORMED 'le'
p_qq_transformed <- ggplot(df_plots, aes(sample = le_transformed)) +
  geom_qq(color = "darkgreen") +
  geom_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "Q-Q Plot: Transformed Data",
    x = "Theoretical Normal Quantiles",
    y = "Sample Quantiles (Transformed 'le')"
  ) +
  theme_minimal()

# Combine and Display the Q-Q Plots
qqplots_combined <- p_qq_original + p_qq_transformed
print(qqplots_combined)

## EDA

library(tidyverse)
library(GGally)
library(reshape2)
library(janitor)

desc_stats <- df %>%
  summarise(across(where(is.numeric),
                   list(
                     n = ~ sum(!is.na(.)),
                     mean = ~ mean(., na.rm = TRUE),
                     median = ~ median(., na.rm = TRUE),
                     sd = ~ sd(., na.rm = TRUE),
                     min = ~ min(., na.rm = TRUE),
                     q1 = ~ quantile(., 0.25, na.rm = TRUE),
                     q3 = ~ quantile(., 0.75, na.rm = TRUE),
                     max = ~ max(., na.rm = TRUE),
                     IQR = ~ IQR(., na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("variable", "statistic"),
               names_sep = "_",
               values_to = "value")

desc_stats

df %>%
  pivot_longer(where(is.numeric),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "skyblue", color = "black", alpha = 0.6) +
  geom_density(size = 1.1, color = "red") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribution of All Numeric Variables",
       x = "Value", y = "Density")

df %>%
  pivot_longer(where(is.numeric),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(variable, value, fill = variable)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of All Numeric Variables",
       x = "Variable", y = "Value")

cor_df <- cor(df |> select(where(is.numeric)),
              use = "pairwise.complete.obs")

ggplot(melt(cor_df), aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(low = "#d73027", high = "#1a9850",
                       mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap")


install.packages("DataExplorer")
library(DataExplorer)

create_report(df)

ggplot(df, aes(x = inComposition, y = le)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "Scatterplot with Regression Line")

model1 <- lm(le~inComposition * schooling, data= df)
summary(model1)









###Backward:
# Load the mtcars dataset (already in R)
data(mtcars)

# 1. Create the FULL MODEL (includes all predictors)
# We use '*' to include all variables and their interactions, 
# or use '+' to include them only as main effects. 
# For simplicity, we'll include them as main effects here:
full_model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, 
                 data = mtcars)

# View a summary of the full model
summary(full_model)

# Perform the backward stepwise selection
# The process starts with the full_model and tries to reduce it.
backward_model <- step(full_model, 
                       direction = "backward", 
                       trace = 1) # trace = 1 shows the steps and AIC changes

# View the summary of the final selected model
summary(backward_model)


# Print the final equation found by backward selection
print(formula(backward_model))

# Extract the final coefficients
coef(backward_model)



# Assuming your original data frame is 'data' and the influential rows are 183, 28, 149
library(dplyr)

# 1. Create a new data frame excluding the outliers/influential points
data_clean <- new_data %>%
  filter(!row_number() %in% c(183, 144)) # Adjust row numbers as needed!

# 2. Fit the model on the cleaned data
model_clean <- lm(le~adMortality+schooling+gdp+status+HIV+thin9+polio, data = data_clean) # Replace Y and Xs

# 3. Compare coefficients and R-squared
cat("--- Full Model Coefficients ---\n")
print(summary(bm7)$coefficients)

cat("\n--- Cleaned Model Coefficients ---\n")
print(summary(model_clean)$coefficients)

# Load the package
library(MASS)

# Fit the robust linear model (rlm)
model_robust <- rlm(le~adMortality+schooling+gdp+status+HIV+thin9+polio, data = new_data)

# Compare the robust coefficients to your OLS coefficients
cat("--- OLS Model Coefficients ---\n")
print(coef(final_model))

cat("\n--- Robust Model Coefficients (rlm) ---\n")
print(coef(model_robust))





library(ggplot2)
library(dplyr)
library(patchwork)
# The 'broom' package is required for the augment() function
library(broom)
library(scales) # For numerical formatting

#' Generates a grid of four ggplot2-style diagnostic plots for a linear model.
#'
#' @param model A fitted linear model object (lm).
#' @return A combined ggplot object displaying the four diagnostic plots.
generate_ggplot_diagnostics <- function(model) {
  
  # --- 1. Prepare Data for Plotting ---
  # Check if the model contains the original data used for fitting.
  # This fixes the "Could not coerce df to a tibble" error by ensuring 
  # the data is passed explicitly to augment().
  
  if (is.null(model$model)) {
    # If the model object didn't save the data (e.g., if data= argument was not used),
    # this will try to extract it from the environment, which is often the cause of the error.
    # To reliably fix it, users should always fit the model using the data= argument.
    stop("Model was likely fitted without using 'data=' argument. Please refit your model using 'lm(..., data=my_dataframe)' for diagnostics to work.")
  }
  
  # Calculate necessary model metrics and residuals
  model_data <- broom::augment(model, data = model$model) %>%
    # Rename default variables for clarity and simplicity
    rename(
      Fitted = .fitted,
      Residuals = .resid,
      StdResiduals = .std.resid,
      Leverage = .hat,
      CooksD = .cooksd
    )
  
  # --- 2. Residuals vs. Fitted Plot (Linearity & Homoscedasticity) ---
  # Standardized residuals vs Fitted values
  p_resid_vs_fitted <- ggplot(model_data, aes(x = Fitted, y = StdResiduals)) +
    geom_point(color = "dodgerblue4", alpha = 0.7) +
    # Add a horizontal line at 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    # Add a smoothed conditional mean (should be flat)
    geom_smooth(method = "loess", se = FALSE, color = "darkorange", linewidth = 1.2) +
    labs(
      title = "Residuals vs. Fitted",
      subtitle = "Checks linearity and equal variance"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  
  # --- 3. Normal Q-Q Plot (Normality) ---
  # Uses geom_qq and geom_qq_line for a clean Q-Q plot
  p_qq <- ggplot(model_data, aes(sample = StdResiduals)) +
    geom_qq(color = "darkgreen", alpha = 0.7) +
    geom_qq_line(color = "red", linetype = "dashed", linewidth = 1.1) +
    labs(
      title = "Normal Q-Q Plot",
      subtitle = "Checks for normally distributed residuals"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  
  # --- 4. Scale-Location Plot (Homoscedasticity) ---
  # Square root of absolute standardized residuals vs Fitted values
  p_scale_location <- ggplot(model_data, aes(x = Fitted, y = sqrt(abs(StdResiduals)))) +
    geom_point(color = "purple4", alpha = 0.7) +
    # Add a smoothed conditional mean (should be flat)
    geom_smooth(method = "loess", se = FALSE, color = "darkorange", linewidth = 1.2) +
    labs(
      title = "Scale-Location Plot",
      subtitle = "Secondary check for equal variance (Homoscedasticity)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  
  # --- 5. Residuals vs. Leverage (Influence) ---
  # Calculates the 0.5 and 1.0 Cook's distance contours
  cooksd_threshold <- 1.0
  
  p_leverage <- ggplot(model_data, aes(x = Leverage, y = StdResiduals)) +
    geom_point(aes(color = CooksD), alpha = 0.7) +
    scale_color_gradient(low = "gray50", high = "red", name = "Cook's D") +
    geom_smooth(method = "loess", se = FALSE, color = "darkorange", linewidth = 1.2) +
    
    # Add Cook's Distance contours (often D=0.5 or D=1.0)
    # Note: Adding the actual contour lines is complex in ggplot2, 
    # so we rely on the color aesthetic and the Cook's D value for diagnosis.
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    
    labs(
      title = "Residuals vs. Leverage",
      subtitle = "Identifies influential points (Cook's Distance)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  
  # --- 6. Combine Plots using patchwork ---
  # Arrange plots in a 2x2 grid
  combined_plots <- (p_resid_vs_fitted + p_qq) / (p_scale_location + p_leverage) +
    plot_annotation(
      title = "Final Model Diagnostic Plots (ggplot2 Style)",
      theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    )
  
  return(combined_plots)
}

# --- EXAMPLE USAGE ---
# This part demonstrates how to call the function.
# NOTE: You must have the 'broom' package installed for augment() to work well.
# install.packages("broom")
# install.packages("patchwork")

# Create a sample model (using mtcars, adjust to your final model)
# The use of data=mtcars ensures the data is saved in the model object.
sample_model <- lm(mpg ~ disp + hp + wt + qsec + am, data = mtcars)

# Generate and print the plots
generate_ggplot_diagnostics(sample_model)
generate_ggplot_diagnostics(final_model)






# Load necessary libraries
library(dplyr)
library(ggplot2)
# Load the psych package for advanced correlation matrix visualization
# You might need to install this: install.packages("psych")
library(psych) 

# ----------------------------------------------------------------------
# 1. Mock Data Setup (REPLACE THIS WITH YOUR ACTUAL DATA)
# ----------------------------------------------------------------------

# NOTE: Since the code cannot access your data, please ensure your data 
# frame 'data' is loaded correctly before running this script.

set.seed(42)
num_obs <- 183
data <- data.frame(
  # Outcome
  le = rnorm(num_obs, mean = 71, sd = 8),
  # Socioeconomic Predictors (using transformed GDP as assumed in your EDA)
  schooling = rnorm(num_obs, mean = 13, sd = 3),
  gdp = rlnorm(num_obs, meanlog = 7, sdlog = 1.5),
  inComposition = runif(num_obs, 0.3, 0.95)
)

# Apply the necessary log transformation to GDP
data$log_gdp <- log(data$gdp)

# Create a clean data frame containing only the variables needed for the plot
# We include LE first to make it the reference point for correlation plots
corplot_data <- new_data %>%
  dplyr::select(le, schooling, gdp, inComposition) %>%
  rename(
    Life_Expectancy = le,
    Schooling = schooling,
    log_GDP = gdp,
    Income_Composition_Index = inComposition
  )

# ----------------------------------------------------------------------
# 2. Correlation Matrix with Scatter Plots
# ----------------------------------------------------------------------

# The lower panel will show scatter plots (visualizing linearity).
# The upper panel will show the absolute correlation coefficients (numeric values).
# The diagonal will show histograms/density plots (visualizing univariate distribution).

# Save the plot as a PDF for best quality in LaTeX
pdf("Library/CloudStorage/OneDrive-UniversityofSouthFlorida/LE Projet/socioeconomic_pairs_plot.pdf", width = 8, height = 8)

# The 'pairs.panels' function from the 'psych' package creates the desired matrix.
pairs.panels(corplot_data,
             method = "pearson", # correlation method
             hist.col = "#00AFBB", # histogram color
             density = TRUE,      # show density curve in histograms
             ellipses = FALSE,    # remove concentration ellipses
             main = "Bivariate Correlation Matrix: Life Expectancy and Socioeconomic Predictors",
             lm = TRUE            # show linear regression lines in scatter plots
)

dev.off()

cat("Correlation Pairs Plot saved to: graphs/socioeconomic_pairs_plot.pdf\n")

# ----------------------------------------------------------------------
# 3. Generating a Correlation Heatmap (Separate, optional visual)
# ----------------------------------------------------------------------

# If you only want the correlation coefficients in a simple heatmap format 
# (requires the 'corrplot' package, but the psych output is usually sufficient)
correlation_matrix <- cor(corplot_data)
corrplot::corrplot(correlation_matrix, method = "color", type = "upper", 
                   addCoef.col = "black", tl.col = "black", tl.srt = 45)





marks = c(71, 87, 101, 51, 74, 71, 82, 84, 93, 56, 92, 92, 95, 65, 91, 89, 79, 101,
          71, 101, 51, 96, 80, 41, 90, 99, 99, 92, 96, 87, 82, 80, 96, 60, 83, 94,
          90, 98, 107, 61, 99, 93, 96, 95, 92, 89, 85, 93, 76, 71, 82, 98, 94)

data66 <- data.frame(marks = marks)
attach(data66)

m1 <- data66 %>% ggplot(aes(x=marks))+
  geom_histogram(aes(y=after_stat(density)),
                 bins=15,
                 color="black",
                 fill="steelblue",
                 alpha=0.6)+
  stat_function(fun = dnorm,
                args = list(mean= mean(marks),
                            sd= sd(marks)),
                color="darkred",
                lwd=1.5) +
  labs(title=paste("Distribution of Marks of the Exam3"),
       x= "Marks",
       y= "Density")+
  theme_minimal()+ 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30")
  )

m2 <- data66 %>% ggplot(aes(x=marks))+
  geom_boxplot()+
  labs(title=paste("Distribution of Marks of the Exam3"),
       x = "Marks") +
  theme_minimal() + 
  theme( 
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30")
  ) 

m1+m2



(pschooling3 + pinComposition3)/(pgdp5+ppopulation4)
(padM6+pHIV4) / (pthin94+pthin194)
(ppolio3+phepB4+pdiphtheria4)
