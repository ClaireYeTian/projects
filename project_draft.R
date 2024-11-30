# 0. setting ####
library(rjson)
library(rstan)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
options(mc.cores = parallel::detectCores())

# 1. data preparation
# View the contents of the .data file
file_path <- "parkinsons_updrs.data"
lines <- readLines(file_path, n = 10)  # Read the first 10 lines
print(lines)

p_data <- read.csv(file_path, header = TRUE)  # Set header to TRUE if there's a header row
colnames(p_data)

# 1.1 explore the data ####
summary(p_data)
correlation_y12 <- cor(p_data$motor_UPDRS, p_data$total_UPDRS, method = "pearson")
correlation_y12

# y1: motor_UPDRS
ggplot(data=p_data)+
  geom_density(aes(x=motor_UPDRS,color=as.factor(subject.)))+
  geom_density(aes(x=motor_UPDRS))

# an outlier 18
p_data[which(p_data$subject.==18),] %>% ggplot()+
  geom_density(aes(x=motor_UPDRS))

# y2: total_UPDRS
ggplot(data=p_data)+
  geom_density(aes(x=total_UPDRS,color=as.factor(subject.)))+
  geom_density(aes(x=total_UPDRS))


# Calculate mean and sd for y1 and y2 for each subject
summary_suject <- p_data %>%
  group_by(subject.) %>%
  summarize(
    mean_y1 = mean(motor_UPDRS, na.rm = TRUE),
    sd_y1 = sd(motor_UPDRS, na.rm = TRUE),
    mean_y2 = mean(total_UPDRS, na.rm = TRUE),
    sd_y2 = sd(total_UPDRS, na.rm = TRUE)
  )
summary_suject
plot(x=summary_suject$subject.,y=summary_suject$mean_y1)
plot(x=summary_suject$subject.,y=summary_suject$sd_y1)

# to detect the time trend ####
p_data[,c("subject.","test_time","motor_UPDRS")] %>% 
  ggplot(aes(x=test_time,y=motor_UPDRS,color=as.factor(subject.)))+geom_line()

# 2.simple model ####

# 2.0 data setting ####
x<-p_data[,7:22]
N<-dim(p_data)[1]
J<-dim(x)[2]
y<-p_data[,c("motor_UPDRS")]

simple_data<-list(x=x,N=N,y=y,J=J)

# 2.1 determine the convergent model ####

set.seed(6668)
fit_simple_2000<- stan(file = 'p_simple_model.stan',data=simple_data,chains=4, iter=2000)
fit_simple_2000
save(fit_simple_2000, file = "fit_simple_2000.RData") 

# 2.2 check predict performance: motor_UPDRS  y1 ####

# 1) construct the lm regression model as comparison 
test_data_df<-data.frame(x=p_data[,7:22],y=p_data[,c("motor_UPDRS")])
colnames(test_data_df)
fit_lm_y1<-lm(y~.,data = test_data_df)
summary(fit_lm_y1)

predicted_values_lm_y1 <- predict(fit_lm_y1, newdata = test_data_df)

# 2) draw density plot 
# Create a data frame to combine predict value and actual value

fit_simple_2000_result<-rstan::extract(fit_simple_2000)

length(fit_simple_2000_result$y_pred) # supposed to be 5875*4*1000=23500000
predict_data_simple <- bind_rows(
  data.frame(value = as.vector(fit_simple_2000_result$y_pred), variable = "Bayesian regression model"),
  data.frame(value = p_data$motor_UPDRS, variable = "True value"),
  data.frame(value = predicted_values_lm_y1, variable = "Linear regression(OLS)")
)
 
length(predicted_values_lm_y1)
# Draw the density plot

#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
# scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
# scale_colour_manual(values=cbPalette)

density_plot_comparison_simple <- ggplot(predict_data_simple, aes(x = value, color = variable)) +
  geom_density(size = 0.5) +  # Set line thickness to 0.5
  labs(x = "Value of y", y = "Density") +
  scale_colour_manual(values = cbPalette) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    panel.background = element_blank(),  # Remove gray background
    panel.grid.minor = element_blank(),  # Optional: Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black border
    axis.text = element_text(size = 9),  # Set axis text size to 9 points
    axis.title = element_text(size = 9),  # Set axis title size to 9 points
    legend.position = "top",  # Position the legend at the top
    legend.title = element_blank(),  # Remove the legend title
    legend.text = element_text(size = 9)  # Set legend text size to 9 points
  )
# bimodal distribution
density_plot_comparison_simple
ggsave("density_plot_comparsion_simple.png", plot = density_plot_comparison_simple, width = 6, height = 4, dpi = 300)

# 2.3 estimate the parameter ####
summary_fit_simple <- summary(fit_simple_2000)$summary

# Extract mean and MCSE for alpha
alpha_mean_simple <- summary_fit_simple[grep("^alpha", rownames(summary_fit_simple)), "mean"] 
alpha_mcse_simple <- summary_fit_simple[grep("^alpha", rownames(summary_fit_simple)), "se_mean"] 
alpha_rhat_simple <- summary_fit_simple[grep("^alpha", rownames(summary_fit_simple)), "Rhat"] 


# Extract mean and MCSE for beta
beta_mean_simple <- summary_fit_simple[grep("^beta", rownames(summary_fit_simple)), "mean"] 
beta_mcse_simple <- summary_fit_simple[grep("^beta", rownames(summary_fit_simple)), "se_mean"]
beta_rhat_simple <- summary_fit_simple[grep("^beta", rownames(summary_fit_simple)), "Rhat"] 


# Function to round means based on MCSE
round_means <- function(means, mcse) {
  sapply(1:length(means), function(i) {
    if (mcse[i] == 0) {
      round(means[i], digits = 4)  # Round to 4 decimal places if MCSE is 0
    } else {
      round(means[i], digits = 2)  # Round to 2 decimal places otherwise
    }
  })
}

# Applying the function to alpha and beta means
rounded_alpha_means_simple <- round_means(alpha_mean_simple, alpha_mcse_simple)
rounded_beta_means_simple <- round_means(beta_mean_simple, beta_mcse_simple)

# compress into a table into overleaf ####

# Create parameter names for alpha
alpha_names_simple <- paste0("alpha[", 1:length(alpha_mean_simple), "]")

# Create parameter names for beta
# Assuming K=2 responses and J=16 predictors
beta_names_simple <- paste0("beta[", 1:length(beta_mean_simple), "]")

# Combine the results into a data frame
parameter_df_M1 <- data.frame(
  Parameter = c(alpha_names_simple, beta_names_simple),
  # Mean = c(alpha_mean_simple, beta_mean_simple),
  Adjusted_Estimate = c(rounded_alpha_means_simple, rounded_beta_means_simple),
  MCSE = c(alpha_mcse_simple, beta_mcse_simple),
  Rhat=c(alpha_rhat_simple,beta_rhat_simple),
  stringsAsFactors = FALSE
)

# Round numeric columns of parameter_df_M1 to 4 decimal places
parameter_df_M1_digit4 <- as.data.frame(lapply(parameter_df_M1, function(x) if(is.numeric(x)) round(x, digits = 3) else x))

print(parameter_df_M1_digit4)
dim(parameter_df_M1_digit4)
dim(parameter_df_M1)

# Generate the LaTeX table code
cat("\\begin{table}[t]\n",
    "\\caption{Parameter estimates of Bayes linear regression model.}\n",
    "\\label{parameter-table}\n",
    "\\vskip 0.15in\n",
    "\\begin{center}\n",
    "\\begin{small}\n",
    "\\begin{sc}\n",
    "\\begin{tabular}{lccc}\n",
    "\\toprule\n",
    "Parameter & Estimate & MCSE & Rhat\\\\\n",
    "\\midrule\n", sep = "")

# Print each row of the table
apply(parameter_df_M1_digit4, 1, function(row) {
  cat(row[1], " & ", row[2], " & ", row[3], " & ", row[4], " \\\\\n", sep = "")
})

# Close LaTeX table
cat("\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{sc}\n",
    "\\end{small}\n",
    "\\end{center}\n",
    "\\vskip -0.1in\n",
    "\\end{table}\n", sep = "")




# 3.hierarchy model ####

# 3.0 data setting ####
N<-dim(p_data)[1]
J<-dim(x)[2]
G<-max(p_data$subject.)
group<-p_data$subject.
x<-p_data[,7:22]
y<-p_data[,c("motor_UPDRS")]

hierarchy_data<-list(N=N,J=J,G=G,group=group,x=x,y=y)

# 3.1 determine the convergent model ####

set.seed(6618)
fit_hierarchy_2000<- stan(file = 'p_hierarchy_model.stan',data=hierarchy_data,chains=4, iter=2000)
fit_hierarchy_2000
save(fit_hierarchy_2000, file = "fit_hierarchy_2000.RData") 

# 3.2 check predict performance: motor_UPDRS  y1 ####

fit_hierarchy_2000_result<-rstan::extract(fit_hierarchy_2000)

total_length<-length(fit_hierarchy_2000_result$y_pred) # supposed to be 5875*4*1000=23500000

fit_simple_data_sampling <- sample(as.numeric(fit_simple_2000_result$y_pred), size=0.5*total_length)
fit_hierarchy_data_sampling <- sample(as.numeric(fit_hierarchy_2000_result$y_pred), size=0.5*total_length)

predict_data_hierarchy <- bind_rows(
  data.frame(value = fit_simple_data_sampling, variable = "Bayesian regression model"),
  data.frame(value = fit_hierarchy_data_sampling, variable = "Hierarchical Bayesian model"),
  data.frame(value = p_data$motor_UPDRS, variable = "True_value")
  
)

#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

density_plot_comparsion_hierarchy <- ggplot(predict_data_hierarchy, aes(x = value, color = variable)) +
  geom_density(size = 0.5) +  # Set line thickness to 0.5
  labs(x = "Value of y", y = "Density") +
  scale_colour_manual(values = cbPalette) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    panel.background = element_blank(),  # Remove gray background
    panel.grid.minor = element_blank(),  # Optional: Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black border
    axis.text = element_text(size = 9),  # Set axis text size to 9 points
    axis.title = element_text(size = 9),  # Set axis title size to 9 points
    legend.position = "top",  # Position the legend at the top
    legend.title = element_blank(),  # Remove the legend title
    legend.text = element_text(size = 9)  # Set legend text size to 9 points
  )
density_plot_comparsion_hierarchy
ggsave("density_plot_comparsion_hierarchy.png", plot = density_plot_comparsion_hierarchy, width = 6, height = 4, dpi = 300)


# 4.0 shrinkage prior ####

# 4.0 data setting ####
x<-p_data[,7:22]
y<-p_data[,c("motor_UPDRS")]
N<-dim(p_data)[1]
J<-dim(x)[2]
p_0<-6
scale_icept<-32.09
scale_global<-p_0/((J-p_0)*sqrt(N))
nu_local<-1
nu_global<-1

simple_shrinkage_data<-list(N=N,J=J,x=x,y=y,scale_icept=scale_icept,scale_global=scale_global,nu_local=nu_local,nu_global=nu_global)

# 4.1 determine the convergent model ####

fit_simple_shrinkage<- stan(file = 'p_simple_shrinkage.stan',data=simple_shrinkage_data,chains=4, iter=5000,control = list(adapt_delta = 0.999))
fit_simple_shrinkage
save(fit_simple_shrinkage, file = "fit_simple_shrinkage.RData") 

fit_simple_shrinkage_7000<- stan(file = 'p_simple_shrinkage.stan',data=simple_shrinkage_data,chains=4, iter=7000,control = list(adapt_delta = 0.999))
fit_simple_shrinkage_7000
save(fit_simple_shrinkage_7000, file = "fit_simple_shrinkage_7000.RData") 

fit_simple_shrinkage_7100<- stan(file = 'p_simple_shrinkage.stan',data=simple_shrinkage_data,chains=4, iter=7100,control = list(adapt_delta = 0.999))
fit_simple_shrinkage_7100
save(fit_simple_shrinkage_7100, file = "fit_simple_shrinkage_7100.RData") 
# 
# summary_stats_simple_2000 <- summary(fit_simple_2000)
# beta_means_simple_2000 <- summary_stats_simple_2000$summary[grep("beta", rownames(summary_stats_simple_2000$summary)), "mean"]
# 
# summary_stats_simple_shrinkage <- summary(fit_simple_shrinkage)
# beta_means_simple_shrinkage <- summary_stats_simple_shrinkage$summary[grep("beta", rownames(summary_stats_simple_shrinkage$summary)), "mean"]
# format(beta_means_simple_shrinkage, scientific = FALSE, digits = 3)
# 
# which(colnames(x) %in% c("Jitter.Abs.","Shimmer" ,"NHR","HNR","DFA","PPE"  ))

# 4.2 use a different code ####
simple_shrinkage_data_2<-list(N=N,J=J,x=as.matrix(x),
                              y=y,scale_icept=scale_icept,scale_global=scale_global,nu_local=nu_local,nu_global=nu_global)

fit_simple_shrinkage_2<- stan(file = 'p_simple_shrinkage_2.stan',data=simple_shrinkage_data,chains=4, iter=5000,control = list(adapt_delta = 0.999))
fit_simple_shrinkage
save(fit_simple_shrinkage, file = "fit_simple_shrinkage.RData") 

# 4.3 use package to do the shrinkage ####
library(devtools)
library(usethis)
library(bayesreg)
library(iterators)
library(pgdraw)
library(doParallel)
library(foreach)
head(s_data)
s_data<-data.frame(x=simple_data$x,y=simple_data$y)

rv.hs <- bayesreg(y~., s_data, prior="hs", n.cores=4)
rv.hs.s <- summary(rv.hs)
y_hat<-predict(rv.hs, s_data, bayes.avg=TRUE)[,1]

predict_data_simple_shrinkage <- bind_rows(
  data.frame(value = as.vector(fit_simple_2000_result$y_pred), variable = "Bayesian regression model"),
  data.frame(value = p_data$motor_UPDRS, variable = "True value"),
  data.frame(value = y_hat,variable="Bayesian regression model with shrinkage prior")
)

head(predict_data_simple_shrinkage)
# Draw the density plot

density_plot_comparsion_simple_s <- ggplot(predict_data_simple_shrinkage, aes(x = value, color = variable)) +
  geom_density(size = 0.5) +  # Set line thickness to 0.5
  labs(x = "Value of y", y = "Density") +
  scale_colour_manual(values = cbPalette) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with larger base font size
  theme(
    panel.background = element_blank(),  # Remove gray background
    panel.grid.minor = element_blank(),  # Optional: Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black border
    axis.text = element_text(size = 9),  # Set axis text size to 9 points
    axis.title = element_text(size = 9),  # Set axis title size to 9 points
    legend.position = "top",  # Position the legend at the top
    legend.title = element_blank(),  # Remove the legend title
    legend.text = element_text(size = 9)  # Set legend text size to 9 points
  )
density_plot_comparsion_simple_s
ggsave("density_plot_comparsion_simple_s.png", plot = density_plot_comparsion_simple_s, width = 6, height = 4, dpi = 300)


# expressed the table
# Assuming rv.hs.s is a summary object with columns "Estimate" and "MCSE"
# Extract coefficients table
coeff_table <- rv.hs.s$mu.coef 

coeff_table <- data.frame(
  Parameter = c(sub("^x\\.", "", rownames(coeff_table))),
  Mean = rv.hs.s$mu.coef ,
  SE = rv.hs.s$se.coef,
  stringsAsFactors = FALSE
)

# Round numeric columns of parameter_df_M1 to 4 decimal places
coeff_table_digit4 <- as.data.frame(lapply(coeff_table, function(x) if(is.numeric(x)) round(x, digits = 3) else x))



# Format LaTeX table code
cat("\\begin{table}[t]\n",
    "\\caption{Parameter estimates of Bayes linear regression model.}\n",
    "\\label{Parameter estimates of Bayes linear regression model}\n",
    "\\vskip 0.15in\n",
    "\\begin{center}\n",
    "\\begin{small}\n",
    "\\begin{sc}\n",
    "\\begin{tabular}{lccc}\n",
    "\\toprule\n",
    "Parameter & Estimate & SE\\\\\n",
    "\\midrule\n")

# Loop through each coefficient and print as LaTeX rows
for (i in 1:nrow(coeff_table)) {
  Parameter <- coeff_table[i, "Parameter"]
  Estimate <- coeff_table[i, "Mean"]
  SE <- coeff_table[i, "SE"]
  cat(sprintf("%s & %.4f   \\\\\n", Parameter, Estimate,SE))
}

apply(coeff_table_digit4, 1, function(row) {
  cat(row[1], " & ", row[2], " & ", row[3], " \\\\\n", sep = "")
})

cat("\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{sc}\n",
    "\\end{small}\n",
    "\\end{center}\n",
    "\\vskip -0.1in\n",
    "\\end{table}\n")



# 5. model comparison ####

# 5.1 use the loocv ####
library(loo)

# 1) simple model ####
simple_loo<-loo(fit_simple_2000)
# All Pareto k estimates are good (k < 0.7).
plot(simple_loo)

# 2) hierarchy model ####
hierarchy_loo<-loo(fit_hierarchy_2000)
# All Pareto k estimates are good (k < 0.7).
plot(hierarchy_loo)

# 3) compare ####
loo_compare(simple_loo, hierarchy_loo)

# 5.2 choose typical case ####
# choose the one with the biggest variance
typical_case <- summary_suject %>% filter(sd_y1>4)
print(typical_case$subject.)
# 17: 4.29
# 21: 4.05

# choose the one with the smallest variance
summary_suject %>% filter(sd_y1<1)  %>% select(subject.)%>% print()
 # 16: 0.451
summary_suject %>% filter(sd_y1 <3 & sd_y1 >1)  %>% select(subject.)%>% print()
# 4: 2.54

# big variance: subject 17 ####
ggplot(data = p_data[p_data$subject.==17,],aes(x=test_time,y=motor_UPDRS))+
  geom_line()
row_number_17<-which(p_data$subject.==17)
timeline_17<-p_data[p_data$subject.==17,"test_time"]

# true value , ols value
data_17<-cbind(timeline_17,predicted_values_lm_y1[row_number_17],p_data$motor_UPDRS[row_number_17])%>%
  as.data.frame()
colnames(data_17)<-c("Test_time","Linear_regression(OLS)","value_of_y")
head(data_17)

# bayesian linear regression
data_17_simple<-fit_simple_2000_result$y_pred[,row_number_17] %>% as.data.frame()
data_17$Bayesian_regression_model<-colMeans(data_17_simple) %>% as.vector()

# bayesian hierarchy model
data_17_hierarchy<-fit_hierarchy_2000_result$y_pred[,row_number_17] %>% as.data.frame()

data_17$Hierarchical_Bayesian_model<-colMeans(data_17_hierarchy) %>% as.vector()

head(data_17)

# Define a color palette
    cbPalette <- c("true_value" = "#999999", 
                   "Linear_regression(OLS)" = "#E69F00", 
                   "Bayesian_linear_regression" = "#56B4E9", 
                   "Bayesian_hierarchy_model" = "#009E73")
  
time_trend_plot_17<-  ggplot() + 
  # Add true value line with a specific color
  geom_line(data = data_17, aes(x = Test_time, y = value_of_y, color = "true_value"), size = 1) +  # Set color directly for true value
  
  # Add linear regression points with a specific color
  geom_point(data = data_17, aes(x = Test_time, y = `Linear_regression(OLS)`, color = "Linear_regression(OLS)")) + 
  
  # Add Bayesian linear regression points with a specific color
  geom_point(data = data_17, aes(x = Test_time, y = `Bayesian_regression_model`, color = "Bayesian_linear_regression")) + 
  
  # Add Bayesian hierarchy model points with a specific color
  geom_point(data = data_17, aes(x = Test_time, y = `Hierarchical_Bayesian_model`, color = "Bayesian_hierarchy_model")) + 
  
  # Define the color scale
  scale_colour_manual(values = cbPalette) + 
  theme_minimal(base_size = 15) + 
  theme(
    panel.background = element_blank(), 
    panel.grid.major = element_line(color = "gray80"), 
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    axis.text = element_text(size = 9), 
    axis.title = element_text(size = 9), 
    legend.position = "none" 
  )
time_trend_plot_17 
ggsave("time_trend_plot_17.png", plot = time_trend_plot_17, width = 6, height = 4, dpi = 300)

# ignore: big variance: subject 21 ####
ggplot(data = p_data[p_data$subject.==21,],aes(x=test_time,y=motor_UPDRS))+
  geom_line()

row_number_21<-which(p_data$subject.==21)
timeline_21<-p_data[p_data$subject.==21,"test_time"]

# true value , ols value
data_21<-cbind(timeline_21,predicted_values_lm_y1[row_number_21],p_data$motor_UPDRS[row_number_21])%>%
  as.data.frame()
colnames(data_21)<-c("time","Linear_regression(OLS)","true_value")
head(data_21)

# bayesian linear regression
data_21_simple<-fit_simple_2000_result$y_pred[,row_number_21] %>% as.data.frame()
data_21$Bayesian_linear_regression<-colMeans(data_21_simple) %>% as.vector()

# bayesian hierarchy model
data_21_hierarchy<-fit_hierarchy_2000_result$y_pred[,row_number_21] %>% as.data.frame()

data_21$Bayesian_hierarchy_model<-colMeans(data_21_hierarchy) %>% as.vector()



# Define a color palette
cbPalette <- c("true_value" = "#999999", 
               "Linear_regression(OLS)" = "#E69F00", 
               "Bayesian_linear_regression" = "#56B4E9", 
               "Bayesian_hierarchy_model" = "#009E73")

ggplot() +
  # Add true value line with a specific color
  geom_line(data = data_21, aes(x = time, y = true_value, color = "true_value")) +
  
  # Add linear regression points with a specific color
  geom_point(data = data_21, aes(x = time, y = `Linear_regression(OLS)`, color = "Linear_regression(OLS)")) +
  
  # Add Bayesian linear regression points with a specific color
  geom_point(data = data_21, aes(x = time, y = `Bayesian_linear_regression`, color = "Bayesian_linear_regression")) +
  
  # Add Bayesian hierarchy model points with a specific color
  geom_point(data = data_21, aes(x = time, y = `Bayesian_hierarchy_model`, color = "Bayesian_hierarchy_model")) +
  
  # Use scale_color_manual to set the colors
  scale_color_manual(values = cbPalette) +
  
  # Customize the plot with labels
  labs(x = "Time", y = "Values", color = "Model Type") +
  theme_minimal()

# small variance: subject 16 ####
ggplot(data = p_data[p_data$subject.==16,],aes(x=test_time,y=motor_UPDRS))+
  geom_line()

row_number_16<-which(p_data$subject.==16)
timeline_16<-p_data[p_data$subject.==16,"test_time"]

# true value , ols value
data_16<-cbind(timeline_16,predicted_values_lm_y1[row_number_16],p_data$motor_UPDRS[row_number_16])%>%
  as.data.frame()
colnames(data_16)<-c("time","Linear_regression(OLS)","true_value")
head(data_16)

# bayesian linear regression
data_16_simple<-fit_simple_2000_result$y_pred[,row_number_16] %>% as.data.frame()
data_16$Bayesian_linear_regression<-colMeans(data_16_simple) %>% as.vector()

# bayesian hierarchy model
data_16_hierarchy<-fit_hierarchy_2000_result$y_pred[,row_number_16] %>% as.data.frame()

data_16$Bayesian_hierarchy_model<-colMeans(data_16_hierarchy) %>% as.vector()



# Define a color palette
cbPalette <- c("true_value" = "#999999", 
               "Linear_regression(OLS)" = "#E69F00", 
               "Bayesian_linear_regression" = "#56B4E9", 
               "Bayesian_hierarchy_model" = "#009E73")

time_trend_plot_16<-ggplot() +
  # Add true value line with a specific color
  geom_line(data = data_16, aes(x = time, y = true_value, color = "true_value")) +
  
  # Add linear regression points with a specific color
  geom_point(data = data_16, aes(x = time, y = `Linear_regression(OLS)`, color = "Linear_regression(OLS)")) +
  
  # Add Bayesian linear regression points with a specific color
  geom_point(data = data_16, aes(x = time, y = `Bayesian_linear_regression`, color = "Bayesian_linear_regression")) +
  
  # Add Bayesian hierarchy model points with a specific color
  geom_point(data = data_16, aes(x = time, y = `Bayesian_hierarchy_model`, color = "Bayesian_hierarchy_model")) +
  
  # Use scale_color_manual to set the colors
  scale_color_manual(values = cbPalette) +
  labs(title = "Motor UPDRS tracking over the six-month trail \n subject 16", x = "Test time", y = "Value",color = "Model Type")

time_trend_plot_16
ggsave("time_trend_plot_16.png", plot = time_trend_plot_16, width = 6, height = 4, dpi = 300)


# mediate variance: subject 4 ####
ggplot(data = p_data[p_data$subject.==4,],aes(x=test_time,y=motor_UPDRS))+
  geom_line()

row_number_4<-which(p_data$subject.==4)
timeline_4<-p_data[p_data$subject.==4,"test_time"]

# true value , ols value
data_4<-cbind(timeline_4,predicted_values_lm_y1[row_number_4],p_data$motor_UPDRS[row_number_4])%>%
  as.data.frame()
colnames(data_4)<-c("time","Linear_regression(OLS)","true_value")
head(data_4)

# bayesian linear regression
data_4_simple<-fit_simple_2000_result$y_pred[,row_number_4] %>% as.data.frame()
data_4$Bayesian_linear_regression<-colMeans(data_4_simple) %>% as.vector()

# bayesian hierarchy model
data_4_hierarchy<-fit_hierarchy_2000_result$y_pred[,row_number_4] %>% as.data.frame()

data_4$Bayesian_hierarchy_model<-colMeans(data_4_hierarchy) %>% as.vector()



# Define a color palette
cbPalette <- c("true_value" = "#999999", 
               "Linear_regression(OLS)" = "#E69F00", 
               "Bayesian_linear_regression" = "#56B4E9", 
               "Bayesian_hierarchy_model" = "#009E73")

time_trend_plot_4<-ggplot() +
  # Add true value line with a specific color
  geom_line(data = data_4, aes(x = time, y = true_value, color = "true_value")) +
  
  # Add linear regression points with a specific color
  geom_point(data = data_4, aes(x = time, y = `Linear_regression(OLS)`, color = "Linear_regression(OLS)")) +
  
  # Add Bayesian linear regression points with a specific color
  geom_point(data = data_4, aes(x = time, y = `Bayesian_linear_regression`, color = "Bayesian_linear_regression")) +
  
  # Add Bayesian hierarchy model points with a specific color
  geom_point(data = data_4, aes(x = time, y = `Bayesian_hierarchy_model`, color = "Bayesian_hierarchy_model")) +
  
  # Use scale_color_manual to set the colors
  scale_color_manual(values = cbPalette) +
  labs(title = "Motor UPDRS tracking over the six-month trail \n subject 4", x = "Test time", y = "Value",color = "Model Type")

time_trend_plot_4
ggsave("time_trend_plot_4.png", plot = time_trend_plot_4, width = 6, height = 4, dpi = 300)


lapply((p_data[which(p_data$subject.==17),]), sd)

# combine plot together ####

library(gridExtra)
# Arrange the plots vertically
grid.arrange(time_trend_plot_4, time_trend_plot_16, time_trend_plot_17, ncol = 1)


# case 2: 4 ####

ggplot(data = p_data[p_data$subject.==4,],aes(x=test_time,y=motor_UPDRS))+
  geom_line()
row_number_4<-which(p_data$subject.==4)
timeline_4<-p_data[p_data$subject.==4,"test_time"]

# true value , ols value
data_4<-cbind(timeline_4,predicted_values_lm_y1[row_number_4],p_data$motor_UPDRS[row_number_4])%>%
  as.data.frame()
colnames(data_4)<-c("Test_time","Linear_regression(OLS)","value_of_y")
head(data_4)

# bayesian linear regression
data_4_simple<-fit_simple_2000_result$y_pred[,row_number_4] %>% as.data.frame()
data_4$Bayesian_regression_model<-colMeans(data_4_simple) %>% as.vector()

# bayesian hierarchy model
data_4_hierarchy<-fit_hierarchy_2000_result$y_pred[,row_number_4] %>% as.data.frame()

data_4$Hierarchical_Bayesian_model<-colMeans(data_4_hierarchy) %>% as.vector()

head(data_4)

# Define a color palette
cbPalette <- c("true_value" = "#999999", 
               "Linear_regression(OLS)" = "#E69F00", 
               "Bayesian_linear_regression" = "#56B4E9", 
               "Bayesian_hierarchy_model" = "#009E73")

time_trend_plot_4<-  ggplot() + 
  # Add true value line with a specific color
  geom_line(data = data_4, aes(x = Test_time, y = value_of_y, color = "true_value"), size = 1) +  # Set color directly for true value
  
  # Add linear regression points with a specific color
  geom_point(data = data_4, aes(x = Test_time, y = `Linear_regression(OLS)`, color = "Linear_regression(OLS)")) + 
  
  # Add Bayesian linear regression points with a specific color
  geom_point(data = data_4, aes(x = Test_time, y = `Bayesian_regression_model`, color = "Bayesian_linear_regression")) + 
  
  # Add Bayesian hierarchy model points with a specific color
  geom_point(data = data_4, aes(x = Test_time, y = `Hierarchical_Bayesian_model`, color = "Bayesian_hierarchy_model")) + 
  
  # Define the color scale
  scale_colour_manual(values = cbPalette) + 
  theme_minimal(base_size = 15) + 
  theme(
    panel.background = element_blank(), 
    panel.grid.major = element_line(color = "gray80"), 
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    axis.text = element_text(size = 9), 
    axis.title = element_text(size = 9), 
    legend.position = "none" 
  )
time_trend_plot_4

ggsave("time_trend_plot_4.png", plot = time_trend_plot_4, width = 6, height = 4, dpi = 300)

# case 3: 16 ####

ggplot(data = p_data[p_data$subject.==16,],aes(x=test_time,y=motor_UPDRS))+
  geom_line()
row_number_16<-which(p_data$subject.==16)
timeline_16<-p_data[p_data$subject.==16,"test_time"]

# true value , ols value
data_16<-cbind(timeline_16,predicted_values_lm_y1[row_number_16],p_data$motor_UPDRS[row_number_16])%>%
  as.data.frame()
colnames(data_16)<-c("Test_time","Linear_regression(OLS)","value_of_y")
head(data_16)

# bayesian linear regression
data_16_simple<-fit_simple_2000_result$y_pred[,row_number_16] %>% as.data.frame()
data_16$Bayesian_regression_model<-colMeans(data_16_simple) %>% as.vector()

# bayesian hierarchy model
data_16_hierarchy<-fit_hierarchy_2000_result$y_pred[,row_number_16] %>% as.data.frame()

data_16$Hierarchical_Bayesian_model<-colMeans(data_16_hierarchy) %>% as.vector()

head(data_16)

# Define a color palette
cbPalette <- c("true_value" = "#999999", 
               "Linear_regression(OLS)" = "#E69F00", 
               "Bayesian_linear_regression" = "#56B4E9", 
               "Bayesian_hierarchy_model" = "#009E73")

time_trend_plot_16<-  ggplot() + 
  # Add true value line with a specific color
  geom_line(data = data_16, aes(x = Test_time, y = value_of_y, color = "true_value"), size = 1) +  # Set color directly for true value
  
  # Add linear regression points with a specific color
  geom_point(data = data_16, aes(x = Test_time, y = `Linear_regression(OLS)`, color = "Linear_regression(OLS)")) + 
  
  # Add Bayesian linear regression points with a specific color
  geom_point(data = data_16, aes(x = Test_time, y = `Bayesian_regression_model`, color = "Bayesian_linear_regression")) + 
  
  # Add Bayesian hierarchy model points with a specific color
  geom_point(data = data_16, aes(x = Test_time, y = `Hierarchical_Bayesian_model`, color = "Bayesian_hierarchy_model")) + 
  
  # Define the color scale
  scale_colour_manual(values = cbPalette) + 
  theme_minimal(base_size = 15) + 
  theme(
    panel.background = element_blank(), 
    panel.grid.major = element_line(color = "gray80"), 
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    axis.text = element_text(size = 9), 
    axis.title = element_text(size = 9), 
    legend.position = "none" 
  )
time_trend_plot_16

ggsave("time_trend_plot_16.png", plot = time_trend_plot_16, width = 6, height = 4, dpi = 300)

# combine the plot together ####


# Combine plots with a shared legend
library(patchwork)

# Combine plots vertically using `patchwork`
combined_plot <- (time_trend_plot_16 / time_trend_plot_4 / time_trend_plot_17) + 
  plot_layout(guides = "collect") &  # Collects the legend and shows it once
  theme(
    legend.position = "top",  
    legend.title = element_blank(),# Position the legend at the top
    legend.text = element_text(size = 9)  # Set legend text size to 9 points
  )

combined_plot
ggsave("combined_plot.png", plot = combined_plot, width = 6.5, height = 10, dpi = 300)





