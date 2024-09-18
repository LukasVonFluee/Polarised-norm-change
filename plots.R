###################################################################################
# This is an adapted version of the code used for the following paper:
# Title: Analysis of threshold simulation model for paper: "When norm change hurts"
# Authors: Charles Efferson, Sönke Ehret, Lukas von Flüe, and Sonja Vogt.
###################################################################################



base_dir <- getwd()

# Parameters specifying payoffs in coordination game that we don't vary:
b <- 0
d <- 1    
g <- 0 

G <- 2 # parameter specifying number of groups

intervention1line <- 1 

intervention2line <- 49 

###################################################################################
###################################################################################

# Part 1: Combined plots

###################################################################################
###################################################################################

# List all subdirectories
subdirectories <- list.dirs(path = base_dir, recursive = FALSE)

# Initialize a list to store the loaded .RData files
loaded_data_list <- list()

# Loop through each subdirectory and load the corresponding .RData files
for (subdir in subdirectories) {
  # Get the list of .RData files in the current subdirectory
  rdata_files <- list.files(path = subdir, pattern = "\\.RData$", full.names = TRUE)
  
  # Extract just the parameter combination name from the subdirectory
  param_name <- basename(subdir)
  
  # Load each .RData file
  for (rdata_file in rdata_files) {
    # Load the file into the current environment
    load(rdata_file)
    
    # Store the loaded data in the list with the subdirectory name as the list element name
    loaded_data_list[[param_name]] <- result
  }
}

###################################################################################

# Gather all amenable treatments and all resistant treatments in separate lists
amenable_list <- list()
resistant_list <- list()

for (param_name in names(loaded_data_list)) {
  if (grepl("target0", param_name)) {
    amenable_list[[param_name]] <- loaded_data_list[[param_name]]
  } else if (grepl("target1", param_name)) {
    resistant_list[[param_name]] <- loaded_data_list[[param_name]]
  }
}

###################################################################################
###################################################################################

if (!dir.exists("combined_plots")) {
  dir.create("combined_plots")
}
setwd("combined_plots")


for (i in 1:length(amenable_list)) {
  
  # Plotting parameters:
  
  windowWidth <- 20
  windowHeight <- 15
  
  lineWidth <- 3
  
  cexNum <- 1.5
  cexWord <- 2
  
  # Retrieve alpha parameter value of current parameter combination, store it under "label", to use it in mtext() in plots
  
  label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
  
  ###################################################################################
  
  # Plot 1: Fraction of Alt choices (upper left)
  
  ###################################################################################
  
  pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                   "_phi", resistant_list[[i]]$phi, 
                   "_a", resistant_list[[i]]$a, 
                   "_h", resistant_list[[i]]$h,
                   "_s_new", resistant_list[[i]]$s_new,
                   "_n", resistant_list[[i]]$n,
                   ".pdf", sep = "") , width = windowWidth, height = windowHeight)
  
  par(mar = c(6,7,4,2),mgp = c(3.75,1,0))
  
  # Have to calculate fractions first because in simulation we recorded frequencies
  
  # Amenable
  
  amenable_list[[i]]$summary_results$fract_alt <- amenable_list[[i]]$summary_results$freq_alt / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_sq <- amenable_list[[i]]$summary_results$freq_sq / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_low_ci_freq_sq <- amenable_list[[i]]$summary_results$low_ci_freq_sq / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_high_ci_freq_sq <- amenable_list[[i]]$summary_results$high_ci_freq_sq / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_low_ci_freq_alt <- amenable_list[[i]]$summary_results$low_ci_freq_alt / amenable_list[[i]]$N
  amenable_list[[i]]$summary_results$fract_high_ci_freq_alt <- amenable_list[[i]]$summary_results$high_ci_freq_alt / amenable_list[[i]]$N
  
  # Resistant:
  
  resistant_list[[i]]$summary_results$fract_alt <- resistant_list[[i]]$summary_results$freq_alt / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_sq <- resistant_list[[i]]$summary_results$freq_sq / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_low_ci_freq_sq <- resistant_list[[i]]$summary_results$low_ci_freq_sq / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_high_ci_freq_sq <- resistant_list[[i]]$summary_results$high_ci_freq_sq / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_low_ci_freq_alt <- resistant_list[[i]]$summary_results$low_ci_freq_alt / resistant_list[[i]]$N
  resistant_list[[i]]$summary_results$fract_high_ci_freq_alt <- resistant_list[[i]]$summary_results$high_ci_freq_alt / resistant_list[[i]]$N
  
  # plot par:
  par(fig = c(0,0.45,0.5,1))
  
  # Alt choices amenable
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
       xlab = "Period", ylab = "Fraction of Alt choices",
       # main = "a) Alt choices",
       col = "darkgreen", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('a',side = 3,at = -15,line = 1,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2)
  
  # Second parameter combination with dashed lines (Resistant)
  
  # Add dashed lines for coordination on Alt (Resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals for Alt (Resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "red", lty = 2)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "red", lty = 2)
  
  # Add a legend with labels
  # legend("topright", legend = c("Alt Amenable", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))
  
  ###################################################################################
  
  # Plot 2: Miscoordination (Upper right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0.5,1),new = T)
  
  # Create the initial line plot with frequencies of miscoordination for amenable targets
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$miscoordination, type = "l",
       xlab = "Period", ylab = "Frequency of miscoordination",
       # main = "b) Coordination on SQ and Alt",
       col = "darkgreen", ylim = c(0, amenable_list[[i]]$N/2),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('b',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals (amenable)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$low_ci_miscoordination, col = "darkgreen", lty = 2, lwd = 0.75)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$high_ci_miscoordination, col = "darkgreen", lty = 2, lwd = 0.75)
  
  # Miscoord resistant
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$miscoordination, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$low_ci_miscoordination, col = "red", lty = 2, lwd = 0.75)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$high_ci_miscoordination, col = "red", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("SQ Amenable", "95% CI", "Alt Amenable", "95% CI", "SQ Resistant", "95% CI", "Alt Resistant", "95% CI"),
  #        col = c("lightgreen", "lightgreen", "darkgreen", "darkgreen", "pink", "pink", "red", "red"),
  #        lty = c(1, 2, 1, 2, 1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 3: Average payoffs (Lower left)
  
  ###################################################################################
  
  par(fig = c(0,0.45,0,0.5),new = T)
  
  # Create the initial line plot with Average payoffs for amenable targets
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$avg_payoff, type = "l",
       xlab = "Period", ylab = "Average payoff",
       # main = "c) Average payoffs",
       col = "darkgreen", ylim = c(0, amenable_list[[i]]$h),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('c',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord) 
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals (amenable)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$low_ci_avg_payoff, col = "darkgreen", lty = 2, lwd = 0.75)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$high_ci_avg_payoff, col = "darkgreen", lty = 2, lwd = 0.75)
  
  # Average payoffs for resistant targets
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$avg_payoff, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$low_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$high_ci_avg_payoff, col = "red", lty = 2, lwd = 0.75)
  
  # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  ###################################################################################
  
  # Plot 4: Gini coefficient (Lower right)
  
  ###################################################################################
  
  par(fig = c(0.45,0.9,0,0.5),new = T)
  
  # Create the initial line plot with Gini coefficients for amenable targets
  plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$gini_coefficient, type = "l",
       xlab = "Period", ylab = "Gini coefficient",
       # main = "d) Gini coefficient",
       col = "darkgreen", ylim = c(0, 1),
       cex.axis = cexNum,cex.lab = cexWord,lwd = lineWidth)
  # Note, "at=..." in "mtext()" is relative to the x-axis. Here, the x-axis lies between 0-100:
  mtext('d',side = 3,at = -15,line = 0.75,cex = 1.5 * cexWord)
  # mtext(label,at = 50,line = 1,cex = cexWord, adj=0.5)
  
  # Add confidence intervals (amenable)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$low_ci_gini_coefficient, col = "darkgreen", lty = 2)
  lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$high_ci_gini_coefficient, col = "darkgreen", lty = 2)
  
  # Gini coefficients for resistant
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$gini_coefficient, col = "red", lty = 1, lwd = 3)
  
  # Add confidence intervals (resistant)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$low_ci_gini_coefficient, col = "red", lty = 2)
  lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$high_ci_gini_coefficient, col = "red", lty = 2)
  
  # # Add a legend with labels
  # legend("topright", legend = c("Amenable", "95% CI", "Resistant", "95% CI"),
  #        col = c("darkgreen", "darkgreen", "red", "red"),
  #        lty = c(1, 2, 1, 2))  
  
  alpha_val <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha))
  beta_val <- bquote(italic(beta) == .(2))
  
  par(fig = c(0.9,1,0,1),new = T)
  par(mar=c(0,0,0,0))
  plot(0:1,0:1,xlab='',ylab='',type = 'l',lty = 'blank',axes=F)
  text(0.05, 0.52, alpha_val, cex = cexWord, adj = 0)
  text(0.05, 0.48, beta_val, cex = cexWord, adj = 0)
  
  
  dev.off()
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for amenable and resistant targets as single plot (see subplot (a) in combined plot)
  #########################################################################################################################
  #########################################################################################################################
  
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 2.75  # Adjusted for larger axis numbers
    cexWord <- 3.25   # Adjusted for larger labels
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     ".pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(6, 6, 4, 2) + 0.5)  # Increase left margin (second number) to avoid cutoff
    par(mgp = c(4, 1.5, 0))         # Move axis labels away from the axis
    
    # Calculate fractions and confidence intervals for both amenable and resistant lists
    amenable_list[[i]]$summary_results$fract_alt <- amenable_list[[i]]$summary_results$freq_alt / amenable_list[[i]]$N
    amenable_list[[i]]$summary_results$fract_low_ci_freq_alt <- amenable_list[[i]]$summary_results$low_ci_freq_alt / amenable_list[[i]]$N
    amenable_list[[i]]$summary_results$fract_high_ci_freq_alt <- amenable_list[[i]]$summary_results$high_ci_freq_alt / amenable_list[[i]]$N
    
    resistant_list[[i]]$summary_results$fract_alt <- resistant_list[[i]]$summary_results$freq_alt / resistant_list[[i]]$N
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt <- resistant_list[[i]]$summary_results$low_ci_freq_alt / resistant_list[[i]]$N
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt <- resistant_list[[i]]$summary_results$high_ci_freq_alt / resistant_list[[i]]$N
    
    # Main Plot for Fraction of Alt Choices
    plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt, type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices", col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)
    
    # Add dashed lines at y = 0 and y = 1 (drawn first, behind other elements)
    abline(h = 0, col = "black", lty = 2)  # Dashed line at y = 0
    abline(h = 1, col = "black", lty = 2)  # Dashed line at y = 1
    
    # Intervention lines
    abline(v = intervention1line, col = "black", lty = 2)
    abline(v = intervention2line, col = "black", lty = 2)
    
    # Add lines for the resistant parameter combination
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt, col = "red", lty = 1, lwd = lineWidth)
    
    # Add ticks and labels for the intervention lines on top
    axis(side = 3, at = c(intervention1line, intervention2line), labels = FALSE, lwd.ticks = 1) # Add ticks
    text(x = c(intervention1line, intervention2line), y = 1.10, labels = c("Intervention 1", "Intervention 2"), xpd = TRUE, cex = 2.75) # Add labels
    
    # Add confidence intervals for amenable
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "darkgreen", lty = 2, lwd=3)
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "darkgreen", lty = 2, lwd=3)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt, col = "red", lty = 2, lwd=3)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt, col = "red", lty = 2, lwd=3)
    
    # Add a legend for better understanding, moving it to bottom-right and increasing the font size
    legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, lwd = lineWidth, cex = 2.75, bg = "white", box.lwd = 2)
    
    # Close the PDF device
    dev.off()
  }
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for amenable and resistant targets GROUP 1
  #########################################################################################################################
  #########################################################################################################################
   
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 2.75  # Adjusted for larger axis numbers
    cexWord <- 3.25   # Adjusted for larger labels
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     "group1.pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(6, 6, 4, 2) + 0.5)  # Increase left margin (second number) to avoid cutoff
    par(mgp = c(4, 1.5, 0))         # Move axis labels away from the axis
    
    # Calculate fractions and confidence intervals for group1 both amenable and resistant lists
    amenable_list[[i]]$summary_results$fract_alt_group1 <- amenable_list[[i]]$summary_results$freq_alt_group1 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group1 <- amenable_list[[i]]$summary_results$low_ci_freq_alt_group1 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group1 <- amenable_list[[i]]$summary_results$high_ci_freq_alt_group1 / (amenable_list[[i]]$N/G)
    
    resistant_list[[i]]$summary_results$fract_alt_group1 <- resistant_list[[i]]$summary_results$freq_alt_group1 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group1 <- resistant_list[[i]]$summary_results$low_ci_freq_alt_group1 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group1 <- resistant_list[[i]]$summary_results$high_ci_freq_alt_group1 / (resistant_list[[i]]$N/G)
    
    # Main Plot for Fraction of Alt Choices
    plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt_group1, type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices in group 1", col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)
    
    # Add dashed lines at y = 0 and y = 1 (drawn first, behind other elements)
    abline(h = 0, col = "black", lty = 2)  # Dashed line at y = 0
    abline(h = 1, col = "black", lty = 2)  # Dashed line at y = 1
    
    # Intervention lines
    abline(v = intervention1line, col = "black", lty = 2)
    abline(v = intervention2line, col = "black", lty = 2)
    
    
    # Add lines for the resistant parameter combination
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt_group1, col = "red", lty = 1, lwd = lineWidth)
    
    # Add ticks and labels for the intervention lines on top
    axis(side = 3, at = c(intervention1line, intervention2line), labels = FALSE, lwd.ticks = 1) # Add ticks
    text(x = c(intervention1line, intervention2line), y = 1.10, labels = c("Intervention 1", "Intervention 2"), xpd = TRUE, cex = 2.75) # Add labels
    
    # Add confidence intervals for amenable
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group1, col = "darkgreen", lty = 2, lwd=3)
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group1, col = "darkgreen", lty = 2, lwd=3)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group1, col = "red", lty = 2, lwd=3)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group1, col = "red", lty = 2, lwd=3)
    
    # Add a legend for better understanding, moving it to bottom-right and increasing the font size
    legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, lwd = lineWidth, cex = 2.75, bg = "white", box.lwd = 2)
    
    # Close the PDF device
    dev.off()
  }
  
  
  #########################################################################################################################
  #########################################################################################################################
  # SINGLE PLOT: Alt choice fractions for amenable and resistant targets GROUP 2
  #########################################################################################################################
  #########################################################################################################################
  
  for (i in 1:length(amenable_list)) {
    
    # Adjusted Plotting Parameters for a Single Main Plot
    windowWidth <- 12
    windowHeight <- 11
    lineWidth <- 2
    cexNum <- 2.75  # Adjusted for larger axis numbers
    cexWord <- 3.25   # Adjusted for larger labels
    
    # Retrieve alpha parameter value for labeling
    label <- bquote(italic(alpha) == .(amenable_list[[i]]$alpha) ~ "," ~ italic(beta) == 2)
    
    # Open a PDF file for the plot
    pdf(file = paste("alpha", resistant_list[[i]]$alpha, 
                     "_phi", resistant_list[[i]]$phi, 
                     "_a", resistant_list[[i]]$a, 
                     "_h", resistant_list[[i]]$h,
                     "_s_new", resistant_list[[i]]$s_new,
                     "_n", resistant_list[[i]]$n,
                     "group2.pdf", sep = ""), width = windowWidth, height = windowHeight)
    
    # Adjust margins and plotting parameters
    par(mar = c(6, 6, 4, 2) + 0.5)  # Increase left margin (second number) to avoid cutoff
    par(mgp = c(4, 1.5, 0))         # Move axis labels away from the axis
    
    # Calculate fractions and confidence intervals for group2 both amenable and resistant lists
    amenable_list[[i]]$summary_results$fract_alt_group2 <- amenable_list[[i]]$summary_results$freq_alt_group2 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group2 <- amenable_list[[i]]$summary_results$low_ci_freq_alt_group2 / (amenable_list[[i]]$N/G)
    amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group2 <- amenable_list[[i]]$summary_results$high_ci_freq_alt_group2 / (amenable_list[[i]]$N/G)
    
    resistant_list[[i]]$summary_results$fract_alt_group2 <- resistant_list[[i]]$summary_results$freq_alt_group2 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group2 <- resistant_list[[i]]$summary_results$low_ci_freq_alt_group2 / (resistant_list[[i]]$N/G)
    resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group2 <- resistant_list[[i]]$summary_results$high_ci_freq_alt_group2 / (resistant_list[[i]]$N/G)
    
    # Main Plot for Fraction of Alt Choices
    plot(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_alt_group2, type = "l", lwd = lineWidth,
         xlab = "period", ylab = "fraction of Alt choices in group 2", col = "darkgreen", ylim = c(0, 1),
         cex.axis = cexNum, cex.lab = cexWord)
    
    # Add dashed lines at y = 0 and y = 1 (drawn first, behind other elements)
    abline(h = 0, col = "black", lty = 2)  # Dashed line at y = 0
    abline(h = 1, col = "black", lty = 2)  # Dashed line at y = 1
    
    # Intervention lines
    abline(v = intervention1line, col = "black", lty = 2)
    abline(v = intervention2line, col = "black", lty = 2)
    
    
    # Add lines for the resistant parameter combination
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_alt_group2, col = "red", lty = 1, lwd = lineWidth)
    
    # Add ticks and labels for the intervention lines on top
    axis(side = 3, at = c(intervention1line, intervention2line), labels = FALSE, lwd.ticks = 1) # Add ticks
    text(x = c(intervention1line, intervention2line), y = 1.10, labels = c("Intervention 1", "Intervention 2"), xpd = TRUE, cex = 2.75) # Add labels
    
    # Add confidence intervals for amenable
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_low_ci_freq_alt_group2, col = "darkgreen", lty = 2, lwd=3)
    lines(1:amenable_list[[i]]$t_max, amenable_list[[i]]$summary_results$fract_high_ci_freq_alt_group2, col = "darkgreen", lty = 2, lwd=3)
    
    # Add confidence intervals for resistant
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_low_ci_freq_alt_group2, col = "red", lty = 2, lwd=3)
    lines(1:resistant_list[[i]]$t_max, resistant_list[[i]]$summary_results$fract_high_ci_freq_alt_group2, col = "red", lty = 2, lwd=3)
    
    # Add a legend for better understanding, moving it to bottom-right and increasing the font size
    legend("bottomright", legend = c("Amenable", "Resistant"), col = c("darkgreen", "red"), lty = 1, lwd = lineWidth, cex = 2.75, bg = "white", box.lwd = 2)
    
    # Close the PDF device
    dev.off()
  }

  
}



setwd('..')
