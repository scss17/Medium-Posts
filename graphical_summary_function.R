
# Example
normal_data <- rnorm(n = 150)

# Load libraries
library(ggplot2)
library(ggpubr)
library(moments)
library(cowplot)



data_mean <- mean(data, na.rm = TRUE)
data_sdv <- sd(data, na.rm = TRUE)
data_variance <- var(data, na.rm = TRUE)
data_len <- length(data)


#  Functions --------------------------------------------------------------
skewness <- function(x, na.rm = FALSE) {
        
        # Remove NA values if na.rm is set to TRUE
        if (na.rm) { x <- x[!is.na(x)] }
        
        # Calculate the number of elements in the vector and skewness
        n <- length(x)
        skew <- (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3/2)
        
        # Return the skewness value
        return(skew)
}
kurtosis <- function(x, na.rm = FALSE) {
        
        # Remove NA values if na.rm is set to TRUE
        if(na.rm) { x <- x[!is.na()] }
        
        # Calculate the number of elements in the vector and kurtosis
        n <- length(x)
        kurto <- n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
        
        # Return the kurtosis value
        return(kurto)
}


# Plots -------------------------------------------------------------------

# Histogram with density line 
p1 <- ggplot(mapping = aes(x = normal_data)) +
        geom_histogram(aes(y = after_stat(density)), fill = "lightblue4", col = "black") +
        geom_density(color = "black", size = 1, adjust = 2) +
        
        labs(y = "", x = "", title = "Histogram") + 
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) 

# Box-plot
p2 <- ggplot(mapping = aes(x = normal_data)) + 
        geom_boxplot(fill = "lightblue4") +
        
        labs(x = "", title = "Box-plot") + 
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank()) + ylim(-1, 1) 

# Both plots together
plot_grid(p1, p2, labels = c("A", "B"), ncol = 1,
          rel_heights = c(2, 1), align = "hv", axis = "tblr")
