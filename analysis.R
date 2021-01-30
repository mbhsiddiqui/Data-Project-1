# ---------------------------------------------------------Header--------------------------------------------------------------- 
# Name: Maaz Siddiqui
# Year: Summer 2020
# Assignment: Data Project #5
# Description: The scenario was obtained from https://dasl.datadescription.com/datafile/product-placement/. This code 
#              contains plots for the data along with the calculation for confidence interval at 95%. In-depth analysis can 
#              be found with the word document report uploaded along with this code.
#
# -----------------------------------------------------End of Header------------------------------------------------------------ 

# --------------Overview--------------
# An experiment was conducted to determine the effect of placement of a product in a small organic food store. The owner 
# decided to place the product on a shelf which was near eye level for majority of the customers. In addition, it was kept in 
# the area along with other international products. The number of containers sold each week was recorded for 24 weeks (six 
# months) for both before and after the change of location/placement was done.

# ***Important Note: When importing dataset in RStudio use 'From text (readr)' and set delimiter as 'Tab'.

# Data extraction as matrix
productPlacementData <- matrix(NA, nrow=24,ncol=3)    # Empty matrix for organization of data
productPlacementData[,1] <- c(1:24)                               # Number of weeks
productPlacementData[,2] <- product_placement$`Before Change`     # Number of containers sold before change
productPlacementData[,3] <- product_placement$`After Change`      # Number of containers sold after change


# Conversion to data frame
beforeChange <- as.data.frame(productPlacementData[,1:2])
afterChange <- as.data.frame(productPlacementData[,c(1,3)])

# Linear regression
beforeChangeRegMod <- lm(formula=V2~V1,data=beforeChange)
afterChangeRegMod <- lm(formula=V2~V1,data=afterChange)

colnames(productPlacementData) <- c("Week", "Before change",      # Naming of columns in the matrix
                                    "After Change")

# --------------Plots--------------
# test for normality for data before change
qqnorm(productPlacementData[,2], main = "Normal Q-Q Plot for Number of Containers Sold Before Change")
qqline(productPlacementData[,2])

# test for normality for data after change
qqnorm(productPlacementData[,3], main = "Normal Q-Q Plot for Number of Containers Sold After Change")
qqline(productPlacementData[,3])


# Comparison Plots
# barplot for sales before change
barplot(productPlacementData[,2], ylim = c(0,60), main = "Number of Containers Sold Before Placement Change",
        xlab="Week", ylab="Containers Sold", names.arg = as.character(c(1:24)), cex.names=0.82, yaxt="n")         
axis(2, at = seq(0, 60, 5), las = 1, cex.axis=0.82)
abline(a = coef(beforeChangeRegMod)[1],b = coef(beforeChangeRegMod)[2], lwd = 2, col="red")                       # trendline

# barplot for sales before change
barplot(productPlacementData[,3], ylim = c(0,80), main = "Number of Containers Sold After Placement Change",
        xlab="Week", ylab="Containers Sold", names.arg = as.character(c(1:24)), cex.names=0.82, yaxt="n")
axis(2, at = seq(0, 80, 5), las = 1, cex.axis=0.82)
abline(a = coef(afterChangeRegMod)[1],b = coef(afterChangeRegMod)[2], lwd = 2, col="red")                         # trendline


# --------------Descriptive Stats, Convidence Interval, Hypothesis Test--------------
# After change
xbar = round(mean(productPlacementData[,3]),3)
sx = round(sd(productPlacementData[,3]),3)
m = length(productPlacementData[,3])

# Before Change
ybar = round(mean(productPlacementData[,2]),3)
sy = round(sd(productPlacementData[,2]),3)
n = length(productPlacementData[,2])

t.test(productPlacementData[,3], productPlacementData[,2],
       alternative = "greater", 
       var.equal = F, paired = T)                # Returns t-stat, degrees of freedom, sample mean, p-value and conf. interval

difference = productPlacementData[,3] - productPlacementData[,2]
t.test(difference,mu=0,alternative="greater")    # Returns t-stat, degrees of freedom, sample mean, p-value and conf. interval
