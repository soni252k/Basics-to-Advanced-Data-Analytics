#import the dataset and view it 
crop.data <- read.csv("C:/Users/sonis/Desktop/CSE Sem 6/2- lab/1- DA/crop.data.csv",
                      header = TRUE, 
                      colClasses = c("factor", "factor", "factor", "numeric"))
View(crop.data)
#To get the information about the type of data, print the head values (top 6 values) and analyze. 
head(crop.data)

#To get the idea about the data, print the summary of the dataset. 
summary(crop.data)

#-------------------------------------------------------------------------------
#One-way ANOVA
#Using in-built function: aov()
#-------------------------------------------------
#Analysis 1- Effect of fertilizer on yield
oneWay1 <- aov(yield ~ fertilizer, data= crop.data)
summary(oneWay1)
#Analysis 2- Effect of block on yield
oneWay2 <- aov(yield ~ block, data= crop.data)
summary(oneWay2)
#Analysis 3- Effect of density on yield
oneWay3 <- aov(yield ~ density, data= crop.data)
summary(oneWay3)

#-------------------------------------------------
#One-way ANOVA
#Using own function
#-------------------------------------------------
oneWay = function(data = list(), alpha= 0.05){
  k= length(data); grand_total= 0; bss= 0; wss = 0; N= 0; mean_list = list()
  for(i in c(1:k)){
    for(j in data[[i]]){
      grand_total = grand_total + j ; N = N + 1  
    }
  }
  grand_mean = grand_total / N  
  for(i in c(1:k)){
    mean_list = append(mean_list, mean(data[[i]]))
  }
  for(i in c(1:k)){
    bss = bss + (((grand_mean-mean_list[[i]])**2)*length(data[[i]]))
    wss = wss + sum(((data[[i]]-mean_list[[i]])**2))
  }
  msb = bss / (k-1) ; msw = wss / (N-k) 
  F_cal = msb / msw ; F_tab = qf(p = alpha, df1 = k-1, df2 = N-k) 
  cat("\n---------------------\nInterpretation 1\n(using F value)\n")
  cat("\nF calc= ",F_cal,"\nF tab= " ,F_tab)
  if(F_cal < F_tab)
    cat("\n\nAs F_cal < F_tab, \nthus accept the NULL hypothesis.\n") else 
        cat("\n\nAs F_cal > F_tab, \nthus reject the NULL hypothesis.\n")
  p.value <- pf(F_cal, k-1, N-k, lower.tail = FALSE)
  cat("---------------------\nInterpretation 2\n(using p value)\n")
  cat("\np= ", p.value, "\nalpha= ", alpha)
  if(p.value>alpha)
    cat("\n\nAs p > alpha, \nthus accept the NULL hypothesis.\n--------------------") else 
          cat("\n\nAs p < alpha, \nthus reject the NULL hypothesis.\n--------------------")
}


#Analysis 1- Effect of fertilizer on yield
data = list()
for(i in c(1: length(levels(crop.data$fertilizer)))){
  data = append(data, list(crop.data$yield[crop.data$fertilizer == i]))
}
oneWay(data, alpha=0.05)

#Analysis 2- Effect of density on yield
data = list()
for(i in c(1: length(levels(crop.data$density)))){
  data = append(data, list(crop.data$yield[crop.data$density == i]))
}
oneWay(data, alpha=0.05)

#Analysis 3- Effect of block on yield
data = list()
for(i in c(1: length(levels(crop.data$block)))){
  data = append(data, list(crop.data$yield[crop.data$block == i]))
}
oneWay(data, alpha=0.05)
#-------------------------------------------------------------------------------
#Two-way ANOVA
#Using in-built function: aov()
#-------------------------------------------------
#Analysis 1- Effect of fertilizer + density on yield
twoWay1 <- aov(yield ~ fertilizer + density, data= crop.data)
summary(twoWay1)
#Analysis 2- Effect of fertilizer + block on yield
twoWay2 <- aov(yield ~ fertilizer + block, data= crop.data)
summary(twoWay2)
#Analysis 3- Effect of density + block on yield
twoWay3 <- aov(yield ~ density + block, data= crop.data)
summary(twoWay3)
#-------------------------------------------------------------------------------
