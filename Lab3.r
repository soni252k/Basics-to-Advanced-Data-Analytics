#get the data and view it 
data("sleep")
View(sleep)
#To get the information about the type of data, print the head values (top 6 values) and analyze. 
head(sleep)

#-------------------------------------------------------------------------------
#One-sample t test
#Using in-built function: t.test()
#-------------------------------------------------
x<- sleep$extra
#Let H0: mu=2
#Let H1: mu<2
Test<- t.test(x,mu=2, alternative= "less", conf.level = 0.95) 
Test

#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#we can also do the following: 
#t(tab) at df=19 and conf.level=0.95 for one tailed is 1.729
#if(Test$statistic<1.729) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#---------------
#Let H0: mu=2
#Let H1: mu>2
Test<- t.test(x,mu=2, alternative= "greater", conf.level = 0.95) 
Test
Test$p.value

#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#---------------
#Let H0: mu=2
#Let H1: mu not equal to 2 
Test<- t.test(x,mu=2, conf.level = 0.95) 
Test

#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#-------------------------------------------------------------------------------
#Two-sample t test
#Using in-built function: t.test()
#-------------------------------------------------
# Separate groups 1 & 2
sleep1 <- subset(sleep, group == 1)
sleep2 <- subset(sleep, group == 2)

#Let H0: mu1=mu2
#Let H1: mu1<mu2
Test<- t.test(sleep1$extra,sleep2$extra, alternative= "less", conf.level = 0.95)
Test
#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#---------------
#Let H0: mu1=mu2
#Let H1: mu1>mu2
Test<- t.test(sleep1$extra,sleep2$extra, alternative= "greater", conf.level = 0.95)
Test
#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#---------------
#Let H0: mu1=mu2
#Let H1: mu1 not equal to mu2
Test<- t.test(sleep1$extra,sleep2$extra, conf.level = 0.95)
Test
#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#-------------------------------------------------
#Two-sample t test
#Using own function
#-------------------------------------------------
two_t_test<- function(x,y){
  n1 <- length(x)
  n2 <- length(y)
  s <- sqrt( ((n1-1)* var(x) + (n2-1)*var(y))/(n1+n2-2))
  t <- (mean(x) - mean(y)) / sqrt((sd(x)*sd(x)/n1) + (sd(y)*sd(y)/n2))
  cat("t calc=", t)
  cat("\nt tab=", 1.729)
  cat("\n-----------------------------\n")
  return (t)
}

if(two_t_test(sleep1$extra,sleep2$extra)<1.729){
  cat("NULL hypothesis is accepted.\nmu(sleep1$extra) = mu(sleep2$extra)\n")
  cat("\n-----------------------------\n")
}else {
  cat("NULL hypothesis is rejected.\nmu(sleep1$extra) not equal to mu(sleep2$extra)\n")
  cat("\n-----------------------------\n")
}

#-------------------------------------------------------------------------------
#Paired t test
#Using in-built function: t.test()
#-------------------------------------------------
#Let H0: mu1=mu2
#Let H1: mu1<mu2
Test<- t.test(sleep1$extra,sleep2$extra, alternative= "less", paired = TRUE, conf.level = 0.95)
Test
#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#---------------
#Let H0: mu1=mu2
#Let H1: mu1>mu2
Test<- t.test(sleep1$extra,sleep2$extra, alternative= "greater",paired =TRUE, conf.level = 0.95)
Test
#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#---------------
#Let H0: mu1=mu2
#Let H1: mu1 not equal to mu2
Test<- t.test(sleep1$extra,sleep2$extra, paired =TRUE, conf.level = 0.95)
Test
#conclusion
if(Test$p.value>0.05) {print("Accept the NULL hypothesis.")}else {print("Reject the NULL hypothesis.")}

#-------------------------------------------------
#Paired t test
#Using own function
#-------------------------------------------------
paired_t_test <- function(v1, v2, siglevel = 5, alternative_hypo = 0) {
  #h0: v1=v2
  #h1: v1 not equal to v2
  if( length(v1) != length(v2)) {
    print("vectors should be of equal size " )
    return()
  }
  di = v1-v2
  di_mean = mean(di) 
  sd = sd(di)
  n = length(v1)
  t_cal = di_mean/(sd/(sqrt(n)))
  if(alternative_hypo == 0){
    t_tab = 1.729
    cat("t calc=", t_cal)
    cat("\nt tab=", t_tab)
    cat("\n-----------------------------\n")
    if(t_cal < t_tab) return (TRUE) else return (FALSE)
  } 
  #find the value of t_tab using the table of t distribution 
  #with alpha=0.5 and dof=9
  t_tab = 1.729 
  if(t_cal > -t_tab && t_cal < t_tab) return (TRUE) else return (FALSE)
  
}

# to test if participants who used Drug 1 gained equal hours of sleep when 
# compared with the same participants who used Drug 2 or not.
if(paired_t_test(sleep1$extra,sleep2$extra,siglevel = 5, 0)){
  cat("NULL hypothesis is accepted.\nmu(sleep1$extra) = mu(sleep2$extra)")
  cat("\n-----------------------------\n")
}else {
  cat("NULL hypothesis is rejected.\nmu(sleep1$extra) not equal to mu(sleep2$extra)")
  cat("\n-----------------------------\n")
}

#-------------------------------------------------------------------------------
#Note-> tabulated value of t can be found using the following command- 
#t_tab = qt(alpha,dof)
#-------------------------------------------------------------------------------

