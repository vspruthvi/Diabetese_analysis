
#Summary
#shows mean ,inter quartile range etc. for each parameter
summary(diabetes)


#applying naive bayer's algorithm
#shows conditional probabilites
m=naiveBayes(Outcome ~ .,data = diabetes)
m

# Data visualization
library(ggplot2) 
library(reshape2)
gg <- melt(diabetes)
#aes is used to specify variables and plot graph
#geom_historam is creating a histogram and
#facet_wrap is wrapping multiple histograms in one map
#binwidth= value of each histogram bin
ggplot(gg, aes(x=value, fill=variable)) +
  geom_histogram(binwidth=10)+
  facet_wrap(~variable)

#we know that age is an important factor in case of diabetes so lets check
#the impact of age on outcome
ggplot(diabetes,aes(x=Age,fill=factor(Outcome)))+geom_density(alpha=0.4)+scale_fill_manual(values=c("blue", "red"))+labs(title="Distribution of Age")
#for above plot x-axis is age,as it is density estimate we are usingg geom 
#density and using diffirent colours for outcome 1 which is positive output 
#and outcome zero which means no diabetes

#bmi v/s outcome
ggplot(diabetes,aes(x=BMI,fill=factor(Outcome)))+geom_density(alpha=0.4)+scale_fill_manual(values=c("blue", "red"))+labs(title="Distribution of BMI")

#DiabetesPedigreeFunction v/s outcome
ggplot(diabetes,aes(x=DiabetesPedigreeFunction,fill=factor(Outcome)))+geom_density(alpha=0.4)+scale_fill_manual(values=c("blue", "red"))+labs(title="Distribution of diabetespedigreefunction")
#relation between blood pressure, glucose and age
ggplot(diabetes,aes(x=Glucose,y=BloodPressure,size=Age,color=Outcome))+geom_jitter(alpha=0.4)+scale_color_gradient(low = 'blue', high = 'red')+labs(title="BP and Glucose level against Age")


#Number of Pregnancies has an impact over diabetes outcome
ggplot(diabetes,aes(x=Pregnancies,fill=factor(Outcome)))+geom_bar(position="Dodge")+scale_fill_manual(values=c("blue","red"))+scale_x_continuous(limits=c(0,16))+labs(title="Pregnancies Vs Outcome")


# Finding the correlation between the attributs
library(ggcorrplot)
#first create a corelation matrix using cor
corr<-round(cor(diabetes),1) # here 1 is indicating that we take 1 decimal place precison for corelation

#now we plot this corelation matrix,hc.order indicates that hirarchical order is maintained
#lab= True indicates that the numbers and lables should be specified
#lab_size indicates font size for numbers
#method plots circles
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("red", "white", "blue"), 
           title="Correlogram of Diabetes data", 
           ggtheme=theme_bw)


ggplot(diabetes,aes(x=BMI,y=BloodPressure,color=Outcome))+geom_jitter(alpha=0.6)+scale_color_gradient(low = 'red', high = 'blue')+labs(title="BP and BMI Corelation")


#decision tree algorithm
library(party)
# Give the chart file a name.
png(file = "dia_decision.png")
# Create the tree.
output.tree <- ctree(
  Outcome ~ Age + BMI + BloodPressure, 
  data = diabetes)
output.tree
# Plot the tree.
plot(output.tree)
dev.off()




#reference:http://ggplot2.tidyverse.org/reference/
#http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2