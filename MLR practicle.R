data=read.csv('C:/Users/Administrator/Desktop/Msc/MLR.csv',header = T,sep = ",")
View(data)
summary(data)

#MLR

Mlr=lm(data$Yi~data$Xi1+data$Xi2+data$Xi3+data$Xi4)
summary(Mlr)
#this give us MLR model 

#Backword elemenation 
back=lm(data$Yi~.,data = data)
back
step(back,direction = "backward")
summary(step(back,direction = "backward"))
#conclusion: AIC=30.5 is min when model is Yi~Xi1+Xi3+Xi4 with Rsq=96.98 and adjRseq=0.9597 


#Forward Selection 
forward=lm(data$Yi~1,data = data)
forward
forwardM=step(forward,direction = "forward",scope = formula(back))
summary(forwardM)
#concluasion: with forward selection method min AIC=28.9  when model is Yi~Xi2+Xi1 with Rsq=0.9685 adjRsq=0.9622

#Stepwise
library("olsrr")
StepR=lm(data$Yi~.,data = data)
step=ols_step_all_possible(StepR)
step
stp=ols_step_both_p(StepR,pent =0.2,prem = 0.3,details = T)
#Conculsion:final modle based on stepwise method is where Yi~Xi2+Xi1 

#final conculsion Based on forward and stepwise selection method best fitted model is Yi~Xi2+Xi1