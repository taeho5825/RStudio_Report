Bias = read.csv("C:/DataScience/Bias_correction_ucl.csv")
Bias
head(Bias)

head(is.na(Bias))

str(Bias)

table(is.na(Bias)) #na값 존재
colSums(is.na(Bias)) # 결측값 확인

Bias[is.na(Bias)] <- 0 #결측값을 숫자0으로 변환
colSums(is.na(Bias)) # 결측값 확인

# mean(Bias$Present_Tmax) #29.49941
# mean(Bias$Present_Tmin) #23.01534
# mean(Bias$LDAPS_RHmin) #56.21023
# mean(Bias$LDAPS_RHmax) #87.51978
# mean(Bias$LDAPS_Tmax_lapse) #29.32694
# mean(Bias$LDAPS_Tmin_lapse) #23.28511
# mean(Bias$LDAPS_WS) #7.029203
# mean(Bias$LDAPS_LH) #61.90029
# mean(Bias$LDAPS_CC1) #0.3652057
# mean(Bias$LDAPS_CC2) #0.3526354
# mean(Bias$LDAPS_CC3) #0.3153234
# mean(Bias$LDAPS_CC4) #0.2962967
# mean(Bias$LDAPS_PPT1) #0.586267
# mean(Bias$LDAPS_PPT2) #0.4803102
# mean(Bias$LDAPS_PPT3) #0.2755081
# mean(Bias$LDAPS_PPT4) #0.2668009
# mean(Bias$LDAPS_RHmax) #87.51978
# mean(Bias$lat) #37.54472
# mean(Bias$lon) #126.9914
# mean(Bias$DEM) #61.86797
# mean(Bias$Slope) #1.257048
# mean(Bias$Next_Tmax) #30.16944
# mean(Bias$Next_Tmin) #22.85235
# mean(Bias$Solar.radiation) #5341.503

library(dplyr)

# temp1 = select(Bias, Present_Tmax, Present_Tmin, LDAPS_LH, LDAPS_PPT1, LDAPS_PPT2, LDAPS_PPT3, LDAPS_PPT4, Solar.radiation, Next_Tmax, Next_Tmin)
# temp2 = summarize(Bias, Present_T1=mean(Present_Tmax))
# temp3 = summarize(Bias, Present_T2=mean(Present_Tmin))
# temp4 = summarize(Bias, temp2, temp3)
# Present_T = (temp2+temp3)/2
# str(Present_T) #26.3
# 
# temp2 = summarize(Bias, LDAPS_PPT1=mean(LDAPS_PPT1))
# temp3 = summarize(Bias, LDAPS_PPT2=mean(LDAPS_PPT2))
# temp4 = summarize(Bias, LDAPS_PPT3=mean(LDAPS_PPT3))
# temp5 = summarize(Bias, LDAPS_PPT4=mean(LDAPS_PPT4))
# LDAPS_PPT = (temp2+temp3+temp4+temp5)/4
# str(LDAPS_PPT) #0.402
# 
# temp2 = summarize(Bias, Next_T1=mean(Next_Tmax))
# temp3 = summarize(Bias, Next_T2=mean(Next_Tmin))
# Next_T = (temp2+temp3)/2
# str(Next_T) #26.5
# 
# temp2 = summarize(Bias, LDAPS_RH=mean(LDAPS_RHmin))
# temp3 = summarize(Bias, LDAPS_RH2=mean(LDAPS_RHmax))
# LDAPS_RH=(temp2+temp3)/2
# str(LDAPS_RH) #71.9

tp <- (Bias$Present_Tmax + Bias$Present_Tmin)/2
tp1 <- (Bias$LDAPS_RHmax + Bias$LDAPS_RHmin)/2
tp2 <- (Bias$LDAPS_PPT1 + Bias$LDAPS_PPT2 + Bias$LDAPS_PPT3 + Bias$LDAPS_PPT4)/4
tp3 <-  (Bias$Next_Tmax + Bias$Next_Tmin)/2
temp <- cbind(Bias, tp, tp1, tp2, tp3)
str(temp)

temp1 = select(temp, Date, LDAPS_LH, Solar.radiation, tp, tp1, tp2, tp3)
str(temp1)

temp1 <- rename(temp1, "LDAPS_RH"="tp1", "LDAPS_PPT"="tp2", "Next_tp"="tp3")
str(temp1)
temp1 #LDAPS_LH, Solar.radiation, tp, LDAPS_RH, LDAPS_PPT, Next_tp

temp2 <- subset(temp1, LDAPS_RH>50 & LDAPS_RH<80)
temp3 <- subset(temp2, LDAPS_PPT > 0.3 & LDAPS_PPT < 23.7)
temp4 <- subset(temp3, Next_tp>20 & Next_tp<30)
final <- subset(temp4, tp>20 & tp<30)
final
str(final)
head(final,20)
colSums(is.na(final))

library(dplyr)
plot(final$tp, final$Next_tp, col=final$Next_tp, pch=c(1:2))
plot(final$LDAPS_RH, final$LDAPS_PPT, col=final$LDAPS_RH, pch=c(1:2))
plot(final$LDAPS_LH, final$Solar.radiation, col=final$LDAPS_LH, pch=c(1:2))

test1 = select(final, tp, Next_tp)
str(test1)
plot(test1, type="h", col=tp, main="final")

temperature = lm(tp ~ Next_tp, final)
coef(temperature)
abline(temperature, col='blue')

RH_PPT = lm(LDAPS_RH~LDAPS_PPT, final)
coef(RH_PPT)
abline(RH_PPT, col='green')

LH_Sol = lm(LDAPS_LH~Solar.radiation, final)
coef(LH_Sol)
abline(LH_Sol, col='red')

# f=glm(tp~.,data=final)
# coef(f)
# deviance(f)

# fitted(final)
# residuals(final)
#--------------------------------------------------------------
# plot(final$tp, final$Next_tp, col=final$Next_tp, pch=c(1:2))
# x=seq(0,25,length.out=200)
# for(i in 1:4){
#   m=lm(LDAPS_RH~poly(LDAPS_PPT, i), data=final)
#   assign(paste('m',i,sep = '.'),m)
#   lines(x,predict(m,data.frame(LDAPS_PPT=x)),col=i)
# }
# anova(m.1,m.2,m.3,m.4)

# m=lm(final$Next_tp~final$tp+final$LDAPS_PPT+final$LDAPS_LH)
# coef(m)
# library(scatterplot3d)
# a = final$tp
# b = final$LDAPS_PPT
# c = final$LDAPS_LH
# scatterplot3d(a,b,c, alim=2:7, blim=7:23, clim=0:10, pch=20, type='h')

# fitted(m)
# residuals(m)
# deviance(m)
# deviance(m)/length(m)

x <- final$tp
y <- final$LDAPS_PPT
cor.test(x,y)

plot(x,y, pch=c(1:2))

m <- lm(y~x)
summary(m)
abline(m, col="blue")

m <- lm(y~x+0)
summary(m)
abline(lm(y~x+0), col="red")

library(ggplot2)
ggplot(final, aes(x=tp, y=Next_tp, col=tp))+geom_point()+scale_x_log10()
ggplot(final, aes(x=LDAPS_RH, y=LDAPS_PPT, col=LDAPS_RH))+geom_point()+scale_x_log10()
ggplot(final, aes(x=LDAPS_LH, y=Solar.radiation, col=LDAPS_RH))+geom_point()+scale_x_log10()
