library(tidyverse)
library(mice)
library(gee)
library(gtsummary)
library(emmeans)
library(ggeffects)
library(geepack)
library(table1)

'https://rpubs.com/izf381'
library(tidyverse)
# Loading data
w1 = da21600.0001 %>% dplyr::select(H1GI1Y,AID,BIO_SEX,H1GI4,H1GI6A,H1GI6B,H1GI6C,H1GI6D,H1GI6E,H1GH59A,H1GH59B,H1GH60,PA12,PA55,H1WP7)
w2 = da21600.0022
w3 = da21600.0032

# weights 
ww = da21600.0042 %>% dplyr::select(AID,GSW145)

plote = function(x){
  emmeans(x, specs = c("wave", "autonomy"), 
          at = list(diagnose = "obs"), 
          cov.keep = "wave", 
          regrid = "response") %>% 
    as.data.frame() %>% ggplot() +
    aes(x = wave, y = prob, color = autonomy, fill = autonomy) +
    geom_line() +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, 
                    color = NULL), alpha = 0.15) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1", guide = NULL) +
    scale_y_continuous(labels = scales::percent) +
    theme_ggeffects() +
    labs(title = "GEE Effect plot", y = "obesity")
}




# Cleaning & Recoding

w1$H1GI6A = as.numeric(w1$H1GI6A)- 1
w1$H1GI6B = as.numeric(w1$H1GI6B)- 1
w1$H1GI6C = as.numeric(w1$H1GI6C)- 1
w1$H1GI6D = as.numeric(w1$H1GI6D)- 1
w1$H1GI6E = as.numeric(w1$H1GI6E)- 1
w1$w1age = 21 - as.integer(w1$H1GI1Y) 
w1$racecount = w1$H1GI6A+w1$H1GI6B+w1$H1GI6C+w1$H1GI6D+w1$H1GI6E
w1$RACE = ifelse(as.numeric(w1$H1GI6A) == "1","White",NA)
w1$RACE = ifelse(as.numeric(w1$H1GI6B) == "1","Black",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI6C) == "1","Indian",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI6D) == "1","Asian",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI6E) == "1","Other",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$racecount) > 1,"Multi-Race",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI4) == "2","Hispanic",w1$RACE)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) < 4,"Less than High School",NA)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) %in% c(4,5),"High School Graduate",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) %in% c(6,7),"Some College",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) == 8 ,"College Graduate",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) == 9 ,"College Graduate +",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) == 10 ,"Less than High School",w1$PAEDUC)
w1$EDUC1 = "Less than High School"
w1$w1HI = ifelse(w1$PA55<75,"Less than $75000",NA)
w1$w1HI = ifelse(w1$PA55<100 & w1$PA55>75,"$75000 - $99999",w1$w1HI)
w1$w1HI = ifelse(w1$PA55>100,"$100000 and over",w1$w1HI)




w2$w2HI = ifelse(as.numeric(w2$H4EC1)<10,"Less than $75000",NA)
w2$w2HI = ifelse(as.numeric(w2$H4EC1) == 10,"$75000 - $99999",w2$w2HI)
w2$w2HI = ifelse(as.numeric(w2$H4EC1) %in% c(11,12),"$100000 and over",w2$w2HI)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2)<3,"Less than High School",NA)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2)==3,"High School Graduate",w2$EDUC2)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2)%in%c(4,5,6),"Some College",w2$EDUC2)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2) == 7,"College Graduate",w2$EDUC2)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2) %in% c(8,9,10,11,12,13),"College Graduate +",w2$EDUC2)
w2$H4DA5 = as.numeric(w2$H4DA5)






w3$w3HI = ifelse(as.numeric(w3$H5EC2)<10,"Less than $75000",NA)
w3$w3HI = ifelse(as.numeric(w3$H5EC2) == 10,"$75000 - $99999",w3$w3HI)
w3$w3HI = ifelse(as.numeric(w3$H5EC2) %in% c(11,12),"$100000 and over",w3$w3HI)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) == 2, "Less than High School",NA)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) %in% c(3,4), "Less than High School",w3$EDUC3)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) %in% c(5,6,7,8,9), "Some College",w3$EDUC3)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) ==10, "College Graduate",w3$EDUC3)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) %in% c(11,12,13,14,15,16), "College Graduate +",w3$EDUC3)
w3$H5ID27 = as.numeric(w3$H5ID27)



ready = ww %>% merge(w3,by="AID") %>% merge(w2,by = "AID") %>% merge(w1,by ="AID")  %>% 
  dplyr::select(AID,H1GH60,H1GH59A,H1GH59B,H4GH5F,H4GH5I,H4GH6,H5ID2F,H5ID2I,H5ID3,PAEDUC,RACE,BIO_SEX,EDUC1,EDUC2,EDUC3,H4DA5,H5ID27,H1WP7,w1HI,w2HI,w3HI,GSW145,w1age)


ready$AID = as.numeric(ready$AID)


ready$w2age = ready$w1age+14
ready$w3age = ready$w2age + 8


ready$H4DA5 <- as.numeric(ready$H4DA5)
ready$H5ID27 <- as.numeric(ready$H5ID27)

str(ready)

ready$w1HI = as.factor(ready$w1HI)
ready$w2HI=as.factor(ready$w2HI)
ready$w3HI=as.factor(ready$w3HI)
ready$EDUC1=as.factor(ready$EDUC1)
ready$EDUC2=as.factor(ready$EDUC2
                      )
ready$EDUC3=as.factor(ready$EDUC3)
ready$PAEDUC = as.factor(ready$PAEDUC)
ready$RACE = as.factor(ready$RACE)
levels(ready$w1HI) = c("Less than $75000","$75000 - $99999","$100000 and over")
levels(ready$w2HI)= c("Less than $75000","$75000 - $99999","$100000 and over")
levels(ready$w3HI)= c("Less than $75000","$75000 - $99999","$100000 and over")
levels(ready$PAEDUC) = c("Less than High School", "High School Graduate","Some College","College Graduate","College Graduate +")
levels(ready$EDUC1) = c("Less than High School", "High School Graduate","Some College","College Graduate","College Graduate +")
levels(ready$EDUC2) = c("Less than High School", "High School Graduate","Some College","College Graduate","College Graduate +")
levels(ready$EDUC3) = c("Less than High School","Some College","College Graduate","College Graduate +")




ready$BIO_SEX = as.factor(ifelse(ready$BIO_SEX == '(1) (1) Male',"Male","Famale"))
colnames(ready)[17] = "w2spt"
colnames(ready)[18] = 'w3spt'
colnames(ready)[19] = 'autonomy'
ready$autonomy = as.factor(ifelse(ready$autonomy == "(0) (0) No","No","Yes"))
ready$H1GH59A = as.numeric(ready$H1GH59A)+3

ready$H1GH59B = as.numeric(ready$H1GH59B)-1
ready$H5ID2F = ifelse(ready$H5ID2F > 8,NA,ready$H5ID2F)
ready$H5ID2I = ifelse(ready$H5ID2I > 12,NA,ready$H5ID2I)



# Imputatio & Format Change
library(mice)
ini = mice(ready,seed = 1,m = 3)
pred1 = ini$predictorMatrix
pred1[,'AID'] = 0
ready = complete(mice(ready,seed = 1,pred = pred1,m = 3))






# Construction of BMI & Obesity

ready$w1obs = 
  
  
  
  (703*(ready$H1GH60)/
     
  ((
    ((as.integer(ready$H1GH59A))*12) + ((as.numeric(ready$H1GH59B))))^2))


ready$w2obs = 
  
  
  
  (703*(ready$H4GH6)/
     
     ((
       ((as.integer(ready$H4GH5F))*12) + ((as.numeric(ready$H4GH5I))))^2))

ready$w3obs = 
  
  
  
  (703*(ready$H5ID3)/
     
     ((
       ((as.integer(ready$H5ID2F))*12) + ((as.numeric(ready$H5ID2I))))^2))

ready %>% filter(w2obs <= 10)
# Removing duplicates & unnecesarries

ready = ready %>% dplyr::select(-H1GH60,-H1GH59A,-H1GH59B,-H4GH5F,-H4GH5I,-H4GH6,-H5ID2F,-H5ID2I,-H5ID3,-GSW145)

# Format Change



ready$baselineHI = ready$w1HI
tbl1 = ready
table1(~w1obs+w2obs+w3obs|autonomy+baselineHI,data = tbl1)
colnames(tbl1)[2] = "Parent's Education Level"
colnames(tbl1)[5] = "Participants Education Wave I"
colnames(tbl1)[6] = "Participants Education Wave IV"
colnames(tbl1)[7] = "Participants Education Wave V"
colnames(tbl1)[8] = "Participants Sports Level Wave IV"
colnames(tbl1)[9] = "Participants Sports Level Wave V"
colnames(tbl1)[11] = "Household Income Wave I"
colnames(tbl1)[12] = "Household Income Wave IV"
colnames(tbl1)[13] = "Household Income Wave V"
colnames(tbl1)[17] = "BMI Wave I"
colnames(tbl1)[18] = "BMI Wave IV"
colnames(tbl1)[19] = "BMI Wave V"
colnames(tbl1)[14] = "Age Wave I"
colnames(tbl1)[15] = "Age Wave IV"
colnames(tbl1)[16] = "Age Wave V"
colnames(tbl1)[4] = "Sex"
colnames(tbl1)[3] = "Race"
colnames(tbl1)[10] = "Autonomy"

table1(~.-AID|baselineHI,data = tbl1)



long = ready %>% pivot_longer(cols = c(EDUC1,EDUC2,EDUC3),values_to
                              = 'EDUC')


long$HI = NA
for(i in seq(0,11129,3)){
  long$HI[i] = long$w3HI[i]
}

for(i in seq(2,11129,3)){
  long$HI[i] = long$w2HI[i]
}

for(i in seq(1,11129,3)){
  long$HI[i] = long$w1HI[i]
}

long$obs = NA
for(i in seq(0,11129,3)){
  long$obs[i] = long$w3obs[i]
}

for(i in seq(2,11129,3)){
  long$obs[i] = long$w2obs[i]
}

for(i in seq(1,11129,3)){
  long$obs[i] = long$w1obs[i]
}


long$HI = as.factor(
  ifelse(long$HI == 1,"Less than $75000", 
         ifelse(
           long$HI == 2, "$75000 - $99999","$100000 and over"
         ))
)

long$age = NA
for(i in seq(0,11129,3)){
  long$age[i] = long$w3age[i]
}

for(i in seq(2,11129,3)){
  long$age[i] = long$w2age[i]
}

for(i in seq(1,11129,3)){
  long$age[i] = long$w1age[i]
}


long = long %>% dplyr::select(-w1obs,-w2obs,-w3obs,-w1HI,-w2HI,-w3HI,-name,-w1age,-w2age,-w3age)
long$wave = rep(c(1,2,3),3713)







ready %>% ggplot(aes(baselineHI)) +geom_bar(aes(fill = autonomy),position = "fill")






long$AID = as.factor(long$AID)

write.csv(long,"tt.csv") # These codes are here because I could not remove a unknown error that was possibily due to some format issue...
long = read.csv("tt.csv")












# Analysis
library(geepack)
high = long %>% filter(baselineHI == "$100000 and over")

hightbl1 = geeglm(obs~age+PAEDUC+RACE+BIO_SEX+w2spt+w3spt+autonomy*age+EDUC,corstr = "ar1",id = AID,family = gaussian,data = high)






mid = long %>% filter(baselineHI == "$75000 - $99999") 

midtbl = geeglm(obs~age+PAEDUC+RACE+BIO_SEX+w2spt+w3spt+autonomy*age+EDUC,corstr = 'ar1',id = AID,data = mid,family='gaussian')



low = long %>% filter(baselineHI == "Less than $75000")

lowtbl = geeglm(obs~age+PAEDUC+RACE+BIO_SEX+w2spt+w3spt+autonomy*age+EDUC,corstr="ar1",id = AID,data = low,family='gaussian')






### Out puts 




tbl_regression(hightbl1)




tbl_regression(midtbl)




tbl_regression(lowtbl)
lowg = data.frame(age = c(12:40), bmi = c(20.40816844 + 0.279 * 12:40 + 1.8,20.40816844 + 0.279 * 12:40),auto = c("YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","YES","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO","NO"))


lowg %>% ggplot(aes(x = age)) + geom_smooth(aes(y = bmi,color = auto))


library(sjPlot)
library(sjmisc)
library(sjlabelled)

allsample = geeglm(obs~age+PAEDUC+RACE+BIO_SEX+w2spt+w3spt+autonomy*age+EDUC,corstr = "ar1",id = AID,family = gaussian,data = long)


summary(allsample)
