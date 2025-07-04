



#Exploring the data
library(psych)
library(car)
library(corrplot)
library(gplots)
library(lattice)

ecomm.df<- read.csv("https://goo.gl/hzRyFd")
ecomm.df

#Exploring the data
str(ecomm.df)
describe(ecomm.df)
summary(ecomm.df)
pairs(ecomm.df)


ecomm.df[,c(2:4,25,34,37)]<-lapply(ecomm.df[,c(2:4,25,34,37)], factor)


table(ecomm.df$behavPageviews)

ecomm.df$pageViewInt <- ecomm.df$behavPageviews

ecomm.df$pageViewInt [ecomm.df$pageViewInt=="10+"]<-10
ecomm.df$pageViewInt [ecomm.df$pageViewInt=="2 to 3"]<-2
ecomm.df$pageViewInt [ecomm.df$pageViewInt=="4 to 6"]<-4
ecomm.df$pageViewInt [ecomm.df$pageViewInt=="7 to 9"]<-7
ecomm.df$pageViewInt <- as.integer(ecomm.df$pageViewInt )

table(ecomm.df$pageViewInt)

#1 Describe number of page views by profile
table(ecomm.df$profile)

aggregate(pageViewInt~profile,data=ecomm.df, summary)

aggregate(pageViewInt~profile,data=ecomm.df, mean)


aggregate(ecomm.df$pageViewInt,list(ecomm.df$profile), mean)
aggregate(ecomm.df$pageViewInt,list(ecomm.df$profile), median)
aggregate(ecomm.df$pageViewInt,list(ecomm.df$profile), sd)
aggregate(ecomm.df$pageViewInt,list(ecomm.df$profile), IQR)
aggregate(ecomm.df$pageViewInt,list(ecomm.df$profile), max)
aggregate(ecomm.df$pageViewInt,list(ecomm.df$profile), min)

# 2 the same using for

for(i in unique( ecomm.df$profile)) {
  print(i)
  print(summary(ecomm.df$pageViewInt[ecomm.df$profile==i]))}


# 3 I prefer the first approach because is shorter to write and I am more familiar with it, 
#and you get it in a dataframe, which you can export. 
#the for approach, maybe when you want more complex functions applied? I do not know

# 4 proportions of male and women within each profile
table(ecomm.df$profile, ecomm.df$gender)
table
table.clean <- table[,c(2,3)]
table.clean
prop.table (table.clean, margin=1)

with(ecomm.df[ecomm.df$gender=="Female" | ecomm.df$gender=="Male", ], 
     prop.table(table(profile, gender), margin=1))

#5 which profile has done more purchases

describe(ecomm.df)

table(ecomm.df$purchaseBefore)
ecomm.df$purchaseBefore
table(ecomm.df$profile, ecomm.df$purchasedWhen)
prop<-prop.table(table( ecomm.df$purchasedWhen, ecomm.df$profile), margin =2)
prop
prop.clean <- prop[c(2:3),]
prop.clean
result<-colSums(prop.clean)
result2<-colSums(prop[c(2:3),])
result3 <- prop[2,]+prop[3,]
result
result2
result3   
barchart(result)


#another way
with(ecomm.df, table(profile, purchasedWhen))
histogram(~profile | purchasedWhen, data=ecomm.df, scales=list(x=list(rot=45)))
            
# for a business quesiton that counts would be more beneficial, an example is: 
#from which profiles we are receiving more sales in the past few weeks
# a business quesiton in which percentages of customers buying frequently within each profile is: 
#which profile of customers tended to buy more recently?
#how is the tendency to buy frequently within each profile? 


#7 Which profile of role-profile and gender had the higher number of purchases?
with(ecomm.df, table(behavAnySale, profile, gender))
with(ecomm.df[ecomm.df$gender == "Female" | ecomm.df$gender == "Male", ], table(behavAnySale, profile, gender))

#female parents are the ones who completed a purchase

prop.table(with(ecomm.df, table(behavAnySale, profile, gender)), margin=2:3)
prop<-prop.table(with(ecomm.df[ecomm.df$gender == "Female" | ecomm.df$gender == "Male", ], table(behavAnySale, profile, gender)), margin=2:3)
prop.clean <- prop [, ,c(2:3)]
prop.clean
# it is difficult to say which combination profile of gender and role-profile has more inclination to buy, because data is very limited to many categories
#but within those in which there is acceptable number of data, parents, other, and female teachers, it looks like male parents are the most inclined to complete sales

#the way is solved with the book is by looking at the means and sums

aggregate(behavAnySale~profile+gender,data=ecomm.df, sum)
aggregate(behavAnySale~profile+gender, data=ecomm.df, mean)
