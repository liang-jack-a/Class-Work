setwd("/Users/jackliang 1/Desktop/School Work/Metrics/Final Project")
library(stargazer)

##Normalizing; generating a histogram from the Stata data
##Recording coefficients
stars = c( -.4868701, -.0064052, -.252161, -.1427061, -.1601881, -.0598389, -.3594812, -.6811546,
           .2409661, .2407592, .4535859, -.5331279, -.6655232, .0497495, -.0370235, .1441357,
           -.3430178, .3030303, .2261072, .5673981, -.163872, -.9910873, -.3290043, -.0989305,
           -.5203528, -.141414, .2760695, .3571429, .3627787, -.3636364, -.1317523) + 3.86363

num = c( 3680, 809, 305, 1634, 1305, 790, 361, 137, 239, 182, 135, 59, 106, 127, 124, 386, 97,
         57, 78, 29, 423, 255, 231, 34, 201, 54, 68, 77, 53, 55, 69)

names = c("restaurant", "spa", "auto", "shopping", "small food", "medical", "localserv",
          "hotel", "arts", "education", "event planning", "nightlife" , "financial",
          "home services", "pets", "active life", "public service", "religious", 
          "professional services", "local flavour", "restaurant_smallfood", "homeserv_realestate",
          "bar_nightlife", "smallfood_shopping", "eventplanning_hotel", "localserv_shopping",
          "arts_nightlife", "arts_shopping", "medical_spa", 
          "restaruant_eventplanning", "shopping_spa")

df = data.frame(names, stars, num)

df = df[rev(order(stars)),]

par(mar=c(3,11,2,4))
bplt = barplot(df$stars, horiz = T, las = 1, col = c("black", "black", "black", "black", "white",
                                                     "white", "white", "white", "white", "white",
                                                     "white", "white", "white", "white", "white",
                                                     "white", "white", "white", "white", "white",
                                                     "white", "black", "black", "black", "black",
                                                     "black", "black", "black", "black", "black",
                                                     "black", "black"), 
               main = "'Average' Ratings of Categories",names.arg = df$names) 

lines(x = rep( 3.623812, 51), y = 0:50)

text(x= df$stars+.5, y= bplt, labels=as.character(df$num), xpd=TRUE)




##Distribution of One Time reviewers
onetime = read.csv("one_time_users.csv", header = TRUE)
onetime = onetime[complete.cases(onetime), ]


all_rev = read.csv("normalize_table.csv", header = TRUE)
all_rev = all_rev[complete.cases(all_rev), ]


points = seq(-3.5,2.5,by=.5)
##Histograms of normalized data
hist(onetime[,33], breaks = points, main = "One Time Reviews, Normalized", xlab = "Normalized Review")
hist(all_rev[,32], breaks = points, main = "All Reviews, Normalized", xlab = "Normalized Review")

##Using Kolmogorov-Smirnov
stats::ks.test(onetime[,33], all_rev[,32])


##Histogram of un-normalized data
##The distribution of this is [reasonably] similar to the normalized data, surprisingly
hist(all_rev[,7], breaks = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5), xlab = "Review", main = "All Reviews, Unnormalized")
hist(onetime[,8], breaks = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5), xlab = "Review", main = "One Time Reviews, Unnormalized")

##out of curiosity, running Kolmogorov-Smirnov on unnormalized data. Get very similar result
stats::ks.test(all_rev[,7], onetime[,8])


##Looking at pre and post April 2010- effect of lessening the cost to sign up
pre_onetime = read.csv("pre_one_time_users.csv", header = TRUE)
pre_onetime = pre_onetime[complete.cases(pre_onetime), ]

post_onetime = read.csv("post_one_time_users.csv", header = TRUE)
post_onetime = post_onetime[complete.cases(post_onetime), ]

hist(pre_onetime[,33], breaks = points, main = "One Time Reviews, Pre Apr 2010", xlab = "Normalized Review")
hist(post_onetime[,33], breaks = points, main = "One Time Reviews, Post Apr 2010", xlab = "Normalized Review")

##Kolmogorov-Smirnov- results were not significant
stats::ks.test(pre_onetime[,33], post_onetime[,33])







##Effect of Prior reviews
pre2012rest = read.csv("regress_data_2012.csv", header = TRUE)
pre2012rest[is.na(pre2012rest)] <- 0

##Regressing the difference between normalized mean in 2012 and normalized mean prior to 2012
## on number of reviews pre 2012, only for people with a non-zero number of reviews in 2012 and prior to 2012
pre2012rest = pre2012rest[pre2012rest[,11] > 0 & pre2012rest[,5] > 0, ]

num_rev_pre2012 = pre2012rest[,5]

diff_total12 = pre2012rest[,10] - pre2012rest[,7]
reg1 = lm(diff_total12 ~ num_rev_pre2012)
summary(reg1)
stargazer(reg1)


##Regressing the difference between restaurant mean in 2012 and restaurant mean prior to 2012
##on the number of restaurant reviews pre 2012, only for people with a non- zero number of 
##restaurant reviews in 2012 and prior to 2012
pre2012rest = pre2012rest[pre2012rest[,12] > 0 & pre2012rest[,6] > 0, ]
diff_rest12 = pre2012rest[,3] - pre2012rest[,8]

num_rev_rest_pre2012 = pre2012rest[,6]
reg2 = lm(diff_rest12 ~ num_rev_rest_pre2012)
summary(reg2)
stargazer(reg2)
##We get a [positive] significant result at the 5 percent level for both regressions



##Trying for a different year, we use 2011 as our baseline now
pre2011rest = read.csv("regress_data_2011.csv", header = TRUE)
pre2011rest[is.na(pre2011rest)] <- 0

##Regressing the difference between normalized mean in 2011 and normalized mean prior to 2011
## on number of reviews pre 2011, only for people with a non-zero number of reviews in 2011 and prior to 2011
pre2011rest = pre2011rest[pre2011rest[,11] > 0 & pre2011rest[,5] > 0, ]

num_rev_pre2011 = pre2011rest[,5]

diff_total11 = pre2011rest[,10] - pre2011rest[,7]
reg1 = lm(diff_total11 ~ num_rev_pre2011)
summary(reg1)
stargazer(reg1)

##Removing users with no restaurant reviews prior to 2011
pre2011rest = pre2011rest[pre2011rest[,12] > 0 & pre2011rest[,13] > 0, ]
##Regressing the difference between restaurant mean in 2011 and restaurant mean prior to 2011
##on the number of restaurant reviews pre 2011
diff_rest11 = pre2011rest[,3] - pre2011rest[,8]

num_rev_rest_pre2011 = pre2011rest[,13]

reg2 = lm(diff_rest11 ~ num_rev_rest_pre2011)
summary(reg2)
stargazer(reg2)



##Trying for a third year, we use 2010 as our baseline now. 
##The code is identical to above with a few dates changed.
pre2010rest = read.csv("regress_data_2010.csv", header = TRUE)
pre2010rest[is.na(pre2010rest)] <- 0

##Regressing the difference between normalized mean in 2010 and normalized mean prior to 2010
## on number of reviews pre 2010, only for people with a non-zero number of reviews in 2010 and prior to 2010
pre2010rest = pre2010rest[pre2010rest[,11] > 0 & pre2010rest[,5] > 0, ]

num_rev_pre2010 = pre2010rest[,5]

diff_total10 = pre2010rest[,10] - pre2010rest[,7]
reg1 = lm(diff_total10 ~ num_rev_pre2010)
summary(reg1)
stargazer(reg1)

##Removing users with no restaurant reviews prior to 2010
pre2010rest = pre2010rest[pre2010rest[,12] > 0 & pre2010rest[,13] > 0, ]
##Regressing the difference between restaurant mean in 2010 and restaurant mean prior to 2010
##on the number of restaurant reviews pre 2010 only for people with a 
##non-zero number of reviews in 2010 and prior to 2010
diff_rest10 = pre2010rest[,3] - pre2010rest[,8]

num_rev_rest_pre2010 = pre2010rest[,13]


reg2 = lm(diff_rest10 ~ num_rev_rest_pre2010)
summary(reg2)
stargazer(reg2)

