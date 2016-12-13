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

