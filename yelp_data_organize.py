import pandas as pd

review_data = pd.read_csv('review_data.csv')
business_data_with_dummies = pd.read_csv('business_data_with_dummies.csv')

biz_data = business_data_with_dummies[['business_id', 'Restaurants', 'Food', 'Event Planning & Services', 'Beauty and Spas', "Health and Medical", 
                                      'Shopping', 'Automotive', 'Local Services', 'Arts & Entertainment', 'Nightlife',
                                      'Hotels & Travel', 'Financial Services', 'Active Life', 'Pets', "Home Services", "Religious Organizations", "Real Estate",
                                      "Bars", "Education", 'Public Services & Government','Professional Services', 'Local Flavor']]

dummies = ['Restaurants', 'Food', 'Event Planning & Services', 'Beauty and Spas', "Health and Medical", 
                                      'Shopping', 'Automotive', 'Local Services', 'Arts & Entertainment', 'Nightlife',
                                      'Hotels & Travel', 'Financial Services', 'Active Life', 'Pets', "Home Services", "Religious Organizations", "Real Estate",
                                      "Bars", "Education", 'Public Services & Government','Professional Services', 'Local Flavor']

normalize_table = pd.merge(review_data, biz_data, on='business_id')
normalize_table['sum'] = sum([normalize_table[i] for i in dummies])
#should probably perform this over all review_data instead

ind_average = {"restaurant":3.376760, "spa":3.857225, "auto":3.611469, "shopping":3.720924, "small_food":3.703442, "medical":3.803791, "localserv":3.504149,
                 "hotel":3.182475, "arts":4.104596, "education":4.104389, "eventplanning":4.317216, "nightlife":3.330502 , "financial":3.198107,
                 "home_services":3.913380, "pets":3.826607, "active_life":4.007766, "public_service":3.520612, "religious":4.166660, 
                 "professional_services":4.089737, "local_flavour":4.431028, "restaurant_smallfood":3.699758, "homeserv_realestate":2.872543,
                 "bar_nightlife":3.534626, "smallfood_shopping":3.764700, "eventplanning_hotel":3.343277, "localserv_shopping":3.722216,
                 "arts_nightlife":4.139699, "arts_shopping":4.220773, "medical_spa":4.226409, 
                 "restaruant_eventplanning":3.499994, "shopping_spa":3.731878}

rest_avg = ind_average['restaurant']
for row in normalize_table[(normalize_table['Restaurants'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - rest_avg

#spa_data = normalize_table.loc[(normalize_table['Beauty and Spas'] == 1) & (normalize_table['Health and Medical'] == 0) & (normalize_table['Shopping'] == 0)]

spa_avg = ind_average['spa']
for row in normalize_table[(normalize_table['Beauty and Spas'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - spa_avg

#auto_data = normalize_table.loc[normalize_table['Automotive'] == 1]

auto_avg = ind_average['auto']
for row in normalize_table[(normalize_table['Automotive'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - auto_avg

#shopping_data = normalize_table.loc[(normalize_table['Shopping'] == 1) & (normalize_table['Food'] == 0) & (normalize_table['Local Services'] == 0) & (normalize_table['Arts & Entertainment'] == 0) & (normalize_table['Beauty and Spas'] == 0)]

shop_avg = ind_average['shopping']
for row in normalize_table[(normalize_table['Shopping'] == 1) & (normalize_table['sum'] == 1) ].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - shop_avg    

#small_food_data = normalize_table.loc[(normalize_table['Food'] == 1) & (normalize_table['Shopping'] == 0) & (normalize_table['Restaurants'] == 0)]

small_food_avg = ind_average['small_food']
for row in normalize_table[(normalize_table['Food'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - small_food_avg


#medical_data = normalize_table.loc[(normalize_table['Health and Medical'] == 1) & (normalize_table['Beauty and Spas'] == 0)]

medical_avg = ind_average['medical']
for row in normalize_table[(normalize_table['Health and Medical'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - medical_avg

#localserv_data = normalize_table.loc[(normalize_table['Local Services'] == 1) & (normalize_table['Shopping'] == 0)]

localserv_avg = ind_average['localserv']
for row in normalize_table[(normalize_table['Local Services'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - localserv_avg


#hotel_data = normalize_table.loc[(normalize_table['Hotels & Travel'] == 1) & (normalize_table['Event Planning & Services'] == 0)]

hotel_avg = ind_average['hotel']
for row in normalize_table[(normalize_table['Hotels & Travel'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - hotel_avg


#arts_data = normalize_table.loc[(normalize_table['Arts & Entertainment'] == 1) & (normalize_table['Shopping'] == 0) & (normalize_table['NightLife'] == 0)]

arts_avg = ind_average['arts']
for row in normalize_table[(normalize_table['Arts & Entertainment'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - arts_avg

#education_data = normalize_table.loc[normalize_table['Education'] == 1]

edu_avg = ind_average['education']
for row in normalize_table[(normalize_table['Education'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - edu_avg

#event_planning_data = normalize_table.loc[(normalize_table['Event Planning & Services'] == 1) & (normalize_table['Hotels & Travel'] == 0) & (normalize_table['Restaurants'] == 0)]

event_planning_avg = ind_average['eventplanning']
for row in normalize_table[(normalize_table['Event Planning & Services'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - event_planning_avg

#nightlife_data = normalize_table.loc[(normalize_table['NightLife'] == 1) & (normalize_table['Arts & Entertainment'] == 0) & (normalize_table['Bars'] == 0)]

nightlife_avg = ind_average['nightlife']
for row in normalize_table[(normalize_table['Nightlife'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - nightlife_avg

#financial_data = normalize_table.loc[normalize_table['Financial Services'] == 1]

fin_avg = ind_average['financial']
for row in normalize_table[(normalize_table['Financial Services'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - fin_avg

#home_services_data = normalize_table.loc[(normalize_table['Home Services'] == 1) & (normalize_table['Real Estate'] == 0)]

home_serv_avg = ind_average['home_services']
for row in normalize_table[(normalize_table['Home Services'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - home_serv_avg

#pets_data = normalize_table.loc[normalize_table['Pets'] == 1]

pets_avg = ind_average['pets']
for row in normalize_table[(normalize_table['Pets'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - pets_avg

#active_life_data = normalize_table.loc[normalize_table['Active Life'] == 1]

active_avg = ind_average['active_life']
for row in normalize_table[(normalize_table['Active Life'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - active_avg

#public_service_data = normalize_table.loc[normalize_table['Public Services & Government'] == 1]

public_avg = ind_average['public_service']
for row in normalize_table[(normalize_table['Public Services & Government'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - public_avg

#religious_data = normalize_table.loc[normalize_table['Religious Organizations'] == 1]

religious_avg = ind_average['religious']
for row in normalize_table[(normalize_table['Religious Organizations'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - religious_avg

#professional_services_data = normalize_table.loc[normalize_table['Professional Services'] == 1]

professional_avg = ind_average['professional_services']
for row in normalize_table[(normalize_table['Professional Services'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - professional_avg

#local_flavour_data = normalize_table.loc[normalize_table['Local Flavor'] == 1]

local_avg = ind_average['local_flavour']
for row in normalize_table[(normalize_table['Local Flavor'] == 1) & (normalize_table['sum'] == 1)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - local_avg

#restaurant_smallfood_data = normalize_table.loc[(normalize_table['Food'] == 1) & (normalize_table['Restaurants'] == 1)]

rest_small_avg = ind_average['restaurant_smallfood']
for row in normalize_table[(normalize_table['Food'] == 1) & (normalize_table['Restaurants'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - rest_small_avg

#homeserv_realestate_data = normalize_table.loc[(normalize_table['Home Services'] == 1) & (normalize_table['Real Estate'] == 1)]

home_real_avg = ind_average['homeserv_realestate']
for row in normalize_table[(normalize_table['Home Services'] == 1) & (normalize_table['Real Estate'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - home_real_avg

#bar_nightlife_data = normalize_table.loc[(normalize_table['Bars'] == 1) & (normalize_table['NightLife'] == 1)]

bar_night_avg = ind_average['bar_nightlife']
for row in normalize_table[(normalize_table['Bars'] == 1) & (normalize_table['Nightlife'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - bar_night_avg

#smallfood_shopping_data = normalize_table.loc[(normalize_table['Food'] == 1) & (normalize_table['Shopping'] == 1)]

small_shop_avg = ind_average['smallfood_shopping']
for row in normalize_table[(normalize_table['Food'] == 1) & (normalize_table['Shopping'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - small_shop_avg


#eventplanning_hotel_data = normalize_table.loc[(normalize_table['Event Planning & Services'] == 1) & (normalize_table['Hotels & Travel'] == 1)]

event_hotel_avg = ind_average['eventplanning_hotel']
for row in normalize_table[(normalize_table['Event Planning & Services'] == 1) & (normalize_table['Hotels & Travel'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - event_hotel_avg


#localserv_shopping_data = normalize_table.loc[(normalize_table['Local Services'] == 1) & (normalize_table['Shopping'] == 1)]

local_shop_avg = ind_average['localserv_shopping']
for row in normalize_table[(normalize_table['Local Services'] == 1) & (normalize_table['Shopping'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - local_shop_avg

#arts_nightlife_data = normalize_table.loc[(normalize_table['Arts & Entertainment'] == 1) & (normalize_table['NightLife'] == 1)]

arts_night_avg = ind_average['arts_nightlife']
for row in normalize_table[(normalize_table['Arts & Entertainment'] == 1) & (normalize_table['Nightlife'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating']= normalize_table.loc[row]['stars'] - arts_night_avg


#arts_shopping_data = normalize_table.loc[(normalize_table['Arts & Entertainment'] == 1) & (normalize_table['Shopping'] == 1)]

arts_shop_avg = ind_average['arts_shopping']
for row in normalize_table[(normalize_table['Arts & Entertainment'] == 1) & (normalize_table['Shopping'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating']= normalize_table.loc[row]['stars'] - arts_shop_avg

#medical_spa_data = normalize_table.loc[(normalize_table['Health and Medical'] == 1) & (normalize_table['Beauty and Spas'] == 1)]

med_spa_avg = ind_average['medical_spa']
for row in normalize_table[(normalize_table['Health and Medical'] == 1) & (normalize_table['Beauty and Spas'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating']= normalize_table.loc[row]['stars'] - med_spa_avg

#restaruant_eventplanning_data = normalize_table.loc[(normalize_table['Restaurants'] == 1) & (normalize_table['Event Planning & Services'] == 1)]

rest_event_avg = ind_average['restaruant_eventplanning']
for row in normalize_table[(normalize_table['Restaurants'] == 1) & (normalize_table['Event Planning & Services'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - rest_event_avg

#shopping_spa_data = normalize_table.loc[(normalize_table['Shopping'] == 1) & (normalize_table['Beauty and Spas'] == 1)]

shop_spa_avg = ind_average['shopping_spa']
for row in normalize_table[(normalize_table['Shopping'] == 1) & (normalize_table['Beauty and Spas'] == 1) & (normalize_table['sum'] == 2)].index.values:
    normalize_table.loc[row]['normalized_rating'] = normalize_table.loc[row]['stars'] - shop_spa_avg

normalize_table.to_csv('normalize_table.csv')
