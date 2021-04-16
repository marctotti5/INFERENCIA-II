

library(tidyverse)

setwd("C:/Users/marct/Documents/CURSOS/PROJECTES/AIRBNB PRICE PREDICTION")

airbnb_data <- read.csv("./DATA/barcelona.csv") %>% select(c("host_since", "host_response_time", 
                                                                             "host_response_rate", "host_acceptance_rate", "host_is_superhost", 
                                                                             "host_neighbourhood", "host_listings_count", "host_total_listings_count",
                                                                             "host_verifications", "host_has_profile_pic", "host_identity_verified",
                                                                             "neighbourhood", "city", "state",
                                                                             "smart_location", "country", "latitude", "longitude",
                                                                             "is_location_exact", "property_type", "room_type", "accommodates", "bathrooms", 
                                                                             "bedrooms", "beds", "bed_type", "amenities", "square_feet", "price", "security_deposit",
                                                                             "cleaning_fee", "guests_included", "extra_people", "minimum_nights", "maximum_maximum_nights",
                                                                             "has_availability", "availability_30", "availability_60", "availability_90", "availability_365",
                                                                             "number_of_reviews", "first_review", "last_review", "review_scores_rating", "review_scores_accuracy",
                                                                             "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication", "review_scores_value",
                                                                             "requires_license", "instant_bookable", "is_business_travel_ready", "cancellation_policy",
                                                                             "require_guest_profile_picture", "require_guest_phone_verification", "calculated_host_listings_count",
                                                                             "calculated_host_listings_count_entire_homes", "calculated_host_listings_count_private_rooms",
                                                                             "calculated_host_listings_count_shared_rooms", "reviews_per_month"))
library(lubridate)
airbnb_data$host_since <- ymd(airbnb_data$host_since)
airbnb_data$host_since_year <- year(airbnb_data$host_since)
airbnb_data <- airbnb_data[, c(1, ncol(airbnb_data), 2 : (ncol(airbnb_data) - 1))] 

airbnb_data$neighbourhood <- airbnb_data$neighbourhood %>% as.factor() 
levels(airbnb_data$neighbourhood) <- c(""   ,                              "Camp d'en Grassot i Gràcia Nova", "Can Baró",                        
                                       "Carmel"   ,                        "Ciutat Vella",                     "Diagonal Mar - La Mar Bella" ,    
                                       "Dreta de l'Eixample"    ,          "Eixample"    ,                     "El Baix Guinardó",               
                                       "El Besós i el Maresme"   ,        "El Bon Pastor" ,                   "El Born" ,                        
                                       "El Camp de l'Arpa del Clot",       "El Clot"  ,                        "El Coll" ,                        
                                       "El Congrés i els Indians" ,       "El Fort Pienc"  ,                  "El Gòtic" ,                      
                                       "El Poble-sec"  ,                   "El Poblenou" ,                     "El Putget i Farró" ,             
                                       "El Raval"  ,                       "Glòries - El Parc" ,              "Gràcia",                         
                                       "Guinardó" ,                       "Horta"  ,                          "Horta-Guinardó"  ,               
                                       "L'Antiga Esquerra de l'Eixample",  "La Barceloneta"   ,                "La Font d'en Fargues" ,           
                                       "La Guineueta - Canyelles" ,        "La Maternitat i Sant Ramon" ,      "La Nova Esquerra de l'Eixample",  
                                       "La Prosperitat"   ,                "La Sagrada Família",              "La Sagrera"  ,                    
                                       "La Salut"    ,                     "La Teixonera",                     "La Trinitat Vella"  ,             
                                       "La Vall d'Hebron"   ,              "La Verneda i La Pau" ,             "La Vila Olímpica" ,              
                                       "Les Corts" ,                       "Les Tres Torres"   ,               "Montbau"  ,                       
                                       "Navas"   ,                         "Nou Barris"  ,                     "Pedralbes" ,                      
                                       "Porta"   ,                         "Provençals del Poblenou" ,        "Sant Andreu" ,                    
                                       "Sant Andreu de Palomar"  ,         "Sant Antoni"   ,                   "Sant Genís dels Agudells" ,      
                                       "Sant Gervasi - Galvany"  ,         "Sant Gervasi - la Bonanova",       "Sant Martí"    ,                 
                                       "Sant Martí de Provençals",       "Sant Pere/Santa Caterina",         "Sants-Montjuïc"   ,              
                                       "Sarrià "   ,                       "Sarrià -Sant Gervasi" ,            "Torre Baró"   ,                  
                                       "Trinitat Nova"  ,                  "Turó de la Peira - Can Peguera",  "Vallcarca i els Penitents" ,      
                                       "Verdum - Los Roquetes" ,           "Vila de Gràcia"     ,             "Vilapicina i la Torre Llobeta") 


# First we check that the class for each column is correct, and change the format of the percent and boolean columns
# In the case of numeric columnns, we will introduce NA's for those values that aren't numbers
# Numeric columns with percentages 
for(i in c(4, 5)){
        airbnb_data[, i] <- as.numeric(gsub("%", "", airbnb_data[, i])) / 100
}

# Numeric columns with dollar sign
for(i in c(30, 31, 32, 34)){
        airbnb_data[, i] <- as.numeric(gsub("\\$", "", airbnb_data[, i])) # the dollar is a regular expression
}

# Boolean columns
for(i in c(6, 11, 12, 20, 37, 51, 52, 53, 55, 56)){
        airbnb_data[, i] <- gsub("t", "TRUE", airbnb_data[, i])
        airbnb_data[, i] <- gsub("f", "FALSE", airbnb_data[, i])
}


# Now we create a list in order to spot strange values, by previously converting all columns to factors and extracting their levels
airbnb_data_factor <- airbnb_data
factor_list <- list()
for(i in 1:ncol(airbnb_data_factor)){
        airbnb_data_factor[, i] <- as.factor(airbnb_data_factor[, i])
        factor_list <- lapply(airbnb_data_factor, levels)
}

library(listviewer)
jsonedit(factor_list) # és una manera de veure les llistes de manera interactiva en una nova finestra

# We can see that the "host_verifications" and "amenities" columns need some treatment to convert them into dummie variables

library(mgsub)
library(stringr)

host_verifications <- as.data.frame(mgsub(airbnb_data$host_verifications, c("\\[", "\\]", "\\,", "\\'") , c("", "", "", "")))
amenities <- as.data.frame(mgsub(airbnb_data$amenities, c("\\{", "\\}", "\\,", "\\'", "\\[", "\\]", "\"", "\\/") , c("", "", "  ", "", "", "", "", "")))

output_list_verifications <- list()
output_list_verifications_count <- list()
output_list_amenities <- list()
output_list_amenities_count <- list()
for(i in 1:nrow(host_verifications)){
        output_list_verifications[i] <- str_split(host_verifications[i, ], " ")[1]
        output_list_verifications_count[i] <- length(output_list_verifications[[i]])
        output_list_amenities[i] <- str_split(amenities[i, ], "  ")[1]
        output_list_amenities_count[i] <- length(output_list_amenities[[i]])
}

dummy_host_verifications_cols <- output_list_verifications[[which.max(output_list_verifications_count)]] # here we have the row with the most number of amenities, which we will use to create some dummy variables
dummy_host_verifications_df <- matrix(nrow = nrow(airbnb_data), ncol = length(dummy_host_verifications_cols)) %>% as.data.frame()
colnames(dummy_host_verifications_df) <- dummy_host_verifications_cols

dummy_amenities_cols <- output_list_amenities[[which.max(output_list_amenities_count)]] # here we have the row with the most number of amenities, which we will use to create some dummy variables
dummy_amenities_df <- matrix(nrow = nrow(airbnb_data), ncol = length(dummy_amenities_cols)) %>% as.data.frame()
colnames(dummy_amenities_df) <- dummy_amenities_cols

# Now we merge the new columns with the airbnb dataset and we fill them with Yes (1) or No (0)
airbnb_data <- cbind(airbnb_data, dummy_host_verifications_df, dummy_amenities_df)

# Now we susbtitute the NA's with Yes(if the apartment has that verification or amenitie, and no if it doesn't)
for(l in 1:length(output_list_verifications)){
        for(j in 62:74){
                if(colnames(airbnb_data)[j] %in% output_list_verifications[[l]]){
                        airbnb_data[l, j] <- "Yes"
                } else{
                        
                }
        }
}

for(l in 1:length(output_list_amenities)){
        for(k in 75:ncol(airbnb_data)){
                if(colnames(airbnb_data)[k] %in% output_list_amenities[[l]]){
                        airbnb_data[l, k] <- "Yes"
                } else{
                        
                }
        }
}


# Now we convert all the new columns into factors
for(j in 62:ncol(airbnb_data)){
        for(i in 1:nrow(airbnb_data)){
                if(is.na(airbnb_data[i, j]) == TRUE){
                        airbnb_data[i, j] <- "No"
                } 
        }
        airbnb_data[, j] <- factor(airbnb_data[, j], levels = c("Yes", "No"))
}


airbnb_data$amenities <- NULL
airbnb_data$host_verifications <- NULL
airbnb_data$has_availability <- NULL
airbnb_data$is_business_travel_ready <- NULL

# Now I create variables that count the days since the host started, the days from the first review, and the days from the last review
airbnb_data$days_since_host <- (today() - as.Date(airbnb_data$host_since)) %>% as.numeric()
airbnb_data$days_since_first_review <- (today() - as.Date(airbnb_data$first_review)) %>% as.numeric()
airbnb_data$days_since_last_review <- (today() - as.Date(airbnb_data$last_review)) %>% as.numeric()
airbnb_data$host_since <- NULL
airbnb_data$first_review <- NULL
airbnb_data$last_review <- NULL
airbnb_data$host_response_time <- NULL
airbnb_data$host_listings_count <- NULL

# we delete the NA's from these columns, which we will use for reference for the following analysis
airbnb_data <- airbnb_data[complete.cases(airbnb_data$host_since_year) & complete.cases(airbnb_data$country) & complete.cases(airbnb_data$city), ] 

# Now we use the previous columns as a reference, in order to substitute the NA's with the median value (in the numerical variables), or the mode in the categorical


# Now we use this list to spot strange values
airbnb_data_factor <- airbnb_data
for(i in 1:ncol(airbnb_data_factor)){
        airbnb_data_factor[, i] <- as.factor(airbnb_data_factor[, i])
        factor_list <- lapply(airbnb_data_factor, levels)
}

library(listviewer)
jsonedit(factor_list)

# Strange values: "", "N/A", NA, "-", "*", ".", "[no name]", "."
library(modeest) #in order to use the mlv (mode function)
library(stringr)

# First we substitute the strange values with NA's
patterns <- c("N/A", "-", "*", "[no name]", ".", "")

for(j in 1:ncol(airbnb_data)){
        for(i in 1:nrow(airbnb_data)){
                for(k in 1:length(patterns)){
                        if(airbnb_data[i, j] == patterns[k] & !is.na(airbnb_data[i, j])){
                                airbnb_data[i, j] <- NA
                        }
                }
        }
}

# Now we substitute NA's with the median (in case of numerical variables) or the mode (in case of categorical variables) of the apartments in the same city and host_year
for(j in 1:ncol(airbnb_data)){
        for(i in 1:nrow(airbnb_data)){
                        if(is.na(airbnb_data[i, j]) == TRUE){
                                if(is.numeric(airbnb_data[, j]) == TRUE){
                                        airbnb_data[i, j] = median(airbnb_data[complete.cases(airbnb_data) & airbnb_data$host_since_year == airbnb_data[i, "host_since_year"]
                                                                               & airbnb_data$country == airbnb_data[i, "country"] & airbnb_data$city == airbnb_data[i, "city"] , j])
                                } else if(is.character(airbnb_data[, j]) == TRUE | is.factor(airbnb_data[, j]) == TRUE){
                                        airbnb_data[i, j] = mlv(airbnb_data[complete.cases(airbnb_data) & airbnb_data$host_since_year == airbnb_data[i, "host_since_year"]
                                                                             & airbnb_data$country == airbnb_data[i, "country"] & airbnb_data$city == airbnb_data[i, "city"], j])[1]
                        }
                }
        }
}


# Finally we filter out the possible NA's left (some columns had more NA's)
airbnb_data <- airbnb_data[complete.cases(airbnb_data), ] # 7500 files més o menys

for(i in 1:ncol(airbnb_data)){
        colnames(airbnb_data)[i] <- gsub(" ", "_", colnames(airbnb_data)[i])
}

airbnb_data <- airbnb_data[, !duplicated(colnames(airbnb_data))] # we delete duplicated columns (because somehow I had an error indicating duplicated column names)

airbnb_data <- airbnb_data[airbnb_data$city == "Barcelona", ] # there was one row that has strange values

airbnb_data$country <- NULL
airbnb_data$state <- NULL
airbnb_data$smart_location <- NULL
airbnb_data$city <- NULL # since there's only one city (Barcelona)
airbnb_data$requires_license <- NULL # since it is a factor with one level


# Em falta corregir els levels dels barris:
airbnb_data$neighbourhood <- as.factor(airbnb_data$neighbourhood)

# We save in a csv file
write.csv(airbnb_data, "C:/Users/marct/Documents/CURSOS/PROJECTES/AIRBNB PRICE PREDICTION/DATA")





# Exploratory Data Analysis: 
airbnb_data[is.na(airbnb_data), ] # there are no NA's, since we have substituted them with median and mean values, and deleted them (in case of variables that were dominated with NA's)
str(airbnb_data)
summary(airbnb_data)

## Now we are going to observe the variable we want to predict
ggplot(data = airbnb_data, aes(x = log(price))) + 
        geom_histogram(binwidth = 0.5, color = "black", fill = "skyblue") + 
        ggtitle("AirBnb Price per Night (Log scale)") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(hjust = 0.5, size = 12),
              axis.text = element_text(size = 12)) + 
        xlab("Log of Price per Night") + ylab("Number of apartments")



night_neighbourhood <- airbnb_data %>% group_by(neighbourhood) %>% summarize(avg_night_price = mean(price),                                                                            median_night_price = median(price)) %>% arrange(desc(avg_night_price))
night_neighbourhood <- night_neighbourhood[-1, ] # first row is clearly an outlier


## Most expensive neighbourhoods
ggplot(night_neighbourhood[1:10, ], aes(x = reorder(neighbourhood, avg_night_price),  y = avg_night_price, 
                                        fill = avg_night_price)) + 
        geom_bar(stat = "identity") + ggtitle("Average Airbnb Price per Night (10 most expensive neighbourhoods)") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(hjust = 0.5, size = 12),
              axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0(x, "$")) +
        xlab("") + ylab("Price") + coord_flip() + theme(legend.position = "none") + scale_fill_gradient(low = "yellow", high = "red")

## Most cheap neighbourhoods
ggplot(night_neighbourhood[(nrow(night_neighbourhood) - 10) : nrow(night_neighbourhood), ], aes(x = reorder(neighbourhood, avg_night_price),  y = avg_night_price, 
                                        fill = avg_night_price)) + 
        geom_bar(stat = "identity") + ggtitle("Average Airbnb Price per Night (10 cheapest neighbourhoods)") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(hjust = 0.5, size = 12),
              axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0(x, "$")) +
        xlab("Neighbourhood") + ylab("Price") + coord_flip() + theme(legend.position = "none") + scale_fill_gradient(low = "green",  high = "blue")

## Mapa barris i el preu mig de cada un
neighbourhoods_geojson <- rgdal::readOGR("./neighbourhoods.geojson")

neighbourhoods_geojson@data$neighbourhood <- as.factor(neighbourhoods_geojson@data$neighbourhood)
levels(neighbourhoods_geojson$neighbourhood) <- c("Baró de Viver" ,                               "Can Baró" ,                                   
                                                  "Can Peguera" ,                                  "Canyelles",                                    
                                                  "Ciutat Meridiana"   ,                           "Diagonal Mar i el Front Marítim del Poblenou",
                                                  "El Baix Guinardó"  ,                           "El Barri Gòtic"    ,                          
                                                  "El Besós i el Maresme",                        "El Bon Pastor"   ,                             
                                                  "El Camp d'en Grassot i Gràcia Nova" ,          "El Camp de l'Arpa del Clot"  ,                 
                                                  "El Carmel"       ,                              "El Clot"  ,                                    
                                                  "El Coll"   ,                                    "El Congrés i els Indians"   ,                 
                                                  "El Fort Pienc" ,                                "El Guinardó"  ,                               
                                                  "El Parc i la Llacuna del Poblenou" ,            "El Poble Sec"  ,                               
                                                  "El Poblenou"   ,                                "El Putxet i el Farró"  ,                      
                                                  "El Raval"   ,                                   "El Turó de la Peira",                         
                                                  "Horta"    ,                                     "Hostafrancs" ,                                 
                                                  "L'Antiga Esquerra de l'Eixample" ,              "La Barceloneta" ,                              
                                                  "La Bordeta"     ,                               "La Clota" ,                                    
                                                  "La Dreta de l'Eixample"  ,                      "La Font d'en Fargues"  ,                       
                                                  "La Font de la Guatlla"  ,                       "La Guineueta"    ,                             
                                                  "La Marina de Port"  ,                           "La Marina del Prat Vermell",                   
                                                  "La Maternitat i Sant Ramon" ,                   "La Nova Esquerra de l'Eixample" ,              
                                                  "La Prosperitat"  ,                              "La Sagrada Família" ,                         
                                                  "La Sagrera"   ,                                 "La Salut"   ,                                  
                                                  "La Teixonera"   ,                               "La Trinitat Nova" ,                            
                                                  "La Trinitat Vella"  ,                           "La Vall d'Hebron" ,                            
                                                  "La Verneda i la Pau"  ,                         "La Vila de Gràcia",                           
                                                  "La Vila Olímpica del Poblenou",                "Les Corts"    ,                                
                                                  "Les Roquetes"  ,                                "Les Tres Torres",                              
                                                  "Montbau" ,                                      "Navas",                                        
                                                  "Pedralbes"   ,                                  "Porta"  ,                                      
                                                  "Provençals del Poblenou"  ,                    "Sant Andreu"  ,                                
                                                  "Sant Antoni"   ,                                "Sant Genís dels Agudells"  ,                  
                                                  "Sant Gervasi - Galvany"  ,                      "Sant Gervasi - la Bonanova"  ,                 
                                                  "Sant Martí de Provençals"  ,                  "Sant Pere, Santa Caterina i la Ribera",        
                                                  "Sants"       ,                                  "Sants - Badal"    ,                            
                                                  "Sarrià "    ,                                   "Torre Baró" ,                                 
                                                  "Vallbona"    ,                                  "Vallcarca i els Penitents" ,                   
                                                  "Vallvidrera, el Tibidabo i les Planes"   ,      "Verdun"  ,                                     
                                                  "Vilapicina i la Torre Llobeta"  )         


neighbourhoods_geojson@data <- left_join(neighbourhoods_geojson@data, night_neighbourhood[, 1:2]) %>% as.data.frame()

for(i in 1:nrow(neighbourhoods_geojson@data)){
        if(is.na(neighbourhoods_geojson@data[i, "avg_night_price"]) == TRUE){
                neighbourhoods_geojson@data[i, 3] <- mean(neighbourhoods_geojson@data[complete.cases(neighbourhoods_geojson@data), 3])
        } else if(is.na(neighbourhoods_geojson@data[i, "avg_night_price"]) == FALSE){
        }
}



# MAPA 1: 


library(leaflet)
pal <- colorNumeric(
        palette = "YlGnBu",
        domain = neighbourhoods_geojson@data$avg_night_price
)


price_per_neighbourhood <- leaflet(neighbourhoods_geojson) %>%
        addTiles() %>% setView(lng = 2.1734, lat = 41.3851, zoom = 12) %>%
        addPolygons(stroke = TRUE, fillColor = ~ pal(avg_night_price), fillOpacity = 0.8,
                    highlight = highlightOptions(weight = 2,
                                                 color = ~ pal(avg_night_price), 
                                                 fillOpacity = 1,
                                                 bringToFront = TRUE),
                    label = ~neighbourhood,
                    smoothFactor = 0.2,
                    popup = ~ paste(paste(neighbourhood,":"), "<br/>","<b/>", paste("Avg Night Price: ", "$", round(avg_night_price)))) %>%
        addLegend("bottomright", pal = pal, values = ~avg_night_price, opacity = 1.0, 
                  title = "Average Airbnb Night Price",
                  labFormat = labelFormat(prefix = "$"), na.label="")



# MAPA PER GRUPS DE TIPUS DE VIVENDA

pal3 <- colorFactor(palette = c(
        "dodgerblue2", "#E31A1C", 
        "green4",
        "#6A3D9A", 
        "#FF7F00", 
        "black", "gold1",
        "skyblue2", "#FB9A99", 
        "palegreen2",
        "#CAB2D6", 
        "#FDBF6F", 
        "gray70", "khaki2",
        "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
        "darkturquoise", "green1"), 
        domain = airbnb_data$property_type)

# Ara creem un dataset per cada grup 
typeofproperty <- list()
for (i in 1:length(levels(as.factor(airbnb_data$property_type)))) {
        typeofproperty[[i]] <- airbnb_data %>% dplyr::filter(property_type == levels(as.factor(airbnb_data$property_type))[i])
}
names(typeofproperty) <- levels(as.factor(airbnb_data$property_type))

typeofproperty_map <- leaflet() %>% addTiles() %>% setView(lng = 2.1734, lat = 41.3851, zoom = 13)

for (i in 1:length(levels(as.factor(airbnb_data$property_type)))) {
        typeofproperty_map <- typeofproperty_map %>% addCircles(data = typeofproperty[[i]], lat = ~latitude, 
                                          lng = ~longitude, color = ~pal3(property_type), 
                                          fillOpacity = 1, label = ~property_type, 
                                          popup = ~price, group = levels(as.factor(airbnb_data$property_type))[i])
}

typeofproperty_map <- typeofproperty_map %>% addLegend(data = airbnb_data, "topleft", 
                                                         pal = pal3, values = ~property_type, title = "Property Type", 
                                                         opacity = 1, group = "Legend")

groups <- c("Legend", levels(as.factor(airbnb_data$property_type)))
typeofproperty_map <- typeofproperty_map %>% addLayersControl(overlayGroups = groups, 
                                                              options = layersControlOptions(collapsed = TRUE)) 

# Now we want to see which category is the most expensive
property_prices <- airbnb_data %>% group_by(property_type) %>% summarize(avg_night_price = mean(price)) %>%
        arrange(avg_night_price)

ggplot(data = property_prices, aes(x = reorder(property_type, avg_night_price), y = avg_night_price, fill = property_type)) + 
        geom_bar(stat = "identity") + coord_flip() + ggtitle("Average Airbnb Price per Night (By Property Type)") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(hjust = 0.5, size = 12),
              axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0(x, "$")) +
        xlab("") + ylab("Average Night Price")  + 
        theme(legend.position = "none") 

# Analyze correlations by pairs
library(corrr)

numeric_correlations <- select_if(airbnb_data, is.numeric) %>% correlate() %>% focus(price)

ggplot(data = numeric_correlations, aes(x = reorder(rowname, price), y = price)) + 
        geom_bar(aes(fill = price), stat = "identity") + 
        scale_fill_gradient(low = "blue", high = "red") +
        ggtitle("Correlation of price and the other numeric variables") +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              axis.title = element_text(hjust = 0.5, size = 14),
              axis.text.x = element_text(size = 14, angle = 90),
              legend.position = "none",
              legend.title = element_blank()) + 
        xlab("Numeric variable") + ylab("Correlation") + coord_flip()

