

# Análisis exploratorio de datos
library(tidyverse)
library(lubridate)
library(leaflet)
library(corrr)
library(nortest)
library(dlookr)
library(reshape2)
library(ggpmisc)
library(ggpubr)
library(scales)
library(gt)

setwd("C:/Users/marct/Documents/CURSOS/PROJECTES/AIRBNB PRICE PREDICTION/DATA")

airbnb_data_viejo <- read.csv("./barcelona.csv") %>% select(c("host_since", "host_response_time", 
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




setwd("C:/Users/marct/Documents/UNI_ESTADISTICA/2o/SEGUNDO CUATRIMESTRE/INFERENCIA ESTADÍSTICA II/TRABAJOS/TRABAJO AIRBNB/DATOS")
airbnb_data <- read.csv("./datos_airbnb.csv")
# Seleccionamos variables que nos import
# Primer filtrado
nombres_seleccionados <- c("host_since_year", "host_acceptance_rate", "neighbourhood", "host_identity_verified", "latitude", "longitude", "property_type", "room_type", "accommodates", "bathrooms", "bedrooms", "beds", "bed_type", "square_feet", "price", "security_deposit", "cleaning_fee", "guests_included", "minimum_nights", "number_of_reviews", "review_scores_rating", "cancellation_policy", "facebook", "Cable_TV",  "Air_conditioning", "Wheelchair_accessible", "Elevator", "Heating", "Washer", "Fire_extinguisher", "Shampoo", "Hair_dryer", "Microwave",  "Coffee_maker", 
                           "Refrigerator", "Dishwasher", "Cooking_basics", "Oven", "Patio_or_balcony")
airbnb_data <- airbnb_data[, nombres_seleccionados]

# Segundo filtrado
segundo_filtrado <- c("host_acceptance_rate", "bed_type", "cleaning_fee", "guests_included", "cancellation_policy", "facebook",
  "Shampoo", "Hair_dryer", "Cooking_basics", "Coffee_maker", "host_identity_verified", 
  "Cable_TV", "Fire_extinguisher", "Wheelchair_accessible", "minimum_nights")

airbnb_data <- select(airbnb_data, -segundo_filtrado)


# Exploratory Data Analysis: 
## Miramos el dataset viejo
airbnb_data_viejo[is.na(airbnb_data_viejo), ] # there are no NA's, since we have substituted them with median and mean values, and deleted them (in case of variables that were dominated with NA's)
str(airbnb_data_viejo)
summary(airbnb_data_viejo)
describe_numerical_variables_viejo <- describe(airbnb_data_viejo) %>% select(c("variable", "n", "na")) # describe only works for numerical data
categorical_variables_viejo <- select_if(airbnb_data_viejo, is.character)
colnames_categorical_variables_viejo <- colnames(categorical_variables_viejo)

na <- vector()
for(j in 1:length(colnames_categorical_variables_viejo)){
  na = c(na, sum(is.na(categorical_variables_viejo[, j])))
}

n <- nrow(airbnb_data_viejo) - na
describe_categorical_variables_viejo <- data.frame(variable = colnames_categorical_variables_viejo, n = n, na = na)

describe_before_cleaning <- rbind(describe_numerical_variables_viejo, describe_categorical_variables_viejo)
tabla_antes_limpieza_datos <- gt(describe_before_cleaning) %>% tab_header(title = md("Tamaños muestrales y NA's antes de la limpieza de datos"))  

## Miramos el dataset nuevo
airbnb_data[is.na(airbnb_data), ] # there are no NA's, since we have substituted them with median and mean values, and deleted them (in case of variables that were dominated with NA's)
str(airbnb_data)
summary(airbnb_data)
describe_numerical_variables <- describe(airbnb_data) %>% select(c("variable", "n", "na")) # describe only works for numerical data
categorical_variables <- select_if(airbnb_data, is.character)
colnames_categorical_variables <- colnames(categorical_variables)
n <- rep(nrow(categorical_variables), length(colnames_categorical_variables))
na <- vector()
for(j in 1:length(colnames_categorical_variables)){
  na = c(na, sum(is.na(categorical_variables[, j])))
}

describe_categorical_variables <- data.frame(variable = colnames_categorical_variables, n = n, na = na)

describe_after_cleaning <- rbind(describe_numerical_variables, describe_categorical_variables)
tabla_despues_limpieza_datos <- gt(describe_after_cleaning) %>% tab_header(title = md("Tamaños muestrales y NA's después de la limpieza de datos"))  

# Convertimos host_since_year en categoria
airbnb_data$host_since_year <- airbnb_data$host_since_year %>% as.factor()

# Convertimos bathrooms en integer pa q no toque los cojones
airbnb_data$bathrooms <- as.integer(round(airbnb_data$bathrooms))
# Añadimos variable log_price
airbnb_data$log_price <- log(airbnb_data$price)

# NUMERICAL VARIABLES
# Sacamos las medidas descriptivas de todas las variables numéricas
analisis_descriptivo_numericas <- as.data.frame(describe(airbnb_data, colnames(select_if(airbnb_data, is.numeric))))
analisis_descriptivo_numericas$variable <- colnames(select_if(airbnb_data, is.numeric)) # porque algunos nombres estaban duplicados
analisis_descriptivo_numericas <- analisis_descriptivo_numericas[, c("variable", "mean", "sd", "IQR", "skewness", "kurtosis", "p00", "p25", "p50", "p75", "p100")]
analisis_descriptivo_numericas[, -1] <- apply(analisis_descriptivo_numericas[, -1], 2, round, 2)
colnames(analisis_descriptivo_numericas) <- c("variable", "mean", "sd", "IQR", "skewness", "kurtosis", "min", "Q1", "median", "Q3", "max")
## Now we are going to observe the variable we want to predict
## PRECIO
analisis_descriptivo_precio <- filter(analisis_descriptivo_numericas, variable == "price")
histograma_precio <- ggplot(data = airbnb_data, aes(x = price)) + 
  geom_histogram(color = "black", fill = "skyblue") + 
  geom_table_npc(data = analisis_descriptivo_precio, label = list(analisis_descriptivo_precio), 
                 npcx = 0.00, npcy = 1, hjust = -0.2, vjust = -0.4) +
  ggtitle("Precio por noche de un apartamento Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Precio por noche ($)") + ylab("Frecuencia") + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 50, 0)))

boxplot_precio <- ggplot(data = airbnb_data, aes(x = price)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Precio por noche ($)") + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_precio <- ggarrange(histograma_precio, boxplot_precio, nrow = 2, ncol = 1, heights = c(1, 0.6))

# Precio log
analisis_descriptivo_precio_log <- filter(analisis_descriptivo_numericas, variable == "log_price")
histograma_precio_log <- ggplot(data = airbnb_data, aes(x = log_price)) + 
  geom_histogram(color = "black", fill = "skyblue") + 
  geom_table_npc(data = analisis_descriptivo_precio_log, label = list(analisis_descriptivo_precio_log), 
                 npcx = 0.00, npcy = 1, hjust = -0.2, vjust = -0.4) +
  ggtitle("Precio por noche de un apartamento Airbnb (log)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Log de Precio por noche ($)") + ylab("Frecuencia") + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 50, 0))) 

boxplot_precio_log <- ggplot(data = airbnb_data, aes(x = log_price)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Log de Precio por noche ($)") + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_precio_log <- ggarrange(histograma_precio_log, boxplot_precio_log, nrow = 2, ncol = 1, heights = c(1, 0.6))

# MAPAS
night_neighbourhood <- airbnb_data %>% group_by(neighbourhood) %>% summarize(avg_night_price = mean(price), median_night_price = median(price), listings = n()) %>% arrange(desc(avg_night_price))
night_neighbourhood <- night_neighbourhood[-1, ] # first row is clearly an outlier

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


neighbourhoods_geojson@data <- left_join(neighbourhoods_geojson@data, night_neighbourhood[, c(1, 2, 4)]) %>% as.data.frame()

for(i in 1:nrow(neighbourhoods_geojson@data)){
        if(is.na(neighbourhoods_geojson@data[i, "avg_night_price"]) == TRUE){
                neighbourhoods_geojson@data[i, 3] <- mean(neighbourhoods_geojson@data[complete.cases(neighbourhoods_geojson@data), 3])
        } 
        
        if(is.na(neighbourhoods_geojson@data[i, "listings"]) == TRUE){
                neighbourhoods_geojson@data[i, 4] <- round(mean(neighbourhoods_geojson@data[complete.cases(neighbourhoods_geojson@data), 4]))
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
                    popup = ~ paste(paste(neighbourhood,":"), "<br/>","<b/>", paste("Precio medio/noche: ", "$", round(avg_night_price)))) %>%
        addLegend("bottomright", pal = pal, values = ~avg_night_price, opacity = 1.0, 
                  title = "Precio medio por noche",
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
                                                       pal = pal3, values = ~property_type, title = "Tipo de propiedad", 
                                                       opacity = 1, group = "Legend")

groups <- c("Legend", levels(as.factor(airbnb_data$property_type)))
typeofproperty_map <- typeofproperty_map %>% addLayersControl(overlayGroups = groups, 
                                                              options = layersControlOptions(collapsed = TRUE)) 

# Now we want to see which category is the most expensive
property_prices <- airbnb_data %>% group_by(property_type) %>% summarize(avg_night_price = mean(price)) %>%
        arrange(avg_night_price)

precio_tipo_propiedad <- ggplot(data = property_prices, aes(x = reorder(property_type, avg_night_price), y = avg_night_price, fill = property_type)) + 
        geom_bar(stat = "identity") + coord_flip() + ggtitle("Precio medio por noche de Airbnb según tipo") + 
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.title = element_text(hjust = 0.5, size = 12),
              axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0(x, "$")) +
        xlab("") + ylab("Precio medio por noche")  + 
        theme(legend.position = "none") 

room_type_prices <- airbnb_data %>% group_by(room_type) %>% summarize(avg_night_price = mean(price)) %>%
  arrange(avg_night_price)

precio_room_type <- ggplot(data = room_type_prices, aes(x = reorder(room_type, avg_night_price), y = avg_night_price, fill = room_type)) + 
  geom_bar(stat = "identity") + coord_flip() + ggtitle("Precio medio por noche de Airbnb según tipo") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0(x, "$")) +
  xlab("") + ylab("Precio medio por noche")  + 
  theme(legend.position = "none") 

## Most expensive neighbourhoods
most_expensive_neighbourhoods <- ggplot(night_neighbourhood[1:10, ], aes(x = reorder(neighbourhood, avg_night_price),  y = avg_night_price, 
                                                                         fill = avg_night_price)) + 
  geom_bar(stat = "identity") + ggtitle("Top 10 barrios más caros por noche") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0(x, "$")) +
  xlab("Barrio") + ylab("Precio medio por noche") + coord_flip() + 
  theme(legend.position = "none") + 
  scale_fill_gradient(low = "yellow", high = "red")

## Most cheap neighbourhoods
cheapest_neighbourhoods <- ggplot(night_neighbourhood[(nrow(night_neighbourhood) - 10) : nrow(night_neighbourhood), ], aes(x = reorder(neighbourhood, avg_night_price),  y = avg_night_price, 
                                                                                                                           fill = avg_night_price)) + 
  geom_bar(stat = "identity") + ggtitle("Top 10 barrios más baratos por noche") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + scale_y_continuous(labels = function(x) paste0(x, "$")) +
  xlab("") + ylab("Precio medio por noche") + coord_flip() + theme(legend.position = "none") + scale_fill_gradient(low = "green",  high = "blue")

## Convertimos en factor las categóricas, después de crear el mapa, pq creo que con factors no deja

## Convertim characters en factors les binaries
for(i in 1:ncol(airbnb_data)){
  if(is.character(airbnb_data[, i]) == TRUE){
    if(i >= 16){
      airbnb_data[, i] <- factor(airbnb_data[, i], levels = c("Yes", "No"))
    } else {
      airbnb_data[, i] <- factor(airbnb_data[, i])
    }
  }
}

## Precios según antiguedad del dueño en la página
boxplot_precio_antiguedad <- ggplot(data = airbnb_data, aes(x = host_since_year, y = price, 
                                                            fill = host_since_year)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Año en que el anfitrión se registró en Airbnb") +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según antiguedad del anfitrión") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))


## Air conditioning
boxplot_precio_air_conditioning <- ggplot(data = airbnb_data, aes(x = Air_conditioning, y = price, 
                                                                  fill = Air_conditioning)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Aire Acondicionado")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de aire acondicionado") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

# Elevator
boxplot_precio_Elevator <- ggplot(data = airbnb_data, aes(x = Elevator, y = price, 
                                                          fill = Elevator)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Ascensor")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de ascensor") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))


# Heating
boxplot_precio_Heating <- ggplot(data = airbnb_data, aes(x = Heating, y = price, 
                                                         fill = Heating)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Calefacción")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de calefacción") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

# Washer
boxplot_precio_Washer <- ggplot(data = airbnb_data, aes(x = Washer, y = price, 
                                                        fill = Washer)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Lavadora")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de lavadora") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

# Microwave
boxplot_precio_Microwave <- ggplot(data = airbnb_data, aes(x = Microwave, y = price, 
                                                           fill = Microwave)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Microondas")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de microondas") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

# Refrigerator
boxplot_precio_Refrigerator <- ggplot(data = airbnb_data, aes(x = Refrigerator, y = price, 
                                                              fill = Refrigerator)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Nevera")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de nevera") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

# Dishwasher
boxplot_precio_Dishwasher <- ggplot(data = airbnb_data, aes(x = Dishwasher, y = price, 
                                                            fill = Dishwasher)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Lavavajillas")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de lavavajillas") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

# Oven
boxplot_precio_Oven <- ggplot(data = airbnb_data, aes(x = Oven, y = price, 
                                                      fill = Oven)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de horno")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de horno") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

# Patio_or_balcony
boxplot_precio_Patio_or_balcony <- ggplot(data = airbnb_data, aes(x = Patio_or_balcony, y = price, 
                                                                  fill = Patio_or_balcony)) + 
  geom_boxplot(color = "black", size = 0.5, alpha = 0.5) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  ylab("Precio por noche") + xlab("Disponibilidad de Patio o Balcón")  +
  scale_y_continuous(labels = function(x) paste0(x, "$")) +
  ggtitle("Precio por noche según \
  la disponibilidad de Patio o Balcón") +
  theme(legend.position = "none",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15, margin = margin(0, 0, 10, 0)),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))


# Analyze correlations by pairs
library(corrr)

numeric_correlations <- as.data.frame(select_if(airbnb_data, is.numeric) %>% correlate()) %>% filter(var2 == "price") %>% select(-var2)
colnames(numeric_correlations) <- c("Variables", "Correlacion_precio")
numeric_correlations <- numeric_correlations[numeric_correlations$Variables != "log_price", ]

grafico_correlaciones_precio <- ggplot(data = numeric_correlations, aes(x = reorder(Variables, Correlacion_precio), y = Correlacion_precio)) + 
        geom_bar(aes(fill = Correlacion_precio), stat = "identity") + 
        scale_fill_gradient(low = "blue", high = "red") +
        ggtitle("Correlación del precio y las demás variables numéricas.") +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(0, 0, 10, 0)),
              axis.title = element_text(hjust = 0.5, size = 14),
              axis.text.x = element_text(size = 16, angle = 90),
              legend.position = "none",
              legend.title = element_blank()) + 
        xlab("Numeric variable") + ylab("Coeficiente de correlación de Pearson") + coord_flip()


## ACCOMMODATES
analisis_descriptivo_accommodates <- filter(analisis_descriptivo_numericas, variable == "accommodates")
histograma_accommodates <- ggplot(data = airbnb_data, aes(x = accommodates)) + 
  geom_histogram(color = "black", fill = "skyblue", binwidth = 2) + 
  geom_table_npc(data = analisis_descriptivo_accommodates, label = list(analisis_descriptivo_accommodates), 
                 npcx = 0.00, npcy = 1, hjust = -0.2, vjust = -0.4) +
  ggtitle("Capacidad de huéspedes de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de huéspedes") + ylab("Frecuencia") + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 50, 0)))

boxplot_accommodates <- ggplot(data = airbnb_data, aes(x = accommodates)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de huéspedes") + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_accommodates <- ggarrange(histograma_accommodates, boxplot_accommodates, nrow = 2, ncol = 1, heights = c(1, 0.6))

## BATHROOMS
analisis_descriptivo_bathrooms <- filter(analisis_descriptivo_numericas, variable == "bathrooms")
histograma_bathrooms <- ggplot(data = airbnb_data, aes(x = bathrooms)) + 
  geom_histogram(color = "black", fill = "skyblue", binwidth = 2) + 
  geom_table_npc(data = analisis_descriptivo_bathrooms, label = list(analisis_descriptivo_bathrooms), 
                 npcx = 0.00, npcy = 1, hjust = -0.3, vjust = -0.4) +
  ggtitle("Número de baños de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de baños") + ylab("Frecuencia") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 50, 0)))

boxplot_bathrooms <- ggplot(data = airbnb_data, aes(x = bathrooms)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de baños") + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_bathrooms <- ggarrange(histograma_bathrooms, boxplot_bathrooms, nrow = 2, ncol = 1, heights = c(1, 0.6))

## BEDROOMS
analisis_descriptivo_bedrooms <- filter(analisis_descriptivo_numericas, variable == "bedrooms")
histograma_bedrooms <- ggplot(data = airbnb_data, aes(x = bedrooms)) + 
  geom_histogram(color = "black", fill = "skyblue", binwidth = 2) + 
  geom_table_npc(data = analisis_descriptivo_bedrooms, label = list(analisis_descriptivo_bedrooms), 
                 npcx = 0.00, npcy = 1, hjust = -0.3, vjust = -0.4) +
  ggtitle("Número de habitaciones de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de habitaciones") + ylab("Frecuencia") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 50, 0)))

boxplot_bedrooms <- ggplot(data = airbnb_data, aes(x = bedrooms)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de dormitorios") + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_bedrooms <- ggarrange(histograma_bedrooms, boxplot_bedrooms, nrow = 2, ncol = 1, heights = c(1, 0.6))

## BEDS
analisis_descriptivo_beds <- filter(analisis_descriptivo_numericas, variable == "beds")
histograma_beds <- ggplot(data = airbnb_data, aes(x = beds)) + 
  geom_histogram(color = "black", fill = "skyblue") + 
  geom_table_npc(data = analisis_descriptivo_beds, label = list(analisis_descriptivo_beds), 
                 npcx = 0.00, npcy = 1, hjust = -0.3, vjust = -0.4) +
  ggtitle("Número de camas de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de camas") + ylab("Frecuencia") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 20, 50, 0)))

boxplot_beds <- ggplot(data = airbnb_data, aes(x = beds)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de camas")  + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_beds <- ggarrange(histograma_beds, boxplot_beds, nrow = 2, ncol = 1, heights = c(1, 0.6))

## SQUARE FEET
analisis_descriptivo_square_feet <- filter(analisis_descriptivo_numericas, variable == "square_feet")
histograma_square_feet <- ggplot(data = airbnb_data, aes(x = square_feet)) + 
  geom_histogram(color = "black", fill = "skyblue", binwidth = 200) + 
  geom_table_npc(data = analisis_descriptivo_square_feet, label = list(analisis_descriptivo_square_feet), 
                 npcx = 0.00, npcy = 1, hjust = -0.25, vjust = -0.4) +
  ggtitle("Superfície de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab(expression (m^2)) + ylab("Frecuencia") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 10, 50, 0)))

boxplot_square_feet <- ggplot(data = airbnb_data, aes(x = square_feet)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab(expression (m^2))  + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_square_feet <- ggarrange(histograma_square_feet, boxplot_square_feet, nrow = 2, ncol = 1, heights = c(1, 0.6))


## SECURITY DEPOSIT
analisis_descriptivo_security_deposit <- filter(analisis_descriptivo_numericas, variable == "security_deposit")
histograma_security_deposit <- ggplot(data = airbnb_data, aes(x = security_deposit)) + 
  geom_histogram(color = "black", fill = "skyblue", binwidth = 100) + 
  geom_table_npc(data = analisis_descriptivo_security_deposit, label = list(analisis_descriptivo_security_deposit), 
                 npcx = 0.00, npcy = 1, hjust = -0.25, vjust = -0.4) +
  ggtitle("Fianza de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Fianza ($)") + ylab("Frecuencia") + xlim(c(0, 1000)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 10, 50, 0)))

boxplot_security_deposit <- ggplot(data = airbnb_data, aes(x = security_deposit)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Fianza ($)")  + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_security_deposit <- ggarrange(histograma_security_deposit, boxplot_security_deposit, nrow = 2, ncol = 1, heights = c(1, 0.6))


## NUMBER OF REVIEWS
analisis_descriptivo_number_of_reviews <- filter(analisis_descriptivo_numericas, variable == "number_of_reviews")
histograma_number_of_reviews <- ggplot(data = airbnb_data, aes(x = number_of_reviews)) + 
  geom_histogram(color = "black", fill = "skyblue") + 
  geom_table_npc(data = analisis_descriptivo_number_of_reviews, label = list(analisis_descriptivo_number_of_reviews), 
                 npcx = 0.00, npcy = 1, hjust = -0.25, vjust = -0.4) +
  ggtitle("Nº reviews de noches de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de reviews") + ylab("Frecuencia") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', 
                                  hjust = 0.5, margin = margin(0, 0, 50, 0)))

boxplot_number_of_reviews <- ggplot(data = airbnb_data, aes(x = number_of_reviews)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Número de reviews")  + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_number_of_reviews <- ggarrange(histograma_number_of_reviews, boxplot_number_of_reviews, nrow = 2, ncol = 1, heights = c(1, 0.6))


## REVIEW SCORES RATING
analisis_descriptivo_review_scores_rating <- filter(analisis_descriptivo_numericas, variable == "review_scores_rating")
histograma_review_scores_rating <- ggplot(data = airbnb_data, aes(x = review_scores_rating)) + 
  geom_histogram(color = "black", fill = "skyblue") + 
  geom_table_npc(data = analisis_descriptivo_review_scores_rating, 
                 label = list(analisis_descriptivo_review_scores_rating), 
                 npcx = 0.00, npcy = 1, hjust = -0.22, vjust = -0.4) +
  ggtitle("Puntuación de los apartamentos de Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Puntuación") + ylab("Frecuencia") + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', 
                                  hjust = 0.5, margin = margin(0, 0, 50, 0)))

boxplot_review_scores_rating <- ggplot(data = airbnb_data, aes(x = review_scores_rating)) + 
  geom_boxplot(color = "black", fill = "lightgreen") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Puntuación")  + ylab("           ") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5, margin = margin(0, 0, 10, 0)))

grafico_review_scores_rating <- ggarrange(histograma_review_scores_rating, boxplot_review_scores_rating, nrow = 2, ncol = 1, heights = c(1, 0.6))


# VARIABLES CATEGÓRICAS
# host_since_year
tabla_proporciones_host_since_year <- as.data.frame(matrix(nrow = length(prop.table(table(airbnb_data$host_since_year))), ncol = 2))
tabla_proporciones_host_since_year[, 1] <- factor(names(prop.table(table(airbnb_data$host_since_year))))
tabla_proporciones_host_since_year[, 2] <- unname(prop.table(table(airbnb_data$host_since_year)))
tabla_proporciones_host_since_year[, 3] <- unname(table(airbnb_data$host_since_year))
colnames(tabla_proporciones_host_since_year) <- c("category", "prop", "count")

grafico_host_since_year <- ggplot(data = tabla_proporciones_host_since_year, aes(x = category, 
                                                                     y = prop)) + 
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) + 
  scale_y_continuous(labels = percent, limits = c(0, 0.34)) + 
  geom_text(data = tabla_proporciones_host_since_year, aes(label = paste0(round(prop*100, 1), "%", " (", count, ")"),
                                                     y = prop + 0.02), size = 4.5, color = "purple", alpha = 0.85) + 
  ggtitle("Año de registro en Airbnb del anfitrión") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Año") + ylab("Frecuencia relativa") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', 
                                  hjust = 0.3, margin = margin(0, 0, 10, 10)))
# neigbourhood
pal2 <- colorNumeric(
  palette = "YlOrRd",
  domain = neighbourhoods_geojson@data$listings
)

count_apartments_per_neighbourhood <- leaflet(neighbourhoods_geojson) %>%
  addTiles() %>% setView(lng = 2.1734, lat = 41.3851, zoom = 12) %>%
  addPolygons(stroke = TRUE, fillColor = ~ pal2(listings), fillOpacity = 0.8,
              highlight = highlightOptions(weight = 2,
                                           color = ~ pal2(listings), 
                                           fillOpacity = 1,
                                           bringToFront = TRUE),
              label = ~neighbourhood,
              smoothFactor = 0.2,
              popup = ~ paste(paste(neighbourhood,":"), "<br/>","<b/>", paste("Número de apartamentos: ", round(listings)))) %>%
  addLegend("bottomright", pal = pal2, values = ~listings, opacity = 1.0, 
            title = "Número de apartamentos por barrio", na.label="")

# property_type
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
                                                       pal = pal3, values = ~property_type, title = "Tipo de propiedad", 
                                                       opacity = 1, group = "Legend")

groups <- c("Legend", levels(as.factor(airbnb_data$property_type)))
typeofproperty_map <- typeofproperty_map %>% addLayersControl(overlayGroups = groups, 
                                                              options = layersControlOptions(collapsed = TRUE)) 

# room_type
pal4 <- colorFactor(palette = c(
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
  domain = airbnb_data$room_type)

# Ara creem un dataset per cada grup 

room_type <- list()
for (i in 1:length(levels(as.factor(airbnb_data$room_type)))) {
  room_type[[i]] <- airbnb_data %>% dplyr::filter(room_type == levels(as.factor(airbnb_data$room_type))[i])
}
names(room_type) <- levels(as.factor(airbnb_data$room_type))

room_type_map <- leaflet() %>% addTiles() %>% setView(lng = 2.1734, lat = 41.3851, zoom = 13)

for (i in 1:length(levels(as.factor(airbnb_data$room_type)))) {
  room_type_map <- room_type_map %>% addCircles(data = room_type[[i]], lat = ~latitude, 
                                                          lng = ~longitude, color = ~pal4(room_type), 
                                                          fillOpacity = 1, label = ~room_type, 
                                                          popup = ~price, group = levels(as.factor(airbnb_data$room_type))[i])
}

room_type_map <- room_type_map %>% addLegend(data = airbnb_data, "topleft", 
                                                       pal = pal4, values = ~room_type, title = "Tipo de habitación", 
                                                       opacity = 1, group = "Legend")

groups <- c("Legend", levels(as.factor(airbnb_data$room_type)))
room_type_map <- room_type_map %>% addLayersControl(overlayGroups = groups, 
                                                              options = layersControlOptions(collapsed = TRUE)) 

tabla_proporciones_room_type <- as.data.frame(matrix(nrow = length(prop.table(table(airbnb_data$room_type))), ncol = 2))
tabla_proporciones_room_type[, 1] <- factor(names(prop.table(table(airbnb_data$room_type))))
tabla_proporciones_room_type[, 2] <- unname(prop.table(table(airbnb_data$room_type)))
tabla_proporciones_room_type[, 3] <- unname(table(airbnb_data$room_type))
colnames(tabla_proporciones_room_type) <- c("category", "prop", "count")


grafico_room_type <- ggplot(data = tabla_proporciones_room_type, aes(x = reorder(category, -prop), 
                                                                     y = prop, fill = category)) + 
  geom_bar(alpha = 0.85, stat = "identity") + 
  scale_y_continuous(labels = percent, limits = c(0,1)) + 
  geom_text(data = tabla_proporciones_room_type, aes(label = paste0(round(prop*100, 1), "%", " (", count, ")"),
                               y = prop + 0.15, color = category), size = 5) + 
  ggtitle("Tipo de habitación en Airbnb") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12)) + 
  xlab("Tipo de habitación") + ylab("Frecuencia relativa") + coord_flip() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 15, face = "bold"), 
        plot.title = element_text(size = 20, face = 'bold', 
                                  hjust = 0.3, margin = margin(0, 0, 10, 10)))



# Variables binarias
# Air conditioning
variable <- vector()
Yes <- vector()
No <- vector()
tabla_proporciones_variables_binarias <- data.frame(variable, Yes, No)
for (i in 16:(ncol(airbnb_data) - 1)){
  yes_no <- unname(prop.table(table(airbnb_data[, i])))
  tabla_proporciones_variables_binarias <- rbind(tabla_proporciones_variables_binarias, data.frame(variable = colnames(airbnb_data)[i], Yes = yes_no[1], No = yes_no[2]))
}

tabla_inicial_proporciones_variables_binarias <- gt(tabla_proporciones_variables_binarias) %>%
tab_header(title = md("Variables binarias: Frecuencias relativas"))  

tabla_final_proporciones_variables_binarias <- fmt_percent(tabla_inicial_proporciones_variables_binarias, columns = c(2, 3))