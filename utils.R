# function to remote special caracters, spaces, comma, dot,etc from location name
data_name_tolower_remove_space_dots <- function(data, data.name) {
  data <- data %>%
    mutate(name_check = str_to_lower(str_trim(str_replace_all(!!sym(data.name), "\\s+|\\.|-|\\(|\\)|\\[]|,|:|º|°", ""))))
  data$name_check<-stringi::stri_trans_general(data$name_check, "Latin-ASCII")
  return(data)
}


# Function to calculate sum based on specified activity, aggregation, and targets to summarize
calculate_sum <- function(data, activity, aggregation, targets_summarise){
  # Filter data based on activity if it is not 'none'
  if (activity[1] != 'none') {
    data <- filter(data, grepl(paste(activity, collapse = "|"), get(col.activity)))
  } 
  # Group by specified aggregation columns and summarise the targets
  data <- data %>%
    group_by_at(vars(all_of(c(aggregation))), .drop = F) %>% 
    summarise(across(all_of(c(targets_summarise)), ~ sum(., na.rm = TRUE))) %>% ungroup()
  return(data)
}


# Function to calculate maximum based on specified activity and aggregation
calculate_max <- function(data, activity, aggregation, targets_summarise){
  # Filter data based on activity if it is not 'none'
  if (activity[1] != 'none') {
    data <- filter(data, grepl(paste(activity, collapse = "|"), get(col.activity)))
  }
  
  data.agg <- data %>%
    group_by_at(vars(aggregation)) %>%
    summarize(across(targets_summarise, max)) %>%
    ungroup()
  
  # # Semi-join data with aggregated data to get filtered data
  # filtered_data <- semi_join(data, data.agg, by = c(aggregation))
  return(data.agg)
}


calculate_age_gender_disaggregation <- function(data){
  # Calculate total counts for each gender and age group
  data <- data %>%
    mutate(!!sym(col.nina) := rowSums(select(., any_of(ninas)), na.rm = TRUE),
           !!sym(col.nino) := rowSums(select(., any_of(ninos)), na.rm = TRUE),
           !!sym(col.mujer) := rowSums(select(., any_of(mujeres)), na.rm = TRUE),
           !!sym(col.hombre) := rowSums(select(., any_of(hombres)), na.rm = TRUE),
           !!sym(col.mujer.mayor) := rowSums(select(., any_of(mujeres_mayor)), na.rm = TRUE),
           !!sym(col.hombre.mayor) := rowSums(select(., any_of(hombres_mayor)), na.rm = TRUE),
           !!sym(col.nina.mujer) := rowSums(select(., any_of(ninas_mujeres)), na.rm = TRUE),
           !!sym(col.nino.hombre) := rowSums(select(., any_of(ninos_hombres)), na.rm = TRUE))
  
  # Calculate total reached
  data <- data %>% select(-!!sym(col.reached))
  data <- data %>%
    mutate(!!sym(col.reached) := rowSums(select(., any_of(col.totals)), na.rm = TRUE))
  
  return(data)
}



# Arrange data 
arrange_data <- function(data, adm.level){
  if ('adm0' %in% adm.level) {
    data <- data %>% 
      gather(key = "desagregacion", value = "personas",  -indicator)
  }else
    # arrange data
    data <- data %>% 
      gather(key = "desagregacion", value = "personas",  -indicator, -adm.level)
  
  return(data)
}


# calculate percentage
calculate_percentage <- function(data, adm.level){  
  # get total values for each indicator/objective
  data_total<-data %>% 
    filter(desagregacion == col.reached) %>% 
    rename('total_personas' = personas)
  
  # data <- data %>% 
  #   filter(variable != 'total_alcanzados')
  
  
  if ('adm0' %in% adm.level) {
    # add total alcanzados to data as new column
    data <- data %>% 
      left_join(data_total %>% select(-desagregacion), by = c('indicator'), relationship = "many-to-one")
    
  }
  else
    # add total alcanzados to data as new column
    data <- data %>% 
      left_join(data_total %>% select(-desagregacion), by = c('indicator', adm.level), relationship = "many-to-one")
  
  # calculate percentage 
  data <- data %>% 
    mutate(personas_pct = round((personas / total_personas) * 100, 2))
  
  return(data)
}


# Arrange data 
arrange_data_target <- function(data, adm.level){
  # arrange data
  data <- data %>% 
    gather(key = "desagregacion", value = "personas",  -adm.level)
  return(data)
}


# calculate percentage
calculate_percentage_target <- function(data, adm.level){  
  # get total values for each indicator/objective
  data_total<-data %>% 
    filter(desagregacion == col.reached) %>% 
    rename('total_personas' = personas)
  
  # add total alcanzados to data as new column
  data <- data %>% 
    left_join(data_total %>% select(-desagregacion), by = c(adm.level), relationship = "many-to-one")
  
  # calculate percentage 
  data <- data %>% 
    mutate(personas_pct = round((personas / total_personas) * 100, 2))
  
  return(data)
}



# VARIABLES ---------------------------------------------------------------
# AoR or Cluster
col.sector <- '#sector'

# Administrative Levels
adm0 <- c("#adm0+name", "#adm0+code")
adm1 <- c("#adm1+code", "#adm1+name")
adm12 <- c("#adm1+code", "#adm1+name", "#adm2+code", "#adm2+name")
adm123 <- c("#adm1+code", "#adm1+name", "#adm2+code", "#adm2+name", "#adm3+code", "#adm3+name")
adm4.code <- "#adm4+code"
# Administrative Level List
adm.level <- list(adm1, adm12, adm123)


# Location Identifier
location <- "#location"
col.location <- "#location"
col.location.type <- "#location+type"
adm.school <- c('#adm4+name', '#adm4+code')
col.loc.type <- "#location+type"


# Columns related to Activities
col.activity <- "#activity+name+selected"
col.recurrent <- "#activity+recurrent"
col.validated <- "#validated"


# Columns related to Reach and Demographics
col.reached <- '#reached'
col.reach.type <-c('#reached+indigenous',	'#reached+disabled', '#reached+lgbti',	'#victims+trafficking')
col.reach.disagg <- c('#reached','#reached+f+children+age0_2', '#reached+m+children+age0_2',	'#reached+f+children+age3_5',	'#reached+m+children+age3_5',	'#reached+f+children+age6_11',
                      '#reached+m+children+age6_11','#reached+f+children+age12_17','#reached+m+children+age12_17', '#reached+f+children+age18_19',	'#reached+m+children+age18_19',
                      '#reached+f+adult+age20_59', '#reached+m+adult+age20_59',	'#reached+f+elderly+age60_',	'#reached+m+elderly+age60_')

# Age variables
age.0.2<- c('#reached+f+children+age0_2', '#reached+m+children+age0_2')
age.3.5<-c('#reached+f+children+age3_5',	'#reached+m+children+age3_5')
age.6.11 <- c('#reached+f+children+age6_11', '#reached+m+children+age6_11')
age.12.17 <- c('#reached+f+children+age12_17', '#reached+m+children+age12_17')
age.adult <- c('#reached+f+children+age18_19', '#reached+m+children+age18_19',
               '#reached+f+adult+age20_59', '#reached+m+adult+age20_59',
               '#reached+f+elderly+age60_', '#reached+m+elderly+age60_')

# Gender and age disaggregation
ninas <- c('#reached+f+children+age0_2','#reached+f+children+age3_5', '#reached+f+children+age6_11','#reached+f+children+age12_17')
ninos <-c('#reached+m+children+age0_2','#reached+m+children+age3_5', '#reached+m+children+age6_11','#reached+m+children+age12_17')
mujeres <-c('#reached+f+children+age18_19','#reached+f+adult+age20_59')
hombres<- c('#reached+m+children+age18_19','#reached+m+adult+age20_59')
mujeres_mayor <-c('#reached+f+elderly+age60_')
hombres_mayor <- c('#reached+m+elderly+age60_')
ninas_mujeres <- c(ninas, mujeres,mujeres_mayor)
ninos_hombres <- c(ninos, hombres, hombres_mayor)

# Calculation Columns
col.totals <- c("#reached+f+children", "#reached+m+children", "#reached+f+adults", "#reached+m+adults",
                "#reached+f+elderly", "#reached+m+elderly")
col.soma <- c("#reached+f+children+adults+elderly", "#reached+m+children+adults+elderly")

# Individual Demographic Columns
col.nina <- "#reached+f+children"
col.nino <- "#reached+m+children"
col.mujer <- "#reached+f+adults"
col.hombre <- "#reached+m+adults"
col.mujer.mayor <- "#reached+f+elderly"
col.hombre.mayor <- "#reached+m+elderly"
col.nina.mujer <- "#reached+f+children+adults+elderly"
col.nino.hombre <- "#reached+m+children+adults+elderly"