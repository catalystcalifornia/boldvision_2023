library(stringr)
library(tidygeocoder)
library(dplyr)
source("W:\\RDA Team\\R\\credentials_source.R")
Sys.setenv(GOOGLEGEOCODE_API_KEY = google_geocoding_lbripa_key)

##### Step 0: Minor prep #####
# import
bv_conn <- connect_to_db("bold_vision")
lasd_stops_query <- "SELECT * FROM bv_2023.lasd_youth_arrests_togeocode;"
lasd_stops <- dbGetQuery(bv_conn, lasd_stops_query) %>%
  # don't need suite, drop it
  select(-c("suite"))
dbDisconnect(bv_conn)

# make everything lowercase
lasd_stops <- lasd_stops %>%
  mutate(across(where(is.character), tolower))

##### Step 1: Add a landmark-only flag (only address data is in landmark column) #####
lasd_stops <- lasd_stops %>%
  mutate(count_na_cols = rowSums(is.na(select(.,"street_number", "street_direction", 
                                              "street_name", "street_type", "cross_street"))),
         landmark_only_flag = ifelse(count_na_cols==5 & !is.na(landmark), 1, 0))

# filter out landmark only stops
landmark_only_stops <- lasd_stops %>%
  filter(landmark_only_flag==1)

##### Step 2: Clean single and cross-street addresses #####
# some of these also have landmark data - these look supplementary at best, will ignore
street_stops <- lasd_stops %>%
  filter(landmark_only_flag==0)

### get this into a long format to clean out redundant streets, standardize street types, etc.
# combine street name and street type into "street1" col
# note: paste doesn't suppress NA so I used this ifelse with str_c; str_c propagates NA (if one of the values is NA the result is NA which is why the ifelse is orderder like this)
street_stops <- street_stops %>%
  mutate(street1 = case_when(
    (is.na(street_direction) & is.na(street_number)) ~ street_name,
     is.na(street_direction) ~ str_c(street_number, street_name, sep=" "),
     is.na(street_number) ~ str_c(street_direction, street_name, sep=" "),
    .default = str_c(street_number, street_direction, street_name, sep=" ")
     ))

street_stops <- street_stops %>%
  mutate(street1 = case_when(
    is.na(street_type) ~ street1,
    .default = str_c(street1, street_type, sep=" ")
  ))

# split cross_street on "/", label results "street2", "street3", etc...
street_stops <- street_stops %>%
  separate(cross_street, c("street2", "street3"), sep="/")

street_stops$street2 <- sapply(street_stops$street2, str_squish)
street_stops$street3 <- sapply(street_stops$street3, str_squish)

# see stops where street1 and 2 and 3 is not NA
# stops_3streets <- street_stops %>%
#   filter(!is.na(street1) & !is.na(street2) & !is.na(street3)) %>%
#   rowwise() %>%
#   mutate(check_street2_dupe = ifelse(grepl(paste0("^", street_name), street2)==TRUE, 1, 0),
#          check_street3_dupe = ifelse(grepl(paste0("^", street_name), street3)==TRUE, 1, 0))

street_stops <- street_stops %>%
  rowwise() %>%
  mutate(street2 = case_when(grepl(paste0("^", street_name), street2)== TRUE ~ NA,
                             .default = street2),
         street3 = case_when(grepl(paste0("^", street_name), street3)== TRUE ~ NA,
                             .default = street3),) %>%
  ungroup()

# long format
street_stops_long <- street_stops %>%
  select(contact_id, person_id, street1, street2, street3) %>%
  pivot_longer(cols=c(street1, street2, street3), names_to = "street_order", values_to = "street") %>%
  filter(!is.na(street))

# clean up white space
street_stops_long$street <- sapply(street_stops_long$street, str_squish)

# check frequency of streets (see potential typos, missing street info like type, etc)
# street_freq <- as.data.frame(table(street_stops_long$street))

# pull out street_type and street_direction information and standardize - this will be fun!
# street direction and street type can appear ANYWHERE in the street, will need to check if each address starts, ends, or contains the various iterations
# fortunately, the spacing is good enough and it doesn't look like any typos exist to mess this up (we can use white space to make sure we only update what we want)
# there are "alphabet" avenue streets - we need to preserve their formats as "avenue [letter]"
# rather than "[letter] avenue" - to track them I add a hyphen to the end of the value
# street_stops_long$street <- gsub("([ave|avenue]\\s[a-z]{1})$", "\\1-", street_stops_long$street)

# # handle street type - 'Avenue, Boulevard, Drive, Highway, Plaza, Road, street, way';
# check_regex <- street_stops_long %>%
#   filter(grepl("(^|\\s)(ave|av)($|\\s)", street))

street_stops_long <- street_stops_long %>%
  mutate(street_clean1 = case_when(
    grepl("(^|\\s)(ave|av)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(ave|av)($|\\s)", " avenue ", street_stops_long$street),
    grepl("(^|\\s)(blvd|bl)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(blvd|bl)($|\\s)", " boulevard ", street_stops_long$street),
    grepl("(^|\\s)(dr)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(dr)($|\\s)", " drive ", street_stops_long$street),
    grepl("(^|\\s)(hwy|hw)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(hwy|hw)($|\\s)", " highway ", street_stops_long$street),
    grepl("(^|\\s)(pl)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(pl)($|\\s)", " plaza ", street_stops_long$street),
    grepl("(^|\\s)(rd)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(rd)($|\\s)", " road ", street_stops_long$street),
    grepl("(^|\\s)(st)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(st)($|\\s)", " street ", street_stops_long$street),
    grepl("(^|\\s)(wa)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(wa)($|\\s)", " way ", street_stops_long$street),
    grepl("(^|\\s)(fwy)($|\\s)", street) == TRUE ~ gsub("(^|\\s)(fwy)($|\\s)", " freeway ", street_stops_long$street),
    .default=street))

# remove hyphen from alphabet avenues
street_stops_long$street_final <- gsub("-", " ", street_stops_long$street_clean1, fixed=TRUE)

# squish (clean white space)
street_stops_long$street_final <- sapply(street_stops_long$street_final, str_squish)

# widen dataframe and build geocode addresses - bring in landmarks or handle separately
street_stops_wide <- street_stops_long %>%
  select(contact_id, person_id, street_order, street_final) %>%
  pivot_wider(values_from = street_final, names_from = street_order)

# join back to original lasd_stops df
final <- lasd_stops %>%
  left_join(street_stops_wide, by=c("contact_id", "person_id"))

# # create geocode address: note the addresses with direction added from the cleaning process do not conflict with addresses in lasd_stops where street_direction exists
final <- final %>%
  mutate(geocode_street = case_when(
    (is.na(street2) & is.na(street3)) ~ street1,
    is.na(street2) ~ str_c(street1, street3, sep=" and "),
    is.na(street3) ~ str_c(street1, street2, sep=" and "),
    is.na(street1) ~ str_c(street2, street3, sep=" and ")))


## some manual fixes
# one case 2300 r avenue - manually update to be consistent with other alphabet avenues
final$geocode_street[(final$contact_id=="785636" & final$person_id=="866946")] <- "2300 e avenue r"

# stop 673289-741708; typo - should probably be 185th St East & E Ave H 12 
final$geocode_street[(final$contact_id=="673289" & final$person_id=="741708")] <- "185th St East and E Ave H 12"

# stop 816546-901216: 50st - probably 50th street
final$geocode_street[(final$contact_id=="816546" & final$person_id=="901216")] <- "50th St West and Ave M 8"

# stop 634894-699315: avenida padilla av - just avenida padilla - google might correct it on its own
final$geocode_street[(final$contact_id=="634894" & final$person_id=="699315")] <- "16000 avenida padilla"

# 652130-718376
final$geocode_street[(final$contact_id=="652130" & final$person_id=="718376")] <- "29300 the old road and biscaluz drive"

# 676342-745064
final$geocode_street[(final$contact_id=="676342" & final$person_id=="745064")] <- "190th Street West and W Ave A"

# check our work
check_final <- final %>%
  select(contact_id, person_id, geocode_street, street_number, street_direction, street_name, street_type, cross_street, city, state, zip_code, landmark, everything())

# pull only necessary columns and clean up remaining symbols(should only be "-" at this point)
geocode_addresses <- final %>%
  select(contact_id, person_id, geocode_street, city, state, zip_code)

# replace NA zipcodes with empty string
geocode_addresses$zip_code <- as.character(geocode_addresses$zip_code)
geocode_addresses$zip_code <- replace_na(geocode_addresses$zip_code, "")

geocode_addresses$country <- "United States" 

# filter out NA addresses (landmarks)
geocode_addresses <- geocode_addresses %>%
  filter(!is.na(geocode_street))

# combine into one address: Street Number, Street Direction, Street Name, City, State, Zip, Country (source: https://stackoverflow.com/questions/7764244/correct-address-format-to-get-the-most-accurate-results-from-google-geocoding-ap)
geocode_addresses <- geocode_addresses %>%
  mutate(address = paste(geocode_street, city, state, zip_code, country, sep=", "))

# manually update addresses that look fully wrong
# stop 771231-850919 is entirely wrong. It's most likely Calle Carona, Green Valley, CA 91390
geocode_addresses$address[(geocode_addresses$contact_id=="771231" & geocode_addresses$person_id=="850919")] <- "39800 calle carona, green valley, ca, 91390, United States"

##### Step 3: geocode #####
# note: does not include landmarks
# geocode_results <- geocode_addresses %>%
#   geocode(address=address,
#           lat="lat",
#           long = "lon",
#           method="google",
#           full_results=TRUE)

# partial_matches <- geocode_results %>%
#   filter(partial_match==TRUE)
# Note: definitions of result types here: https://developers.google.com/maps/documentation/javascript/geocoding
geocode_results_select_columns <- geocode_results %>%
  select(contact_id, person_id, address, lat, lon, formatted_address, partial_match, types, geometry.location_type, postcode_localities)

 
##### Step 4: Manually assign coordinates to Landmark-only stops #####
landmark_coords <- lasd_stops %>%
  filter(landmark_only_flag==1) %>%
  mutate(address = paste(full_address, city, state, zip_code, "United States", sep=", ")) %>%
  select(contact_id, person_id, address, landmark_only_flag) 

landmark_coords$address <- gsub("NA", "", landmark_coords$address, fixed=TRUE)

## manual add coordinates
# landmark_coords$lat[(landmark_coords$contact_id=="" & landmark_coords$person_id=="")] <- 
# landmark_coords$lon[(landmark_coords$contact_id=="" & landmark_coords$person_id=="")] <- 

# 594165-654596: piru st/central ave., unincorporated compton, ca, 90220, United States
# 33.90871248569256, -118.25448670789568 : https://www.google.com/maps/place/W+Piru+St+%26+N+Central+Ave,+Compton,+CA+90222/@33.9086474,-118.2548421,20z/data=!4m10!1m2!2m1!1sW+Piru+St+%26+N+Central+Ave,+Compton,+CA+90222!3m6!1s0x80c2ca531c2788fb:0x3e9842266e5c31ee!8m2!3d33.9086857!4d-118.2544905!15sCixXIFBpcnUgU3QgJiBOIENlbnRyYWwgQXZlLCBDb21wdG9uLCBDQSA5MDIyMpIBDGludGVyc2VjdGlvbuABAA!16s%2Fg%2F11f3g5sx4c?entry=ttu
landmark_coords$lat[(landmark_coords$contact_id=="594165" & landmark_coords$person_id=="654596")] <- 33.90871248569256
landmark_coords$lon[(landmark_coords$contact_id=="594165" & landmark_coords$person_id=="654596")] <- -118.25448670789568

# 626948-690637: citrus pax, unincorporated azusa, ca, , United States
# this could be referencing a passenger on the APU/Citrus College Station (Metro Gold Line) or Citrus Crossing (shopping center)]
# check original data to see if these is used anywhere else
# both are less than 1 mile apart, opted for the station
# 34.136828428033034, -117.89166943883889 : https://www.google.com/maps/place/APU+%2F+Citrus+College+Station/@34.1317932,-117.8981282,16z/data=!4m22!1m15!4m14!1m6!1m2!1s0x80c327c50b6bffff:0xdcd9e9b65e6e104a!2sCitrus+Crossing,+North+Citrus+Avenue,+Azusa,+CA!2m2!1d-117.8919004!2d34.1280092!1m6!1m2!1s0x80c327b84ea423a9:0xfa2bbec686e84324!2sAPU+%2F+Citrus+College+Station,+Azusa,+CA+91702!2m2!1d-117.8917186!2d34.1368109!3m5!1s0x80c327b84ea423a9:0xfa2bbec686e84324!8m2!3d34.1368109!4d-117.8917186!16s%2Fm%2F0bwlkg4?entry=ttu
landmark_coords$lat[(landmark_coords$contact_id=="626948" & landmark_coords$person_id=="690637")] <- 34.136828428033034
landmark_coords$lon[(landmark_coords$contact_id=="626948" & landmark_coords$person_id=="690637")] <- -117.89166943883889

# 698281-769729: rail op center, willowbrook, ca, , United States
# 33.92875614770526, -118.23717434930315: https://www.google.com/maps/place/LACMTA+Rail+Operations+Control,+2000+E+Imperial+Hwy,+Los+Angeles,+CA+90059/@33.9235928,-118.2650693,14z/data=!4m22!1m15!4m14!1m6!1m2!1s0x80c2ca4caad3be23:0xea6c9250c8ac6aae!2sWillowbrook,+CA!2m2!1d-118.2550726!2d33.9169602!1m6!1m2!1s0x80c2cbc6e17e86e5:0xc4b99d8f1b5c4488!2sLACMTA+Rail+Operations+Control,+2000+E+Imperial+Hwy,+Los+Angeles,+CA+90059!2m2!1d-118.2371811!2d33.9288724!3m5!1s0x80c2cbc6e17e86e5:0xc4b99d8f1b5c4488!8m2!3d33.9288724!4d-118.2371811!16s%2Fg%2F11_tn3n8p?entry=ttu
landmark_coords$lat[(landmark_coords$contact_id=="698281" & landmark_coords$person_id=="769729")] <- 33.92875614770526
landmark_coords$lon[(landmark_coords$contact_id=="698281" & landmark_coords$person_id=="769729")] <- -118.23717434930315

# 705243-777550: willowbrook pax, willowbrook, ca, , United States
# Using the Willowbrook - Rosa Parks Station coordinates (it's across from the rail op center above)
# 33.92779471964422, -118.23747474314197
landmark_coords$lat[(landmark_coords$contact_id=="705243" & landmark_coords$person_id=="777550")] <- 33.92779471964422
landmark_coords$lon[(landmark_coords$contact_id=="705243" & landmark_coords$person_id=="777550")] <- -118.23747474314197

# 744066-820937: el monte metrolink station, el monte, ca, 91750, United States
# 34.07698353691694, -118.03601183056902: https://www.google.com/maps/place/El+Monte+Metrolink+Station/@34.0767658,-118.038576,17z/data=!3m1!4b1!4m6!3m5!1s0x80c2d0a73307b241:0x47569f8a0214cc31!8m2!3d34.0767614!4d-118.0360011!16s%2Fg%2F11gd689jxk?entry=ttu
landmark_coords$lat[(landmark_coords$contact_id=="744066" & landmark_coords$person_id=="820937")] <- 34.07698353691694 
landmark_coords$lon[(landmark_coords$contact_id=="744066" & landmark_coords$person_id=="820937")] <- -118.03601183056902

## combine landmarks only with google-geocoded results
all_stops_geocoded <- bind_rows(geocode_results_select_columns, landmark_coords)

## clean up nulls and NAs (where needed)
glimpse(all_stops_geocoded)

all_stops_geocoded <- all_stops_geocoded %>%
  mutate_if(is.list, as.character)

all_stops_geocoded[all_stops_geocoded=="NULL"] <- NA

all_stops_geocoded$landmark_only_flag <- as.integer(ifelse(is.na(all_stops_geocoded$landmark_only_flag), 0, all_stops_geocoded$landmark_only_flag))

glimpse(all_stops_geocoded)

# add back in landmark column for debriefing 
all_stops_geocoded <- all_stops_geocoded %>%
  left_join(select(lasd_stops, c(contact_id, person_id, landmark)), by=c("contact_id", "person_id"))

# clean up column names
all_stops_geocoded <- all_stops_geocoded %>%
  rename(address_type = types,
         location_type = geometry.location_type,
         landmark_only = landmark_only_flag) %>%
  mutate(contact_id = as.character(contact_id),
         person_id = as.character(person_id))

##### Step 5: send results to csv and pgadmin #####
# # Send geocoded results to csv
# write.csv(all_stops_geocoded,"W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\Geo-code\\all_geocoded_lasd_stops.csv", row.names = FALSE)
# final_table <- read.csv("W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\Geo-code\\all_geocoded_lasd_stops.csv", encoding="UTF-8")

# Send geocoded results to pg
table_schema <- "bv_2023"
table_name <- "lasd_youth_arrests_geocoded"
conn <- connect_to_db("bold_vision")

dbWriteTable(conn, c(table_schema, table_name), final_table,
             overwrite = FALSE, row.names = FALSE)

# For column comments
column_names <- names(all_stops_geocoded)
column_comments <- list(
  "Unique Identification",
  "Unique Identification at person level",
  "LASD stop address provided to geocoder",
  "Latitude",
  "Longitude",
  "Final address used by geocoder - can change from provided address; Is the address represented by the Latitude/Longitude coordinates.",
  "Provided by geocoder - flags if address only had a partial match",
  "Provided by geocoder - address type (e.g., street_address, premise, intersection, etc.). More details here: https://developers.google.com/maps/documentation/javascript/geocoding#GeocodingAddressTypes",
  "Provided by geocoder - location type (e.g., ROOFTOP, RANGE_INTERPOLATED, GEOMETRIC_CENTER, or APPROXIMATE). More details here: https://developers.google.com/maps/documentation/javascript/geocoding#GeocodingResponses",
  "Provided by geocoder - all the localities contained in a postal code; only present when the postal code contains multiple localities.",
  "Flag added to landmark-only stops, i.e., where the only street information available is recorded in the landmark column.",
  "Additional location information if stop occurred at a landmark."
)

for (i in seq_along(column_names)) {
  column_comment <- column_comments[[i]]
  
  column_comment_sql <- paste0("COMMENT ON COLUMN ",
                               table_schema, ".", table_name, ".", column_names[[i]]," IS '", column_comment, "';")
  
  dbSendQuery(conn, column_comment_sql)
}

table_comment <- "Geocoded results for 181 LASD stops that originally failed to join to a SPA. Landmark-only stops were manually geocoded. R Script: W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\Geo-code\\geocode_lasd_stops.R."
table_comment_sql <- paste0("COMMENT ON TABLE ", table_schema, ".", table_name,  " IS '", table_comment,"';")

dbSendQuery(conn, table_comment_sql)

dbDisconnect(conn)

##### SQL for geom columns #####

# alter table bv_2023.lasd_youth_arrests_geocoded add column geom Geometry('POINT', '3310');
# update bv_2023.lasd_youth_arrests_geocoded set geom = ST_SetSRID(ST_TRANSFORM(ST_SetSRID(st_point(lon,lat), 4326), 3310), 3310);
# create index lasd_youth_arrests_coords_geom_idx on bv_2023.lasd_youth_arrests_geocoded using gist(geom);
# vacuum analyze bv_2023.lasd_youth_arrests_geocoded;
