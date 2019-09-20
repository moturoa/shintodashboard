

# library(sf)
# library(leaflet)
# library(dplyr)


woning_productie_map <- function(jaar_range, database){
  
  project_key <- select(database, project_naam, geovlak) %>% distinct
  
  areas <- structure(project_key$geovlak, class="WKB") %>%
    st_as_sfc(., EWKB=TRUE) %>%
    st_transform(., crs = 4326) %>% 
    data.frame(geometry = .) %>%
    cbind(project_key)
  
  tab <- filter(database, 
                opleverjaar >= jaar_range[1],
                opleverjaar <= jaar_range[2]) %>%
    group_by(project_naam) %>%
    summarize(aantal = sum(aantalwoningen, na.rm=TRUE))
  
  df <- left_join(areas, tab, by="project_naam")
  
  pal <- colorBin("viridis", 
                  domain = df$aantal, 
                  reverse = TRUE,
                  bins = 20)
  
  popups <-  paste0("<strong>",df$project_naam,"</strong>", 
                    "<br>Aantal woningen (totaal):", 
                    df$aantal) %>% 
    lapply(HTML)
  
  
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(weight=1, data=df$geometry,
                fillOpacity = 0.9,
                fillColor = pal(df$aantal),
                label = popups)


}


if(FALSE){
  woning_productie_map(c(2018, 2020), woning_productie)  
}





