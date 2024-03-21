library(rjson)
library(gridtext)
library(glue)
library(shiny)
library(circlize)
library(leaflet)
library(geojsonio)
library(sf)
library(InteractiveComplexHeatmap)

heatmap_list = fromJSON(file="3-hour-day-1-resolution-steps-data.json")
info_list = fromJSON(file="3-hour-day-1-heatmap-additional-data.json")
geojson_data = geojson_read("sectors.geojson")

col_fun = colorRamp2(c(0, 3, 6), c("skyblue", "gold", "red"))

get_heatmap = function(heatmap_list, iteration_step) {
  heatmap_data = matrix(
    unlist(heatmap_list[iteration_step]), ncol=12, byrow = TRUE
  )
  rownames(heatmap_data) = info_list$row_name
  colnames(heatmap_data) = info_list$column_name
  
  flight_chosen = info_list$`atfm-measurement`[[iteration_step]][[1]]
  action_chosen = info_list$`atfm-measurement`[[iteration_step]][[2]]
  if (action_chosen == 0) {
    action = "Finalize flight plan"
  } else {
    action = "Shift to next timeslot"
  }

  ht = Heatmap(
    heatmap_data,
    name = "Overcapacity",
    col = col_fun,
    row_title = "Sector",
    column_title = gt_render(
      paste0(
        "<span style='font-size:18pt; color:black'>**Resolution step:** ",
        iteration_step,
        "</span><br><br>",
        "<span style='font-size:14pt; color:black'>**ATFM Measurement:**<br>",
        glue("**Flight**: {flight_chosen}, **Action**: {action}."),
        "</span><br><br>",
        "Timeslot"
      ), 
      r = unit(2, "pt"), 
      padding = unit(c(2, 2, 2, 2), "pt")
    ),
    cluster_rows = FALSE,
    show_row_dend = FALSE,
    row_names_gp = gpar(fontsize = 9),
    show_column_names = FALSE,
    cluster_columns = FALSE,
    show_column_dend = FALSE,
    heatmap_legend_param = list(
      legend_height = unit(6, "cm"),
      legend_width = unit(3, "cm"),
      labels_gp = gpar(font = 6),
      title_position = "leftcenter-rot"
    )
  )
  ht = draw(ht)
}

get_sector_color = function(sector_name, overcapacity_info) {
  if (sector_name %in% names(overcapacity_info)) {
    return(unname(col_fun(overcapacity_info[sector_name])))
  } else {
    return(col_fun(0))
  }
}

click_action = function(df, input, output, session) {
  if(is.null(df)) {
    map = leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 115, lat = 8, zoom = 3)
    
    output$myMap = renderLeaflet(map)
  } else {
    heatmap_data = matrix(
      unlist(heatmap_list[input$integer]), ncol=12, byrow = TRUE
    )
    rownames(heatmap_data) = info_list$row_name
    colnames(heatmap_data) = info_list$column_name
    
    row_index_name = info_list$row_name[df$row_index]
    column_index_name = info_list$column_name[df$column_index]
    
    overcapacity_info = heatmap_data[, column_index_name]
    
    # Create a map object, with initial location set to the Asia-Pacific region
    map = leaflet() %>%
      addTiles() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 115, lat = 8, zoom = 4)
    
    for (sector in geojson_data$features) {
      sector_name = sector$properties$index
      color = get_sector_color(sector_name, overcapacity_info)
      
      if (sector_name == row_index_name) {
        fill_opacity = 10
        weight = 5
        opacity = 1
      } else {
        fill_opacity = 0
        weight = 5
        opacity = 1
      }
      
      map = addGeoJSON(
        map,
        geojson = sector,
        stroke = TRUE,
        color = color,
        fillOpacity = fill_opacity,
        weight = weight,
        opacity = opacity,
        data = sector
      )
    }
    
    output$myMap = renderLeaflet(map)
  }
}

server = function(input, output, session) {
  observeEvent(
    input$integer,
    InteractiveComplexHeatmapWidget(
      input, output, session,
      get_heatmap(heatmap_list, input$integer),
      width1 = 800,
      height1 = 800,
      output_id = "heatmap_output",
      response = "click",
      output_ui = leafletOutput('myMap', width = "200%", height = 800),
      click_action = click_action
    )
  )
}