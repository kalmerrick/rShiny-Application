# Donut Plot ####
plot.donut <- function(data, name, xAxisTitle = 'Category', yAxisTitle = 'Count', dp = 0, optionalCategoryName = NULL, colorNo = 1, formatType = 'Numeric', margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  data[is.na(data[,1]),1] <- 'NA'
  data[data[,1] == '',1] <- '(blank)'
  plot_ly() %>%
    add_trace(
      name = name,
      labels = data[,1],
      values = data[,2],
      sort = FALSE,
      textposition = 'outside',
      textinfo = 'label+percent',
      type = 'pie',
      hole = 0.5,
      showlegend = FALSE,
      hoverinfo = 'text',
      text = paste0(
        paste0('</br><b>', if(is.null(optionalCategoryName)) {data[,1]} else {optionalCategoryName}, '</b>',
               '</br>', yAxisTitle, ': ', formatC(if(formatType == 'Percent') {data[,2]*100} else{data[,2]}, digits = dp, format = 'f', big.mark = ','), ifelse(formatType == 'Percent', '%', '')
        ),
        if(yAxisTitle %in% c('Count', 'Persons')) {paste0('</br>Percent: ', round(data[,2]/sum(data[,2])*100,1), '%')} else {''}
      )
    ) %>%
    layout(
      title = name,
      margin = margin
    ) %>%
    config(displayModeBar = F)
}

# Pie Plot ####
plot.pie <- function(data, name, xAxisTitle = 'Category', yAxisTitle = 'Count', dp = 0, optionalCategoryName = NULL, colorNo = 1, formatType = 'Numeric', margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  data[is.na(data[,1]),1] <- 'NA'
  data[data[,1] == '',1] <- '(blank)'
  plot_ly() %>%
    add_trace(
      name = name,
      labels = data[,1],
      values = data[,2],
      sort = FALSE,
      textposition = 'inside',
      textinfo = 'label+percent',
      type = 'pie',
      hoverinfo = 'text',
      text = paste0(
        paste0('</br><b>', if(is.null(optionalCategoryName)) {data[,1]} else {optionalCategoryName}, '</b>',
               '</br>', yAxisTitle, ': ', formatC(if(formatType == 'Percent') {data[,2]*100} else{data[,2]}, digits = dp, format = 'f', big.mark = ','), ifelse(formatType == 'Percent', '%', '')
        ),
        if(yAxisTitle %in% c('Count', 'Persons')) {paste0('</br>Percent: ', round(data[,2]/sum(data[,2])*100,1), '%')} else {''}
      )
    ) %>%
    layout(
      title = name,
      margin = margin
    ) %>%
    config(displayModeBar = F)
}

# Bar plot - Single Vertical ####
plot.bar.vert <- function(data, name, xAxisTitle = 'Category', yAxisTitle = 'Count', dp = 0, optionalCategoryName = NULL, colorNo = 1, formatType = 'Numeric', 
                          margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  data[is.na(data[,1]),1] <- 'NA'
  data[data[,1] == '',1] <- '(blank)'
  plot_ly() %>%
    add_trace(
      name = name,
      x = sapply(data[,1], as.character),
      y = data[,2],
      type = 'bar',
      marker = list(color = colorSet[colorNo]),
      hoverinfo = 'text',
      text = paste0(
        paste0('</br><b>', if(is.null(optionalCategoryName)) {data[,1]} else {optionalCategoryName}, '</b>',
               '</br>', yAxisTitle, ': ', formatC(if(formatType == 'Percent') {data[,2]*100} else{data[,2]}, digits = dp, format = 'f', big.mark = ','), ifelse(formatType == 'Percent', '%', '')
        ),
        if(yAxisTitle %in% c('Count', 'Persons')) {paste0('</br>Percent: ', round(data[,2]/sum(data[,2])*100,1), '%')} else {''}
      )
    ) %>%
    add_annotations(
      x = data[,1],
      y = data[,2],
      text = paste0(formatC(if(formatType == 'Percent') {data[,2]*100} else{data[,2]}, digits = dp, format = 'f', big.mark = ','), ifelse(formatType == 'Percent', '%', '')),
      xref = 'x',
      yref = 'y',
      showarrow = FALSE,
      yanchor = 'bottom'
    ) %>%
    layout(
      title = name,
      xaxis = list(title = xAxisTitle, categoryorder = 'trace'),
      yaxis = list(title = yAxisTitle, tickformat = if(formatType == 'Percent') {'%'} else {paste0(',.', dp, 'f')}),
      margin = margin
    ) %>%
    config(displayModeBar = F)
}
# Bar plot - Single Vertical ####
plot.bar.vert <- function(data, name, xAxisTitle = 'Category', yAxisTitle = 'Count', dp = 0, optionalCategoryName = NULL, colorNo = 1, formatType = 'Numeric', 
                          margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  data[is.na(data[,1]),1] <- 'NA'
  data[data[,1] == '',1] <- '(blank)'
  plot_ly() %>%
    add_trace(
      name = name,
      x = sapply(data[,1], as.character),
      y = data[,2],
      type = 'bar',
      marker = list(color = colorSet[colorNo]),
      hoverinfo = 'text',
      text = paste0(
        paste0('</br><b>', if(is.null(optionalCategoryName)) {data[,1]} else {optionalCategoryName}, '</b>',
               '</br>', yAxisTitle, ': ', formatC(if(formatType == 'Percent') {data[,2]*100} else{data[,2]}, digits = dp, format = 'f', big.mark = ','), ifelse(formatType == 'Percent', '%', '')
        ),
        if(yAxisTitle %in% c('Count', 'Persons')) {paste0('</br>Percent: ', round(data[,2]/sum(data[,2])*100,1), '%')} else {''}
      )
    ) %>%
    add_annotations(
      x = data[,1],
      y = data[,2],
      text = paste0(formatC(if(formatType == 'Percent') {data[,2]*100} else{data[,2]}, digits = dp, format = 'f', big.mark = ','), ifelse(formatType == 'Percent', '%', '')),
      xref = 'x',
      yref = 'y',
      showarrow = FALSE,
      yanchor = 'bottom'
    ) %>%
    layout(
      title = name,
      xaxis = list(title = xAxisTitle, categoryorder = 'trace'),
      yaxis = list(title = yAxisTitle, tickformat = if(formatType == 'Percent') {'%'} else {paste0(',.', dp, 'f')}),
      margin = margin
    ) %>%
    config(displayModeBar = F)
}

# Bar plot - Multiple Vertical ####
plot.bar.vert.multi <- function(data, new.colnames, Title, xAxisTitle = 'Category', yAxisTitle = 'Count', thirdLabelTitle = 'Proportion', dp = 0, formatType = 'Numeric', 
                                margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  data[is.na(data[,1]),1] <- 'NA'
  data[data[,1] == '',1] <- '(blank)'
  
  p <- plot_ly()
  
  for(i in 1:length(new.colnames)) {
    p <- p %>%
      add_trace(
        name = new.colnames[i],
        x = sapply(data[,1], as.character),
        y = data[,new.colnames[i]],
        type = 'bar',
        marker = list(color = colorSet[i]),
        hoverinfo = 'text',
        text = paste0(
          paste0('</br><b>', new.colnames[i], '</b>',
                 '</br>', xAxisTitle, ': ', data[,1],
                 '</br>', yAxisTitle, ': ', 
                 formatC(if(formatType == 'Percent') {data[,new.colnames[i]]*100} else{data[,new.colnames[i]]}, digits = dp, format = 'f', big.mark = ','), 
                 ifelse(formatType == 'Percent', '%', '')
          ),
          if(thirdLabelTitle == 'Proportion') {paste0('</br>', thirdLabelTitle, ': ', round(data[,new.colnames[i]]/sum(data[,new.colnames[i]])*100,1), '%')} else {''}
        )
      )
  }
  
  p %>%
    layout(
      title = Title,
      xaxis = list(title = xAxisTitle, categoryorder = 'trace'),
      yaxis = list(title = yAxisTitle, tickformat = if(formatType == 'Percent') {'%'} else {paste0(',.', dp, 'f')}),
      hovermode = 'compare',
      margin = margin
    ) %>%
    config(displayModeBar = F)
}

# Bar plot - Single Horizontal ####
plot.bar.horiz <- function(data, name, xAxisTitle = 'Count', dp = 0, optionalCategoryName = NULL, colorNo = 1, 
                           margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  data[is.na(data[,1]),1] <- 'NA'
  data[data[,1] == '',1] <- '(blank)'
  plot_ly() %>%
    add_trace(
      name = name,
      x = data[,2],
      y = sapply(data[,1], as.character),
      type = 'bar',
      orientation = 'h',
      marker = list(color = colorSet[colorNo]),
      hoverinfo = 'text',
      text = paste0(
        paste0('</br><b>', if(is.null(optionalCategoryName)) {data[,1]} else {optionalCategoryName}, '</b>',
               '</br>', xAxisTitle, ': ', formatC(data[,2], digits = dp, format = 'f', big.mark = ','), ifelse(xAxisTitle == 'Percent', '%', '')
        ),
        if(xAxisTitle %in% c('Count', 'Persons')) {paste0('</br>Percent: ', round(data[,2]/sum(data[,2])*100,1), '%')} else {''}
      )
    ) %>%
    add_annotations(
      x = data[,2],
      y = data[,1],
      text = formatC(data[,2], digits = dp, format = 'f', big.mark = ','),
      xref = 'x',
      yref = 'y',
      showarrow = FALSE,
      xanchor = 'left'
    ) %>%
    layout(
      title = name,
      xaxis = list(title = xAxisTitle, tickformat = paste0(',.', dp, 'f')),
      yaxis = list(title = '', categoryorder = 'trace'),
      margin = margin
    ) %>%
    config(displayModeBar = F)
  
}

# Bar plot - Multiple Horizontal ####
plot.bar.horiz.multi <- function(data, new.colnames, Title, xAxisTitle = 'Count', yAxisTitle = 'Category', thirdLabelTitle = 'Proportion', dp = 0, formatType = 'Numeric', 
                                 margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  data[is.na(data[,1]),1] <- 'NA'
  data[data[,1] == '',1] <- '(blank)'
  
  p <- plot_ly()
  
  for (i in 1:length(new.colnames)) {
    p <- p %>%
      add_trace(
        name = new.colnames[i],
        x = data[,new.colnames[i]],
        y = sapply(data[,1], as.character),
        type = 'bar',
        orientation = 'h',
        marker = list(color = colorSet[i]),
        hoverinfo = 'text',
        text = paste0(
          paste0('</br><b>', new.colnames[i], '</b>',
                 '</br>', yAxisTitle, ': ', data[,1],
                 '</br>', xAxisTitle, ': ', formatC(if(formatType == 'Percent') {data[,new.colnames[i]]*100} else{data[,new.colnames[i]]}, digits = dp, format = 'f', big.mark = ','), 
                 ifelse(formatType == 'Percent', '%', '')
          ),
          if(thirdLabelTitle == 'Proportion') {paste0('</br>', thirdLabelTitle, ': ', round(data[,new.colnames[i]]/sum(data[,new.colnames[i]])*100,1), '%')} else {''}
        )
      )
  } 
  
  p %>%
    layout(
      title = Title,
      xaxis = list(title = xAxisTitle, tickformat = if(formatType == 'Percent') {'%'} else {paste0(',.', dp, 'f')}),
      yaxis = list(title = '', categoryorder = 'trace'),
      hovermode = 'closest',
      margin = margin
    ) %>%
    config(displayModeBar = F)
  
}

# Leaflet Choropleth ####
leaflet.choropleth <- function(LabelData, ShapeFile, MergeBy.x, MergeBy.y, LabelName, dp = 0, palette = 'YlOrRd', bins, formatType = 'Numeric') {
  new.sp <- copy(ShapeFile)
  sp.data <- copy(ShapeFile@data)
  sp.data <- merge(sp.data, LabelData, by.x = MergeBy.x, by.y = MergeBy.y, all.x = TRUE, sort = FALSE)
  sp.data <- sp.data[match(ShapeFile@data[, MergeBy.x], sp.data[, MergeBy.x]),]
  sp.data[is.na(sp.data[,names(LabelData)[2]]), names(LabelData)[2]] <- 0
  new.sp@data <- sp.data
  
  pal <- colorBin(palette, domain = sp.data[,2], bins = bins)
  labels <- sprintf(
    paste0('<b>%s</b><br/>%s', ifelse(formatType == 'Percent', '%% ', ' '), LabelName),
    sp.data[,MergeBy.x], formatC(if(formatType == 'Percent') {sp.data[,names(LabelData)[2]]*100} else {sp.data[,names(LabelData)[2]]}, digits = dp, format = 'f', big.mark = ',')
  ) %>% lapply(htmltools::HTML)
  
  leaflet(new.sp) %>%
    addTiles() %>%
    addPolygons(
      color = 'white',
      dashArray = '3',
      weight = 2,
      opacity = 1,
      fillColor = pal(sp.data[,names(LabelData)[2]]),
      fillOpacity = 0.5,
      highlight = highlightOptions(
        weight = 5,
        color = '#666',
        dashArray = '',
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list('font-weight' = 'normal', padding = '3px 8px'),
        textsize = '15px',
        direction = 'auto'
      )
    ) %>%
    addLegend(
      pal = pal,
      values = sp.data[,names(LabelData)[2]],
      opacity = 0.7,
      title = NULL,
      position = 'bottomright'
    )
}

# Add Percent and Total to Data ####
AddPercentAndTotal <- function(data, new.colnames) {
  new.data <- copy(data)
  names(new.data)[-1] <- new.colnames
  new.data$Total <- rowSums(new.data[,-1])
  new.data[,paste0(new.colnames, '%')] <- new.data[,new.colnames]/new.data$Total
  new.data
}

# Line Plot - Single ####
plot.line.single <- function(data, name, xAxisTitle = 'Year', xAxisTitleHover = xAxisTitle, yAxisTitle = 'Count', markers = TRUE, dp = 0, colorNo = 1, formatType = 'Integer', 
                             margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  plot_ly() %>%
    add_trace(
      name = name,
      x = sapply(data[,1], as.character),
      y = data[,2],
      type = 'scatter',
      mode = if(markers == TRUE) {'lines+markers'} else {'lines'},
      line = list(color = colorSet[colorNo]),
      marker = if(markers == TRUE) {list(color = colorSet[colorNo])} else {NULL},
      hoverinfo = 'text',
      text = paste0('</br><b>', name, '</b>',
                    '</br>', xAxisTitleHover, ': ', data[,1],
                    '</br>', yAxisTitle, ': ', formatC(if(formatType == 'Percent') {data[,2]*100} else{data[,2]}, digits = dp, format = 'f', big.mark = ','), ifelse(formatType == 'Percent', '%', '')
      )
    ) %>%
    layout(
      title = name,
      xaxis = list(title = xAxisTitle, categoryorder = 'trace'),
      yaxis = list(title = yAxisTitle, range = c(0, max(data[,2])*1.1), tickformat = if(yAxisTitle == 'Proportion') {'%'} else {paste0(',.', dp, 'f')}),
      hovermode = 'compare',
      margin = margin
    ) %>%
    config(displayModeBar = F)
}

# Line Plot - Multiple ####
plot.line.multi <- function(data, new.colnames, Title, xAxisTitle = 'Year', yAxisTitle = 'Count', thirdLabelTitle = 'Proportion', dp = 0, formatType = 'Numeric', shortAnnotation = FALSE, 
                            margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  p <- plot_ly()
  
  for(i in 1:length(new.colnames)) {
    p <- p %>%
      add_trace(
        name = new.colnames[i],
        x = sapply(data[,1], as.character),
        y = data[,new.colnames[i]],
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = colorSet[i]),
        marker = list(color = colorSet[i]),
        hoverinfo = 'text',
        text = if(shortAnnotation == TRUE) {
          paste0('</br><b>', new.colnames[i], '</b>',
                 '</br>', if(formatType == 'Percent') {paste0(round(data[,new.colnames[i]]*100,dp), '%')} else {formatC(data[,new.colnames[i]], digits = dp, format = 'f', big.mark = ',')}, if(thirdLabelTitle == 'Proportion') {paste0(' (', round(data[,paste0(new.colnames[i], '%')]*100,1), '%)')})
        }
        else {
          paste0('</br><b>', new.colnames[i], '</b>',
                 '</br>', xAxisTitle, ': ', data[,1],
                 '</br>', yAxisTitle, ': ', if(formatType == 'Percent') {paste0(round(data[,new.colnames[i]]*100,dp), '%')} else {formatC(data[,new.colnames[i]], digits = dp, format = 'f', big.mark = ',')},
                 if(thirdLabelTitle == 'Proportion') {paste0('</br>', thirdLabelTitle, ': ', round(data[,paste0(new.colnames[i], '%')]*100,1), '%')} else {})
        }
      )
  }
  p %>%
    layout(
      title = Title,
      xaxis = list(title = xAxisTitle, categoryorder = 'trace'),
      yaxis = list(title = yAxisTitle, range = c(0, max(data[,new.colnames])*1.1), tickformat = if(formatType == 'Percent') {'%'} else {paste0(',.', dp, 'f')}),
      hovermode = 'compare',
      margin = margin
    ) %>%
    config(displayModeBar = F)
}

# Area Plot - Multiple ####
plot.area.multi <- function(data, new.colnames, Title, xAxisTitle = 'Year', yAxisTitle = 'Count', thirdLabelTitle = 'Proportion', dp = 0, shortAnnotation = FALSE, 
                            margin = NULL) {
  colorSet <- c('rgba(31,119,180,1)', 'rgba(255,127,14,1)', 'rgba(44,160,44,1)', 'rgba(214,39,40,1)', 'rgba(148,103,189,1)', 'rgba(140,86,75,1)', 'rgba(227,119,194,1)', 'rgba(127,127,127,1)', 'rgba(188,189,34,1)', 'rgba(23,190,207,1)')
  data <- as.data.frame(data)
  p <- plot_ly()
  
  for(i in 1:length(new.colnames)) {
    p <- p %>%
      add_trace(
        name = new.colnames[i],
        x = sapply(data[,1], as.character),
        y = data[,new.colnames[i]],
        type = 'scatter',
        mode = 'none',
        stackgroup = 'one',
        fillcolor = colorSet[i],
        hoverinfo = 'text',
        text = if(shortAnnotation == TRUE) {
          paste0('</br><b>', new.colnames[i], '</b>',
                 '</br>', formatC(data[,new.colnames[i]], digits = dp, format = 'f', big.mark = ','), ' (', round(data[,paste0(new.colnames[i], '%')]*100,1), '%)')
        }
        else {
          paste0('</br><b>', new.colnames[i], '</b>',
                 '</br>', xAxisTitle, ': ', data[,1],
                 '</br>', yAxisTitle, ': ', formatC(data[,new.colnames[i]], digits = dp, format = 'f', big.mark = ','),
                 '</br>', thirdLabelTitle, ': ', round(data[,paste0(new.colnames[i], '%')]*100,1), '%')
        }
      )
  }
  p %>%
    layout(
      title = Title,
      xaxis = list(title = xAxisTitle, categoryorder = 'trace'),
      yaxis = list(title = yAxisTitle, range = c(0, max(data$Total)*1.1), tickformat = paste0(',.', dp, 'f')),
      hovermode = 'compare',
      margin = margin
    ) %>%
    config(displayModeBar = F)
}

