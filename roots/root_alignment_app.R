library(shiny)
library(shinydashboard)
library(XML)
library(ggplot2)
library(plotly)
library(DT)

# Source utility functions
source("root_alignment_utils.R")

# Helper functions for RSML parsing
parse_rsml <- function(rsml_file) {
  tryCatch({
    # Add debug information
    debug_info <- list()
    debug_info$file_path <- rsml_file
    debug_info$file_exists <- file.exists(rsml_file)
    debug_info$file_size <- if(file.exists(rsml_file)) file.info(rsml_file)$size else 0
    
    if (!file.exists(rsml_file)) {
      return(list(
        error = paste("File does not exist:", rsml_file),
        debug_info = debug_info,
        success = FALSE
      ))
    }
    
    if (debug_info$file_size == 0) {
      return(list(
        error = "File is empty",
        debug_info = debug_info,
        success = FALSE
      ))
    }
    
    # Try to parse XML
    doc <- xmlParse(rsml_file)
    root <- xmlRoot(doc)
    debug_info$xml_root_name <- xmlName(root)
    
    # Check if this is a valid RSML file
    if (xmlName(root) != "rsml") {
      return(list(
        error = paste("Not a valid RSML file. Root element is:", xmlName(root)),
        debug_info = debug_info,
        success = FALSE
      ))
    }
    
    # Extract metadata
    metadata <- list()
    meta_node <- getNodeSet(doc, "//metadata")
    debug_info$metadata_found <- length(meta_node) > 0
    
    if (length(meta_node) > 0) {
      meta_children <- xmlChildren(meta_node[[1]])
      for (child in meta_children) {
        metadata[[xmlName(child)]] <- xmlValue(child)
      }
    }
    
    # Extract plant data - check both //plant and //scene/plant paths
    plant_nodes <- getNodeSet(doc, "//plant")
    if (length(plant_nodes) == 0) {
      plant_nodes <- getNodeSet(doc, "//scene/plant")
    }
    
    debug_info$plant_count <- length(plant_nodes)
    
    if (length(plant_nodes) == 0) {
      return(list(
        error = "No plant nodes found in RSML file",
        debug_info = debug_info,
        success = FALSE
      ))
    }
    
    plants <- list()
    total_roots <- 0
    total_points <- 0
    
    for (i in seq_along(plant_nodes)) {
      plant <- plant_nodes[[i]]
      plant_id <- xmlGetAttr(plant, "ID", default = as.character(i))
      
      # Extract root system
      roots <- list()
      root_nodes <- getNodeSet(plant, ".//root")
      
      for (j in seq_along(root_nodes)) {
        root_node <- root_nodes[[j]]
        root_id <- xmlGetAttr(root_node, "ID", default = as.character(j))
        
        # Extract polyline points
        polyline_nodes <- getNodeSet(root_node, ".//polyline")
        points <- list()
        
        if (length(polyline_nodes) > 0) {
          point_nodes <- getNodeSet(polyline_nodes[[1]], ".//point")
          for (k in seq_along(point_nodes)) {
            pt <- point_nodes[[k]]
            x_attr <- xmlGetAttr(pt, "x")
            y_attr <- xmlGetAttr(pt, "y")
            
            # Validate coordinates
            if (is.null(x_attr) || is.null(y_attr)) {
              next  # Skip invalid points
            }
            
            x <- as.numeric(x_attr)
            y <- as.numeric(y_attr)
            
            if (is.na(x) || is.na(y)) {
              next  # Skip points with invalid coordinates
            }
            
            points[[k]] <- c(x = x, y = y)
            total_points <- total_points + 1
          }
        }
        
        # Extract root properties
        properties <- list()
        property_nodes <- getNodeSet(root_node, ".//properties/*")
        for (prop in property_nodes) {
          name <- xmlName(prop)
          value <- xmlValue(prop)
          properties[[name]] <- value
        }
        
        # Also check for properties as attributes
        prop_attrs <- getNodeSet(root_node, ".//property")
        for (prop in prop_attrs) {
          name <- xmlGetAttr(prop, "name")
          value <- xmlGetAttr(prop, "value")
          if (!is.null(name) && !is.null(value)) {
            properties[[name]] <- value
          }
        }
        
        roots[[root_id]] <- list(
          id = root_id,
          points = points[!sapply(points, is.null)],  # Remove null points
          properties = properties,
          parent = xmlGetAttr(root_node, "parent", default = NA)
        )
        
        total_roots <- total_roots + 1
      }
      
      plants[[plant_id]] <- list(
        id = plant_id,
        roots = roots
      )
    }
    
    debug_info$total_roots <- total_roots
    debug_info$total_points <- total_points
    
    # Free XML memory
    free(doc)
    
    return(list(
      metadata = metadata,
      plants = plants,
      debug_info = debug_info,
      success = TRUE
    ))
  }, error = function(e) {
    return(list(
      error = paste("Parsing error:", as.character(e)),
      debug_info = if(exists("debug_info")) debug_info else list(),
      success = FALSE
    ))
  })
}

# Helper function to extract all root points
extract_root_points <- function(rsml_data) {
  if (is.null(rsml_data) || !rsml_data$success) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }
  
  all_points <- list()
  point_idx <- 1
  
  tryCatch({
    for (plant in rsml_data$plants) {
      if (is.null(plant$roots)) next
      
      for (root in plant$roots) {
        if (is.null(root$points) || length(root$points) == 0) next
        
        for (point in root$points) {
          if (is.null(point) || length(point) != 2) next
          if (is.na(point[1]) || is.na(point[2])) next
          
          all_points[[point_idx]] <- point
          point_idx <- point_idx + 1
        }
      }
    }
    
    if (length(all_points) > 0) {
      # Convert to data frame with error handling
      points_list <- lapply(all_points, function(p) {
        tryCatch({
          data.frame(x = as.numeric(p[1]), y = as.numeric(p[2]))
        }, error = function(e) {
          data.frame(x = numeric(0), y = numeric(0))
        })
      })
      
      # Filter out empty data frames
      points_list <- points_list[sapply(points_list, nrow) > 0]
      
      if (length(points_list) > 0) {
        points_df <- do.call(rbind, points_list)
        # Remove any remaining NA values
        points_df <- points_df[complete.cases(points_df), ]
        return(points_df)
      }
    }
    
    return(data.frame(x = numeric(0), y = numeric(0)))
    
  }, error = function(e) {
    warning(paste("Error extracting root points:", as.character(e)))
    return(data.frame(x = numeric(0), y = numeric(0)))
  })
}

# Helper function to extract root line segments for line rendering
extract_root_segments <- function(rsml_data) {
  if (is.null(rsml_data) || !rsml_data$success) {
    return(data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), 
                     root_id = character(0), plant_id = character(0)))
  }
  
  all_segments <- list()
  segment_idx <- 1
  
  tryCatch({
    for (plant in rsml_data$plants) {
      if (is.null(plant$roots)) next
      
      for (root in plant$roots) {
        if (is.null(root$points) || length(root$points) < 2) next
        
        # Create segments between consecutive points within the same root
        for (i in 2:length(root$points)) {
          p1 <- root$points[[i-1]]
          p2 <- root$points[[i]]
          
          # Check if both points are valid
          if (is.null(p1) || is.null(p2) || length(p1) != 2 || length(p2) != 2) next
          if (is.na(p1[1]) || is.na(p1[2]) || is.na(p2[1]) || is.na(p2[2])) next
          
          all_segments[[segment_idx]] <- data.frame(
            x = as.numeric(p1[1]),
            y = as.numeric(p1[2]),
            xend = as.numeric(p2[1]),
            yend = as.numeric(p2[2]),
            root_id = root$id,
            plant_id = plant$id,
            stringsAsFactors = FALSE
          )
          segment_idx <- segment_idx + 1
        }
      }
    }
    
    if (length(all_segments) > 0) {
      # Combine all segments
      segments_df <- do.call(rbind, all_segments)
      # Remove any remaining NA values
      segments_df <- segments_df[complete.cases(segments_df[,1:4]), ]
      return(segments_df)
    }
    
    return(data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), 
                     root_id = character(0), plant_id = character(0)))
    
  }, error = function(e) {
    warning(paste("Error extracting root segments:", as.character(e)))
    return(data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), 
                     root_id = character(0), plant_id = character(0)))
  })
}

# Simple 2D histogram function
hist2d <- function(x, y, nbins = 50, xlim = NULL, ylim = NULL, plot = FALSE) {
  if (is.null(xlim)) xlim <- range(x)
  if (is.null(ylim)) ylim <- range(y)
  
  x_breaks <- seq(xlim[1], xlim[2], length.out = nbins + 1)
  y_breaks <- seq(ylim[1], ylim[2], length.out = nbins + 1)
  
  x_cut <- cut(x, breaks = x_breaks, include.lowest = TRUE)
  y_cut <- cut(y, breaks = y_breaks, include.lowest = TRUE)
  
  counts <- table(x_cut, y_cut)
  
  return(list(
    x = x_breaks[-length(x_breaks)] + diff(x_breaks)/2,
    y = y_breaks[-length(y_breaks)] + diff(y_breaks)/2,
    counts = as.matrix(counts)
  ))
}

# Transform points function with advanced parameters
transform_points <- function(points, translation_x, translation_y, rotation, scale, 
                           stretch_x = 1.0, stretch_y = 1.0, 
                           keystone_h = 0, keystone_v = 0,
                           shear_x = 0, shear_y = 0) {
  if (nrow(points) == 0) return(points)
  
  # Convert rotation to radians
  theta <- rotation * pi / 180
  
  # Center points
  center_x <- mean(points$x)
  center_y <- mean(points$y)
  
  # Apply transformations in order: centering -> stretching -> shear -> keystone -> rotation -> scale -> translation
  points_centered <- points
  points_centered$x <- points$x - center_x
  points_centered$y <- points$y - center_y
  
  # 1. Apply X/Y stretching
  x_stretch <- points_centered$x * stretch_x
  y_stretch <- points_centered$y * stretch_y
  
  # 2. Apply shear transformation
  # Shear matrix: [1, shear_x; shear_y, 1]
  x_shear <- x_stretch + shear_x * y_stretch
  y_shear <- shear_y * x_stretch + y_stretch
  
  # 3. Apply keystone correction (perspective transformation)
  # Normalize coordinates for keystone calculation
  x_norm <- x_shear / (abs(max(x_shear, na.rm = TRUE)) + 1e-6)
  y_norm <- y_shear / (abs(max(y_shear, na.rm = TRUE)) + 1e-6)
  
  # Apply keystone distortion
  x_keystone <- x_shear * (1 + keystone_h * y_norm)
  y_keystone <- y_shear * (1 + keystone_v * x_norm)
  
  # 4. Apply rotation matrix
  cos_theta <- cos(theta)
  sin_theta <- sin(theta)
  
  x_rot <- x_keystone * cos_theta - y_keystone * sin_theta
  y_rot <- x_keystone * sin_theta + y_keystone * cos_theta
  
  # 5. Apply uniform scale and translate back
  points$x <- x_rot * scale + center_x + translation_x
  points$y <- y_rot * scale + center_y + translation_y
  
  return(points)
}

# Transform segments function (for line rendering) with advanced parameters
transform_segments <- function(segments, translation_x, translation_y, rotation, scale,
                             stretch_x = 1.0, stretch_y = 1.0,
                             keystone_h = 0, keystone_v = 0,
                             shear_x = 0, shear_y = 0) {
  if (nrow(segments) == 0) return(segments)
  
  # Convert rotation to radians
  theta <- rotation * pi / 180
  
  # Calculate center from all points (start and end points)
  all_x <- c(segments$x, segments$xend)
  all_y <- c(segments$y, segments$yend)
  center_x <- mean(all_x)
  center_y <- mean(all_y)
  
  # Helper function to apply all transformations to a point pair
  transform_point <- function(x, y) {
    # Center
    x_c <- x - center_x
    y_c <- y - center_y
    
    # Stretch
    x_s <- x_c * stretch_x
    y_s <- y_c * stretch_y
    
    # Shear
    x_sh <- x_s + shear_x * y_s
    y_sh <- shear_y * x_s + y_s
    
    # Keystone (normalized)
    x_norm <- x_sh / (abs(max(abs(all_x - center_x), na.rm = TRUE)) + 1e-6)
    y_norm <- y_sh / (abs(max(abs(all_y - center_y), na.rm = TRUE)) + 1e-6)
    
    x_k <- x_sh * (1 + keystone_h * y_norm)
    y_k <- y_sh * (1 + keystone_v * x_norm)
    
    # Rotation
    cos_theta <- cos(theta)
    sin_theta <- sin(theta)
    
    x_r <- x_k * cos_theta - y_k * sin_theta
    y_r <- x_k * sin_theta + y_k * cos_theta
    
    # Scale and translate
    x_final <- x_r * scale + center_x + translation_x
    y_final <- y_r * scale + center_y + translation_y
    
    return(c(x_final, y_final))
  }
  
  # Transform start points
  start_transformed <- mapply(transform_point, segments$x, segments$y, SIMPLIFY = FALSE)
  segments$x <- sapply(start_transformed, function(p) p[1])
  segments$y <- sapply(start_transformed, function(p) p[2])
  
  # Transform end points
  end_transformed <- mapply(transform_point, segments$xend, segments$yend, SIMPLIFY = FALSE)
  segments$xend <- sapply(end_transformed, function(p) p[1])
  segments$yend <- sapply(end_transformed, function(p) p[2])
  
  return(segments)
}

# Calculate root length
calculate_root_length <- function(rsml_data) {
  total_length <- 0
  
  for (plant in rsml_data$plants) {
    for (root in plant$roots) {
      if (length(root$points) > 1) {
        for (i in 2:length(root$points)) {
          p1 <- root$points[[i-1]]
          p2 <- root$points[[i]]
          segment_length <- sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
          total_length <- total_length + segment_length
        }
      }
    }
  }
  
  return(total_length)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "RSML Root System Alignment Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("RSML Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Alignment", tabName = "align", icon = icon("sliders-h")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .img-container {
          border: 2px solid #ddd;
          padding: 10px;
          margin: 10px;
          background: white;
        }
      "))
    ),
    
    tabItems(
      # Upload Tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload RSML Files",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            p("Upload your digitized root system data (RSML files) for both time points. RSML files contain all the necessary root structure information extracted from your original images."),
            fluidRow(
              column(6,
                h4("Time Step 1"),
                fileInput("rsml1", "Choose RSML File for Time 1",
                         accept = c(".rsml", ".xml")),
                p("Upload the RSML file containing root structure data for the first time point.", style = "font-size: 12px; color: #666;")
              ),
              column(6,
                h4("Time Step 2"),
                fileInput("rsml2", "Choose RSML File for Time 2",
                         accept = c(".rsml", ".xml")),
                p("Upload the RSML file containing root structure data for the second time point.", style = "font-size: 12px; color: #666;")
              )
            ),
            hr(),
            fluidRow(
              column(6,
                h4("Time Step 1 Data Summary"),
                div(id = "status1", class = "alert alert-info", style = "display: none;", "Processing..."),
                div(class = "img-container",
                    verbatimTextOutput("rsml1_summary"))
              ),
              column(6,
                h4("Time Step 2 Data Summary"),
                div(id = "status2", class = "alert alert-info", style = "display: none;", "Processing..."),
                div(class = "img-container",
                    verbatimTextOutput("rsml2_summary"))
              )
            ),
            
            tags$script(HTML("
              // Add visual feedback for file uploads
              $(document).on('shiny:inputchanged', function(event) {
                if (event.name === 'rsml1') {
                  $('#status1').show().removeClass('alert-success alert-danger').addClass('alert-info').text('Processing Time Step 1...');
                }
                if (event.name === 'rsml2') {
                  $('#status2').show().removeClass('alert-success alert-danger').addClass('alert-info').text('Processing Time Step 2...');
                }
              });
              
              // Update status based on notifications
              $(document).on('shiny:notification', function(event) {
                if (event.message.includes('Time Step 1') && event.type === 'success') {
                  $('#status1').removeClass('alert-info alert-danger').addClass('alert-success').text('✓ Time Step 1 loaded successfully');
                }
                if (event.message.includes('Time Step 1') && event.type === 'error') {
                  $('#status1').removeClass('alert-info alert-success').addClass('alert-danger').text('✗ Error loading Time Step 1');
                }
                if (event.message.includes('Time Step 2') && event.type === 'success') {
                  $('#status2').removeClass('alert-info alert-danger').addClass('alert-success').text('✓ Time Step 2 loaded successfully');
                }
                if (event.message.includes('Time Step 2') && event.type === 'error') {
                  $('#status2').removeClass('alert-info alert-success').addClass('alert-danger').text('✗ Error loading Time Step 2');
                }
              });
            "))
          )
        )
      ),
      
      # Alignment Tab
      tabItem(
        tabName = "align",
        fluidRow(
          box(
            title = "Manual Transformation Controls",
            status = "info",
            solidHeader = TRUE,
            width = 4,
            
            h4("Basic Transformation Controls"),
            sliderInput("translation_x", "X Translation (pixels)",
                       min = -500, max = 500, value = 0, step = 1),
            sliderInput("translation_y", "Y Translation (pixels)",
                       min = -500, max = 500, value = 0, step = 1),
            sliderInput("rotation", "Rotation (degrees)",
                       min = -180, max = 180, value = 0, step = 0.1),
            sliderInput("scale", "Scale Factor",
                       min = 0.5, max = 2.0, value = 1.0, step = 0.01),
            
            hr(),
            
            # Collapsible advanced parameters section
            tags$details(
              tags$summary("Advanced Transformation Parameters", style = "font-weight: bold; font-size: 16px; cursor: pointer;"),
              br(),
              
              h5("Differential Stretching"),
              p("Apply different stretch factors to X and Y axes", style = "font-size: 12px; color: #666;"),
              sliderInput("stretch_x", "X-Axis Stretch Factor",
                         min = 0.5, max = 2.0, value = 1.0, step = 0.01),
              sliderInput("stretch_y", "Y-Axis Stretch Factor",
                         min = 0.5, max = 2.0, value = 1.0, step = 0.01),
              
              br(),
              h5("Keystone Correction"),
              p("Correct perspective distortion (trapezoid to rectangle)", style = "font-size: 12px; color: #666;"),
              sliderInput("keystone_h", "Horizontal Keystone",
                         min = -0.5, max = 0.5, value = 0, step = 0.01),
              sliderInput("keystone_v", "Vertical Keystone",
                         min = -0.5, max = 0.5, value = 0, step = 0.01),
              
              br(),
              h5("Shear Transformation"),
              p("Apply shear distortion to correct skewed images", style = "font-size: 12px; color: #666;"),
              sliderInput("shear_x", "X-Shear (horizontal skew)",
                         min = -0.5, max = 0.5, value = 0, step = 0.01),
              sliderInput("shear_y", "Y-Shear (vertical skew)",
                         min = -0.5, max = 0.5, value = 0, step = 0.01)
            ),
            
            
            hr(),
            
            actionButton("reset_transform", "Reset All", 
                        class = "btn-warning", width = "100%"),
            br(), br(),
            
            h5("Auto-Alignment Options"),
            actionButton("auto_align_fast", "Quick Align (Basic Only)", 
                        class = "btn-info", width = "100%"),
            br(), br(),
            actionButton("auto_align", "Full Align (All Parameters)", 
                        class = "btn-success", width = "100%"),
            br(),
            p("Quick: ~5-10 seconds, basic parameters only", 
              style = "font-size: 11px; color: #666; margin-top: 5px;"),
            p("Full: ~30-60 seconds, optimizes all 10 parameters", 
              style = "font-size: 11px; color: #666; margin-top: -10px;"),
            
            hr(),
            
            h4("Layer Controls"),
            checkboxInput("show_time1", "Show Time 1 Data", value = TRUE),
            checkboxInput("show_time2", "Show Time 2 Data", value = TRUE),
            
            conditionalPanel(
              condition = "input.show_time1 && input.show_time2",
              h5("Z-Order (Layer Ordering)"),
              actionButton("time1_to_front", "Bring Time 1 to Front", 
                          class = "btn-sm btn-outline-primary", width = "100%"),
              br(), br(),
              actionButton("time2_to_front", "Bring Time 2 to Front", 
                          class = "btn-sm btn-outline-info", width = "100%")
            ),
            
            hr(),
            
            h4("Visualization Options"),
            radioButtons("display_mode", "Display Mode:",
                        choices = list("Points Only" = "points",
                                     "Lines Only" = "lines", 
                                     "Lines + Points" = "both"),
                        selected = "lines"),
            sliderInput("point_alpha", "Point Transparency",
                       min = 0.1, max = 1, value = 0.7, step = 0.05),
            sliderInput("point_size", "Point Size",
                       min = 1, max = 5, value = 2, step = 0.5),
            sliderInput("line_width", "Line Width",
                       min = 1, max = 5, value = 2, step = 0.5),
            radioButtons("view_mode", "View Mode:",
                        choices = list("Original Positions" = "original",
                                     "Aligned Overlay" = "overlay",
                                     "Growth Difference" = "difference"),
                        selected = "overlay")
          ),
          
          box(
            title = "Alignment Preview",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("alignment_plot", height = "600px")
          )
        ),
        
        fluidRow(
          box(
            title = "Transformation Parameters",
            status = "info",
            width = 12,
            verbatimTextOutput("transform_params")
          )
        )
      ),
      
      # Analysis Tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Root System Metrics",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            tableOutput("metrics_table")
          ),
          
          box(
            title = "Growth Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotOutput("growth_chart", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Detailed Statistics",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("detailed_stats")
          )
        )
      ),
      
      # Export Tab
      tabItem(
        tabName = "export",
        fluidRow(
          box(
            title = "Export Options",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            h4("Download Results"),
            br(),
            
            fluidRow(
              column(4,
                downloadButton("download_root_plot", "Download Root Structure Plot",
                             class = "btn-primary btn-block"),
                br(),
                downloadButton("download_alignment_plot", "Download Alignment Visualization",
                             class = "btn-primary btn-block")
              ),
              column(4,
                downloadButton("download_transform", "Download Transform Parameters",
                             class = "btn-info btn-block"),
                br(),
                downloadButton("download_stats", "Download Statistics (CSV)",
                             class = "btn-info btn-block")
              ),
              column(4,
                downloadButton("download_report", "Download Analysis Report (PDF)",
                             class = "btn-success btn-block")
              )
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    rsml1_data = NULL,
    rsml2_data = NULL,
    transform_matrix = NULL,
    time1_front = FALSE,  # TRUE if Time 1 should be rendered on top
    time2_front = TRUE    # TRUE if Time 2 should be rendered on top (default)
  )
  
  # RSML file upload handlers
  observeEvent(input$rsml1, {
    req(input$rsml1)
    
    # Show processing notification
    showNotification("Processing Time Step 1 RSML file...", duration = 3)
    
    # Add debug information about the uploaded file
    file_info <- paste("Uploaded file:", input$rsml1$name, 
                      "Size:", file.info(input$rsml1$datapath)$size, "bytes")
    showNotification(file_info, duration = 5)
    
    result <- parse_rsml(input$rsml1$datapath)
    
    if (result$success) {
      values$rsml1_data <- result
      
      # Create detailed success message
      success_msg <- paste(
        "Time Step 1 RSML loaded successfully!",
        "\nPlants:", length(result$plants),
        "\nRoots:", result$debug_info$total_roots,
        "\nData points:", result$debug_info$total_points
      )
      showNotification(success_msg, duration = 8)
    } else {
      # Show detailed error information
      error_msg <- paste("Error loading Time Step 1 RSML:", result$error)
      
      if (!is.null(result$debug_info)) {
        debug_details <- paste(
          "\nFile exists:", result$debug_info$file_exists,
          "\nFile size:", result$debug_info$file_size, "bytes"
        )
        if (!is.null(result$debug_info$xml_root_name)) {
          debug_details <- paste(debug_details, "\nXML root:", result$debug_info$xml_root_name)
        }
        if (!is.null(result$debug_info$plant_count)) {
          debug_details <- paste(debug_details, "\nPlants found:", result$debug_info$plant_count)
        }
        error_msg <- paste(error_msg, debug_details)
      }
      
      showNotification(error_msg, duration = 15)
      values$rsml1_data <- NULL  # Clear any previous data
    }
  })
  
  observeEvent(input$rsml2, {
    req(input$rsml2)
    
    # Show processing notification
    showNotification("Processing Time Step 2 RSML file...", duration = 3)
    
    # Add debug information about the uploaded file
    file_info <- paste("Uploaded file:", input$rsml2$name, 
                      "Size:", file.info(input$rsml2$datapath)$size, "bytes")
    showNotification(file_info, duration = 5)
    
    result <- parse_rsml(input$rsml2$datapath)
    
    if (result$success) {
      values$rsml2_data <- result
      
      # Create detailed success message
      success_msg <- paste(
        "Time Step 2 RSML loaded successfully!",
        "\nPlants:", length(result$plants),
        "\nRoots:", result$debug_info$total_roots,
        "\nData points:", result$debug_info$total_points
      )
      showNotification(success_msg, duration = 8)
    } else {
      # Show detailed error information
      error_msg <- paste("Error loading Time Step 2 RSML:", result$error)
      
      if (!is.null(result$debug_info)) {
        debug_details <- paste(
          "\nFile exists:", result$debug_info$file_exists,
          "\nFile size:", result$debug_info$file_size, "bytes"
        )
        if (!is.null(result$debug_info$xml_root_name)) {
          debug_details <- paste(debug_details, "\nXML root:", result$debug_info$xml_root_name)
        }
        if (!is.null(result$debug_info$plant_count)) {
          debug_details <- paste(debug_details, "\nPlants found:", result$debug_info$plant_count)
        }
        error_msg <- paste(error_msg, debug_details)
      }
      
      showNotification(error_msg, duration = 15)
      values$rsml2_data <- NULL  # Clear any previous data
    }
  })
  
  # RSML data summary outputs
  output$rsml1_summary <- renderPrint({
    if (is.null(values$rsml1_data)) {
      cat("No RSML file loaded for Time Step 1\n")
      cat("Please upload an RSML file using the file input above.\n")
      return()
    }
    
    if (!values$rsml1_data$success) {
      cat("Error loading Time Step 1 RSML file\n")
      cat("=====================================\n")
      cat("Error:", values$rsml1_data$error, "\n")
      
      if (!is.null(values$rsml1_data$debug_info)) {
        cat("\nDebug Information:\n")
        debug <- values$rsml1_data$debug_info
        
        cat(sprintf("File exists: %s\n", debug$file_exists))
        cat(sprintf("File size: %s bytes\n", debug$file_size))
        
        if (!is.null(debug$xml_root_name)) {
          cat(sprintf("XML root element: %s\n", debug$xml_root_name))
        }
        if (!is.null(debug$plant_count)) {
          cat(sprintf("Plants found: %d\n", debug$plant_count))
        }
        if (!is.null(debug$metadata_found)) {
          cat(sprintf("Metadata found: %s\n", debug$metadata_found))
        }
      }
      return()
    }
    
    tryCatch({
      points <- extract_root_points(values$rsml1_data)
      root_count <- sum(sapply(values$rsml1_data$plants, function(p) length(p$roots)))
      total_length <- calculate_root_length(values$rsml1_data)
      
      cat("RSML Data Summary - Time Step 1\n")
      cat("===============================\n")
      cat(sprintf("File parsing: SUCCESS\n"))
      cat(sprintf("Number of plants: %d\n", length(values$rsml1_data$plants)))
      cat(sprintf("Number of roots: %d\n", root_count))
      cat(sprintf("Total data points: %d\n", nrow(points)))
      cat(sprintf("Total root length: %.2f pixels\n", total_length))
      
      # Show debug info from parsing
      if (!is.null(values$rsml1_data$debug_info)) {
        debug <- values$rsml1_data$debug_info
        cat(sprintf("\nFile size: %s bytes\n", debug$file_size))
        if (!is.null(debug$total_roots)) {
          cat(sprintf("Roots processed: %d\n", debug$total_roots))
        }
        if (!is.null(debug$total_points)) {
          cat(sprintf("Points processed: %d\n", debug$total_points))
        }
      }
      
      if (length(values$rsml1_data$metadata) > 0) {
        cat("\nMetadata:\n")
        for (name in names(values$rsml1_data$metadata)) {
          cat(sprintf("  %s: %s\n", name, values$rsml1_data$metadata[[name]]))
        }
      }
      
      # Show coordinate ranges
      if (nrow(points) > 0) {
        cat("\nCoordinate Ranges:\n")
        cat(sprintf("  X: %.1f to %.1f pixels\n", min(points$x), max(points$x)))
        cat(sprintf("  Y: %.1f to %.1f pixels\n", min(points$y), max(points$y)))
      }
      
    }, error = function(e) {
      cat("Error generating summary:", as.character(e), "\n")
      cat("Raw data structure is available but may be malformed.\n")
    })
  })
  
  output$rsml2_summary <- renderPrint({
    if (is.null(values$rsml2_data)) {
      cat("No RSML file loaded for Time Step 2\n")
      cat("Please upload an RSML file using the file input above.\n")
      return()
    }
    
    if (!values$rsml2_data$success) {
      cat("Error loading Time Step 2 RSML file\n")
      cat("=====================================\n")
      cat("Error:", values$rsml2_data$error, "\n")
      
      if (!is.null(values$rsml2_data$debug_info)) {
        cat("\nDebug Information:\n")
        debug <- values$rsml2_data$debug_info
        
        cat(sprintf("File exists: %s\n", debug$file_exists))
        cat(sprintf("File size: %s bytes\n", debug$file_size))
        
        if (!is.null(debug$xml_root_name)) {
          cat(sprintf("XML root element: %s\n", debug$xml_root_name))
        }
        if (!is.null(debug$plant_count)) {
          cat(sprintf("Plants found: %d\n", debug$plant_count))
        }
        if (!is.null(debug$metadata_found)) {
          cat(sprintf("Metadata found: %s\n", debug$metadata_found))
        }
      }
      return()
    }
    
    tryCatch({
      points <- extract_root_points(values$rsml2_data)
      root_count <- sum(sapply(values$rsml2_data$plants, function(p) length(p$roots)))
      total_length <- calculate_root_length(values$rsml2_data)
      
      cat("RSML Data Summary - Time Step 2\n")
      cat("===============================\n")
      cat(sprintf("File parsing: SUCCESS\n"))
      cat(sprintf("Number of plants: %d\n", length(values$rsml2_data$plants)))
      cat(sprintf("Number of roots: %d\n", root_count))
      cat(sprintf("Total data points: %d\n", nrow(points)))
      cat(sprintf("Total root length: %.2f pixels\n", total_length))
      
      # Show debug info from parsing
      if (!is.null(values$rsml2_data$debug_info)) {
        debug <- values$rsml2_data$debug_info
        cat(sprintf("\nFile size: %s bytes\n", debug$file_size))
        if (!is.null(debug$total_roots)) {
          cat(sprintf("Roots processed: %d\n", debug$total_roots))
        }
        if (!is.null(debug$total_points)) {
          cat(sprintf("Points processed: %d\n", debug$total_points))
        }
      }
      
      if (length(values$rsml2_data$metadata) > 0) {
        cat("\nMetadata:\n")
        for (name in names(values$rsml2_data$metadata)) {
          cat(sprintf("  %s: %s\n", name, values$rsml2_data$metadata[[name]]))
        }
      }
      
      # Show coordinate ranges
      if (nrow(points) > 0) {
        cat("\nCoordinate Ranges:\n")
        cat(sprintf("  X: %.1f to %.1f pixels\n", min(points$x), max(points$x)))
        cat(sprintf("  Y: %.1f to %.1f pixels\n", min(points$y), max(points$y)))
      }
      
    }, error = function(e) {
      cat("Error generating summary:", as.character(e), "\n")
      cat("Raw data structure is available but may be malformed.\n")
    })
  })
  
  # Layer control event handlers
  observeEvent(input$time1_to_front, {
    values$time1_front <- TRUE
    values$time2_front <- FALSE
    showNotification("Time 1 data brought to front", duration = 2)
  })
  
  observeEvent(input$time2_to_front, {
    values$time1_front <- FALSE
    values$time2_front <- TRUE
    showNotification("Time 2 data brought to front", duration = 2)
  })
  
  # Reset transform - now includes all advanced parameters
  observeEvent(input$reset_transform, {
    updateSliderInput(session, "translation_x", value = 0)
    updateSliderInput(session, "translation_y", value = 0)
    updateSliderInput(session, "rotation", value = 0)
    updateSliderInput(session, "scale", value = 1.0)
    updateSliderInput(session, "stretch_x", value = 1.0)
    updateSliderInput(session, "stretch_y", value = 1.0)
    updateSliderInput(session, "keystone_h", value = 0)
    updateSliderInput(session, "keystone_v", value = 0)
    updateSliderInput(session, "shear_x", value = 0)
    updateSliderInput(session, "shear_y", value = 0)
  })
  
  # Auto-align functionality with enhanced optimization
  observeEvent(input$auto_align, {
    req(values$rsml1_data, values$rsml2_data)
    
    showModal(modalDialog(
      title = "Enhanced Auto-Align in Progress...",
      div(
        h4("Hierarchical Multi-Stage Optimization"),
        p("Advanced 10-parameter optimization to maximize overlap between Time 1 and Time 2 root systems..."),
        br(),
        div(
          p(strong("Stage 1:"), " Coarse basic parameter search (translation, rotation, scale)", style = "color: #333;"),
          p(strong("Stage 2:"), " Fine-tune basic parameters", style = "color: #333;"),
          p(strong("Stage 3:"), " Individual advanced parameter optimization", style = "color: #333;"),
          p(strong("Stage 4:"), " Joint parameter fine-tuning", style = "color: #333;")
        ),
        br(),
        p("This may take 30-60 seconds for optimal results. The algorithm will automatically:", 
          style = "color: #666; font-size: 12px;"),
        tags$ul(
          tags$li("Search efficiently through expanded parameter space", style = "font-size: 12px;"),
          tags$li("Apply convergence criteria to avoid unnecessary computation", style = "font-size: 12px;"),
          tags$li("Optimize all transformation parameters including advanced controls", style = "font-size: 12px;")
        ),
        br(),
        div(class = "progress progress-striped active",
            div(class = "progress-bar", role = "progressbar", style = "width: 100%")
        )
      ),
      footer = NULL
    ))
    
    # Record start time for performance monitoring
    start_time <- Sys.time()
    
    # Use enhanced hierarchical optimization with all advanced parameters
    alignment_result <- feature_based_align(
      values$rsml1_data, values$rsml2_data, 
      verbose = FALSE,  # Reduced verbosity for UI
      use_advanced_params = TRUE,  # Enable advanced parameter optimization
      convergence_threshold = 0.2  # Reasonable threshold for UI responsiveness
    )
    
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Update ALL sliders with alignment results
    updateSliderInput(session, "translation_x", value = round(alignment_result$translation[1]))
    updateSliderInput(session, "translation_y", value = round(alignment_result$translation[2]))
    updateSliderInput(session, "rotation", value = round(alignment_result$rotation, 1))
    updateSliderInput(session, "scale", value = round(alignment_result$scale, 3))
    
    # Update advanced parameter sliders
    updateSliderInput(session, "stretch_x", value = round(alignment_result$stretch_x, 3))
    updateSliderInput(session, "stretch_y", value = round(alignment_result$stretch_y, 3))
    updateSliderInput(session, "keystone_h", value = round(alignment_result$keystone_h, 3))
    updateSliderInput(session, "keystone_v", value = round(alignment_result$keystone_v, 3))
    updateSliderInput(session, "shear_x", value = round(alignment_result$shear_x, 3))
    updateSliderInput(session, "shear_y", value = round(alignment_result$shear_y, 3))
    
    removeModal()
    
    # Show comprehensive results
    showNotification(
      paste("Enhanced Auto-alignment Complete!",
            sprintf("Final Overlap: %.2f%% (%.1fs optimization)", 
                   alignment_result$overlap_percentage, elapsed_time),
            sprintf("Basic: tx=%d, ty=%d, rot=%.1f°, scale=%.3f",
                   round(alignment_result$translation[1]), 
                   round(alignment_result$translation[2]),
                   alignment_result$rotation, alignment_result$scale),
            sprintf("Advanced: stretch=(%.3f,%.3f), keystone=(%.3f,%.3f), shear=(%.3f,%.3f)",
                   alignment_result$stretch_x, alignment_result$stretch_y,
                   alignment_result$keystone_h, alignment_result$keystone_v,
                   alignment_result$shear_x, alignment_result$shear_y),
            sprintf("Optimization steps: %d", alignment_result$optimization_steps),
            "All transformation parameters have been optimized!",
            sep = "\n"),
      duration = 12
    )
  })
  
  # Fast auto-align functionality (basic parameters only)
  observeEvent(input$auto_align_fast, {
    req(values$rsml1_data, values$rsml2_data)
    
    showModal(modalDialog(
      title = "Quick Auto-Align in Progress...",
      div(
        h4("Fast Basic Parameter Optimization"),
        p("Optimizing translation, rotation, and scale only for quick alignment..."),
        br(),
        div(
          p(strong("Stage 1:"), " Coarse basic parameter search", style = "color: #333;"),
          p(strong("Stage 2:"), " Fine-tune basic parameters", style = "color: #333;")
        ),
        br(),
        p("Advanced parameters will be reset to defaults. Expected time: 5-15 seconds.", 
          style = "color: #666; font-size: 12px;"),
        br(),
        div(class = "progress progress-striped active",
            div(class = "progress-bar", role = "progressbar", style = "width: 100%")
        )
      ),
      footer = NULL
    ))
    
    # Record start time for performance monitoring
    start_time <- Sys.time()
    
    # Reset advanced parameters to defaults before fast alignment
    updateSliderInput(session, "stretch_x", value = 1.0)
    updateSliderInput(session, "stretch_y", value = 1.0)
    updateSliderInput(session, "keystone_h", value = 0)
    updateSliderInput(session, "keystone_v", value = 0)
    updateSliderInput(session, "shear_x", value = 0)
    updateSliderInput(session, "shear_y", value = 0)
    
    # Use hierarchical optimization with basic parameters only
    alignment_result <- feature_based_align(
      values$rsml1_data, values$rsml2_data, 
      verbose = FALSE,  # Reduced verbosity for UI
      use_advanced_params = FALSE,  # Basic parameters only for speed
      convergence_threshold = 0.3  # Slightly looser threshold for speed
    )
    
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    # Update basic parameter sliders only
    updateSliderInput(session, "translation_x", value = round(alignment_result$translation[1]))
    updateSliderInput(session, "translation_y", value = round(alignment_result$translation[2]))
    updateSliderInput(session, "rotation", value = round(alignment_result$rotation, 1))
    updateSliderInput(session, "scale", value = round(alignment_result$scale, 3))
    
    removeModal()
    
    # Show quick results
    showNotification(
      paste("Quick Auto-alignment Complete!",
            sprintf("Overlap: %.2f%% (%.1fs optimization)", 
                   alignment_result$overlap_percentage, elapsed_time),
            sprintf("Basic params: tx=%d, ty=%d, rot=%.1f°, scale=%.3f",
                   round(alignment_result$translation[1]), 
                   round(alignment_result$translation[2]),
                   alignment_result$rotation, alignment_result$scale),
            sprintf("Optimization steps: %d", alignment_result$optimization_steps),
            "Basic transformation parameters optimized.",
            "Use 'Full Align' for advanced parameter optimization.",
            sep = "\n"),
      duration = 8
    )
  })
  
  # Helper function to add plot layers in correct order
  add_plot_layers <- function(p, layers_data) {
    # layers_data should be a list with elements in the order they should be plotted
    # Each element should have: data, type ("points" or "segments"), color, name, etc.
    
    for (layer in layers_data) {
      if (layer$type == "points") {
        p <- p %>%
          add_trace(data = layer$data, x = ~x, y = ~y,
                   type = "scatter", mode = "markers",
                   marker = list(color = layer$color, size = layer$size, opacity = layer$alpha),
                   name = layer$name,
                   hovertemplate = layer$hovertemplate)
      } else if (layer$type == "segments") {
        p <- p %>%
          add_segments(data = layer$data, 
                      x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                      line = list(color = layer$color, width = layer$width),
                      opacity = layer$alpha,
                      name = layer$name,
                      hovertemplate = layer$hovertemplate,
                      customdata = layer$customdata)
      }
    }
    
    return(p)
  }
  
  # Alignment plot
  output$alignment_plot <- renderPlotly({
    # Check if data is available and successfully parsed
    if (is.null(values$rsml1_data) || is.null(values$rsml2_data)) {
      p <- plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = "Please upload RSML files for both time steps", 
                showlegend = FALSE, textfont = list(size = 16)) %>%
        layout(
          xaxis = list(title = "Upload RSML files to see alignment", 
                      showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE)
        )
      return(p)
    }
    
    if (!values$rsml1_data$success || !values$rsml2_data$success) {
      error_msg <- "Error in RSML data:"
      if (!values$rsml1_data$success) error_msg <- paste(error_msg, "\nTime 1:", values$rsml1_data$error)
      if (!values$rsml2_data$success) error_msg <- paste(error_msg, "\nTime 2:", values$rsml2_data$error)
      
      p <- plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = error_msg, 
                showlegend = FALSE, textfont = list(size = 14, color = "red")) %>%
        layout(
          xaxis = list(title = "Fix RSML parsing errors to see alignment", 
                      showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE)
        )
      return(p)
    }
    
    tryCatch({
      # Extract data based on display mode
      if (input$display_mode %in% c("points", "both")) {
        # Extract points for point rendering
        points1 <- extract_root_points(values$rsml1_data)
        points2 <- extract_root_points(values$rsml2_data)
        
        # Transform points2 with all parameters
        points2_transformed <- transform_points(
          points2,
          input$translation_x,
          input$translation_y,
          input$rotation,
          input$scale,
          input$stretch_x,
          input$stretch_y,
          input$keystone_h,
          input$keystone_v,
          input$shear_x,
          input$shear_y
        )
      }
      
      if (input$display_mode %in% c("lines", "both")) {
        # Extract segments for line rendering
        segments1 <- extract_root_segments(values$rsml1_data)
        segments2 <- extract_root_segments(values$rsml2_data)
        
        # Transform segments2 with all parameters
        segments2_transformed <- transform_segments(
          segments2,
          input$translation_x,
          input$translation_y,
          input$rotation,
          input$scale,
          input$stretch_x,
          input$stretch_y,
          input$keystone_h,
          input$keystone_v,
          input$shear_x,
          input$shear_y
        )
      }
    
    # Check if any layers should be shown
    if (!input$show_time1 && !input$show_time2) {
      p <- plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = "No layers selected for display\nEnable Time 1 or Time 2 data in Layer Controls", 
                showlegend = FALSE, textfont = list(size = 16)) %>%
        layout(
          xaxis = list(title = "Enable layers to see data", 
                      showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE)
        )
      return(p)
    }
    
    # Create plot based on view mode with layer controls
    p <- plot_ly() %>%
      layout(
        xaxis = list(title = "X (pixels)", scaleanchor = "y"),
        yaxis = list(title = "Y (pixels)", autorange = "reversed"),
        showlegend = TRUE,
        hovermode = "closest"
      )
    
    # Prepare layers list based on z-order preference and view mode
    layers_to_plot <- list()
    
    # Determine layer order based on z-order settings
    if (values$time1_front) {
      # Time 1 on top, so add Time 2 first, then Time 1
      layer_order <- c("time2", "time1")
    } else {  # time2_front or default
      # Time 2 on top, so add Time 1 first, then Time 2
      layer_order <- c("time1", "time2")
    }
    
    # Build layers list based on view mode
    if (input$view_mode == "original") {
      # Original positions mode
      for (layer_id in layer_order) {
        if (layer_id == "time1" && input$show_time1) {
          # Add Time 1 layers
          if (input$display_mode %in% c("lines", "both") && exists("segments1") && nrow(segments1) > 0) {
            layers_to_plot[[length(layers_to_plot) + 1]] <- list(
              type = "segments",
              data = segments1,
              color = "red",
              width = input$line_width,
              alpha = input$point_alpha,
              name = "Time 1 (lines)",
              hovertemplate = "Time 1<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
              customdata = ~root_id
            )
          }
          if (input$display_mode %in% c("points", "both") && exists("points1") && nrow(points1) > 0) {
            layers_to_plot[[length(layers_to_plot) + 1]] <- list(
              type = "points",
              data = points1,
              color = "red",
              size = input$point_size,
              alpha = input$point_alpha,
              name = "Time 1",
              hovertemplate = "Time 1<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>"
            )
          }
        }
        
        if (layer_id == "time2" && input$show_time2) {
          # Add Time 2 layers (original positions)
          if (input$display_mode %in% c("lines", "both") && exists("segments2") && nrow(segments2) > 0) {
            layers_to_plot[[length(layers_to_plot) + 1]] <- list(
              type = "segments",
              data = segments2,
              color = "blue",
              width = input$line_width,
              alpha = input$point_alpha,
              name = "Time 2 (Original lines)",
              hovertemplate = "Time 2 Original<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
              customdata = ~root_id
            )
          }
          if (input$display_mode %in% c("points", "both") && exists("points2") && nrow(points2) > 0) {
            layers_to_plot[[length(layers_to_plot) + 1]] <- list(
              type = "points",
              data = points2,
              color = "blue",
              size = input$point_size,
              alpha = input$point_alpha,
              name = "Time 2 (Original)",
              hovertemplate = "Time 2 Original<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>"
            )
          }
        }
      }
    } else if (input$view_mode == "overlay") {
      # Overlay mode with transformed Time 2 data
      for (layer_id in layer_order) {
        if (layer_id == "time1" && input$show_time1) {
          # Add Time 1 layers
          if (input$display_mode %in% c("lines", "both") && exists("segments1") && nrow(segments1) > 0) {
            layers_to_plot[[length(layers_to_plot) + 1]] <- list(
              type = "segments",
              data = segments1,
              color = "red",
              width = input$line_width,
              alpha = input$point_alpha,
              name = "Time 1 (lines)",
              hovertemplate = "Time 1<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
              customdata = ~root_id
            )
          }
        }
        
        if (layer_id == "time2" && input$show_time2) {
          # Add Time 2 layers (transformed)
          if (input$display_mode %in% c("lines", "both") && exists("segments2_transformed") && nrow(segments2_transformed) > 0) {
            layers_to_plot[[length(layers_to_plot) + 1]] <- list(
              type = "segments",
              data = segments2_transformed,
              color = "blue",
              width = input$line_width,
              alpha = input$point_alpha,
              name = "Time 2 (Aligned lines)",
              hovertemplate = "Time 2 Aligned<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
              customdata = ~root_id
            )
          }
        }
      }
      
      # Handle points for overlay mode with overlap detection
      if (input$display_mode %in% c("points", "both")) {
        # Find overlapping points (within threshold distance)
        overlap_threshold <- 5  # pixels
        
        if (input$show_time1 && input$show_time2 && exists("points1") && exists("points2_transformed") && 
            nrow(points1) > 0 && nrow(points2_transformed) > 0) {
          
          overlap_mask1 <- rep(FALSE, nrow(points1))
          overlap_mask2 <- rep(FALSE, nrow(points2_transformed))
          
          for (i in 1:nrow(points1)) {
            distances <- sqrt((points1$x[i] - points2_transformed$x)^2 + 
                             (points1$y[i] - points2_transformed$y)^2)
            if (min(distances) < overlap_threshold) {
              overlap_mask1[i] <- TRUE
              overlap_mask2[which.min(distances)] <- TRUE
            }
          }
          
          # Add layers in correct order for overlay visualization
          for (layer_id in layer_order) {
            if (layer_id == "time1") {
              # Non-overlapping Time 1 points
              if (sum(!overlap_mask1) > 0) {
                layers_to_plot[[length(layers_to_plot) + 1]] <- list(
                  type = "points",
                  data = points1[!overlap_mask1, ],
                  color = "red",
                  size = input$point_size,
                  alpha = input$point_alpha,
                  name = "Time 1 only",
                  hovertemplate = "Time 1 only<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>"
                )
              }
            }
            
            if (layer_id == "time2") {
              # Non-overlapping Time 2 points
              if (sum(!overlap_mask2) > 0) {
                layers_to_plot[[length(layers_to_plot) + 1]] <- list(
                  type = "points",
                  data = points2_transformed[!overlap_mask2, ],
                  color = "blue",
                  size = input$point_size,
                  alpha = input$point_alpha,
                  name = "Time 2 only",
                  hovertemplate = "Time 2 only<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>"
                )
              }
            }
          }
          
          # Overlapping points (always on top)
          if (sum(overlap_mask1) > 0) {
            layers_to_plot[[length(layers_to_plot) + 1]] <- list(
              type = "points",
              data = points1[overlap_mask1, ],
              color = "green",
              size = input$point_size + 1,
              alpha = input$point_alpha,
              name = "Overlapped",
              hovertemplate = "Overlapped<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>"
            )
          }
        } else {
          # Show individual layers when only one is enabled
          for (layer_id in layer_order) {
            if (layer_id == "time1" && input$show_time1 && exists("points1") && nrow(points1) > 0) {
              layers_to_plot[[length(layers_to_plot) + 1]] <- list(
                type = "points",
                data = points1,
                color = "red",
                size = input$point_size,
                alpha = input$point_alpha,
                name = "Time 1",
                hovertemplate = "Time 1<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>"
              )
            }
            
            if (layer_id == "time2" && input$show_time2 && exists("points2_transformed") && nrow(points2_transformed) > 0) {
              layers_to_plot[[length(layers_to_plot) + 1]] <- list(
                type = "points",
                data = points2_transformed,
                color = "blue",
                size = input$point_size,
                alpha = input$point_alpha,
                name = "Time 2 (Aligned)",
                hovertemplate = "Time 2 Aligned<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>"
              )
            }
          }
        }
      }
    } else if (input$view_mode == "difference") {
      # Difference view - only show if both layers are enabled and in points mode
      if (input$show_time1 && input$show_time2 && input$display_mode %in% c("points", "both")) {
        if (exists("points1") && exists("points2_transformed") && nrow(points1) > 0 && nrow(points2_transformed) > 0) {
          # Create density grids
          x_range <- range(c(points1$x, points2_transformed$x))
          y_range <- range(c(points1$y, points2_transformed$y))
          
          # Create 2D histogram for difference
          bins <- 50
          h1 <- hist2d(points1$x, points1$y, nbins = bins, 
                      xlim = x_range, ylim = y_range, plot = FALSE)
          h2 <- hist2d(points2_transformed$x, points2_transformed$y, nbins = bins,
                      xlim = x_range, ylim = y_range, plot = FALSE)
          
          diff_matrix <- h2$counts - h1$counts
          
          p <- plot_ly(
            x = h1$x,
            y = h1$y,
            z = diff_matrix,
            type = "heatmap",
            colorscale = list(
              c(0, "red"),
              c(0.5, "white"),
              c(1, "blue")
            ),
            colorbar = list(title = "Growth<br>Difference"),
            hovertemplate = "X: %{x:.1f}<br>Y: %{y:.1f}<br>Difference: %{z}<extra></extra>"
          ) %>%
          layout(
            xaxis = list(title = "X (pixels)"),
            yaxis = list(title = "Y (pixels)", autorange = "reversed"),
            title = "Root Growth Difference Map"
          )
          
          return(p)
        }
      } else {
        # Show message for unavailable difference view
        p <- plot_ly() %>%
          add_text(x = 0.5, y = 0.5, text = "Difference view requires both Time 1 and Time 2 data\nand Points or Both display mode", 
                  showlegend = FALSE, textfont = list(size = 16)) %>%
          layout(
            xaxis = list(title = "Enable both layers and use Points mode", 
                        showgrid = FALSE, showticklabels = FALSE),
            yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE)
          )
        return(p)
      }
    }
    
    # Add all layers to the plot in the correct order
    p <- add_plot_layers(p, layers_to_plot)
    
    # Skip the old plotting logic by returning here
    return(p)
    
    # OLD PLOTTING LOGIC BELOW (keeping for reference, but this won't be executed)
    if (input$view_mode == "original") {
      # Show original positions
      if (input$display_mode %in% c("lines", "both")) {
        # Add line segments for Time 1
        if (nrow(segments1) > 0) {
          p <- p %>%
            add_segments(data = segments1, 
                        x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                        line = list(color = "red", width = input$line_width),
                        opacity = input$point_alpha,
                        name = "Time 1 (lines)",
                        hovertemplate = "Time 1<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
                        customdata = ~root_id)
        }
        
        # Add line segments for Time 2
        if (nrow(segments2) > 0) {
          p <- p %>%
            add_segments(data = segments2, 
                        x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                        line = list(color = "blue", width = input$line_width),
                        opacity = input$point_alpha,
                        name = "Time 2 (Original lines)",
                        hovertemplate = "Time 2 Original<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
                        customdata = ~root_id)
        }
      }
      
      if (input$display_mode %in% c("points", "both")) {
        # Add points
        p <- p %>%
          add_trace(data = points1, x = ~x, y = ~y,
                   type = "scatter", mode = "markers",
                   marker = list(color = "red", size = input$point_size, opacity = input$point_alpha),
                   name = "Time 1",
                   hovertemplate = "Time 1<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>") %>%
          add_trace(data = points2, x = ~x, y = ~y,
                   type = "scatter", mode = "markers",
                   marker = list(color = "blue", size = input$point_size, opacity = input$point_alpha),
                   name = "Time 2 (Original)",
                   hovertemplate = "Time 2 Original<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>")
      }
    } else if (input$view_mode == "overlay") {
      # Show overlay with color coding
      if (input$display_mode %in% c("lines", "both")) {
        # Add line segments for Time 1
        if (nrow(segments1) > 0) {
          p <- p %>%
            add_segments(data = segments1, 
                        x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                        line = list(color = "red", width = input$line_width),
                        opacity = input$point_alpha,
                        name = "Time 1 (lines)",
                        hovertemplate = "Time 1<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
                        customdata = ~root_id)
        }
        
        # Add line segments for Time 2 (transformed)
        if (nrow(segments2_transformed) > 0) {
          p <- p %>%
            add_segments(data = segments2_transformed, 
                        x = ~x, y = ~y, xend = ~xend, yend = ~yend,
                        line = list(color = "blue", width = input$line_width),
                        opacity = input$point_alpha,
                        name = "Time 2 (Aligned lines)",
                        hovertemplate = "Time 2 Aligned<br>Root: %{customdata}<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>",
                        customdata = ~root_id)
        }
      }
      
      if (input$display_mode %in% c("points", "both")) {
        # Find overlapping points (within threshold distance)
        overlap_threshold <- 5  # pixels
        
        overlap_mask1 <- rep(FALSE, nrow(points1))
        overlap_mask2 <- rep(FALSE, nrow(points2_transformed))
        
        if (nrow(points1) > 0 && nrow(points2_transformed) > 0) {
          for (i in 1:nrow(points1)) {
            distances <- sqrt((points1$x[i] - points2_transformed$x)^2 + 
                             (points1$y[i] - points2_transformed$y)^2)
            if (min(distances) < overlap_threshold) {
              overlap_mask1[i] <- TRUE
              overlap_mask2[which.min(distances)] <- TRUE
            }
          }
        }
        
        # Plot non-overlapping points
        if (sum(!overlap_mask1) > 0) {
          p <- p %>%
            add_trace(data = points1[!overlap_mask1, ], x = ~x, y = ~y,
                     type = "scatter", mode = "markers",
                     marker = list(color = "red", size = input$point_size, opacity = input$point_alpha),
                     name = "Time 1 only",
                     hovertemplate = "Time 1 only<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>")
        }
        
        if (sum(!overlap_mask2) > 0) {
          p <- p %>%
            add_trace(data = points2_transformed[!overlap_mask2, ], x = ~x, y = ~y,
                     type = "scatter", mode = "markers",
                     marker = list(color = "blue", size = input$point_size, opacity = input$point_alpha),
                     name = "Time 2 only",
                     hovertemplate = "Time 2 only<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>")
        }
        
        # Plot overlapping points
        if (sum(overlap_mask1) > 0) {
          p <- p %>%
            add_trace(data = points1[overlap_mask1, ], x = ~x, y = ~y,
                     type = "scatter", mode = "markers",
                     marker = list(color = "green", size = input$point_size + 1, opacity = input$point_alpha),
                     name = "Overlapped",
                     hovertemplate = "Overlapped<br>X: %{x:.1f}<br>Y: %{y:.1f}<extra></extra>")
        }
      }
    } else if (input$view_mode == "difference") {
      # Show difference map (only works with points for now)
      if (input$display_mode %in% c("points", "both")) {
        # Create a heatmap showing areas of growth
        if (nrow(points1) > 0 && nrow(points2_transformed) > 0) {
          # Create density grids
          x_range <- range(c(points1$x, points2_transformed$x))
          y_range <- range(c(points1$y, points2_transformed$y))
          
          # Create 2D histogram for difference
          bins <- 50
          h1 <- hist2d(points1$x, points1$y, nbins = bins, 
                      xlim = x_range, ylim = y_range, plot = FALSE)
          h2 <- hist2d(points2_transformed$x, points2_transformed$y, nbins = bins,
                      xlim = x_range, ylim = y_range, plot = FALSE)
          
          diff_matrix <- h2$counts - h1$counts
          
          p <- plot_ly(
            x = h1$x,
            y = h1$y,
            z = diff_matrix,
            type = "heatmap",
            colorscale = list(
              c(0, "red"),
              c(0.5, "white"),
              c(1, "blue")
            ),
            colorbar = list(title = "Growth<br>Difference"),
            hovertemplate = "X: %{x:.1f}<br>Y: %{y:.1f}<br>Difference: %{z}<extra></extra>"
          ) %>%
          layout(
            xaxis = list(title = "X (pixels)"),
            yaxis = list(title = "Y (pixels)", autorange = "reversed"),
            title = "Root Growth Difference Map"
          )
        }
      } else {
        # For lines-only mode in difference view, show message
        p <- p %>%
          add_text(x = 0.5, y = 0.5, text = "Difference view currently only available for points mode", 
                  showlegend = FALSE, textfont = list(size = 16)) %>%
          layout(
            xaxis = list(title = "Switch to Points or Both mode for difference view", 
                        showgrid = FALSE, showticklabels = FALSE),
            yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE)
          )
      }
    }
    
    p
    }, error = function(e) {
      # Handle errors in plot generation
      p <- plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = paste("Error generating plot:", as.character(e)), 
                showlegend = FALSE, textfont = list(size = 14, color = "red")) %>%
        layout(
          xaxis = list(title = "Plot generation error", 
                      showgrid = FALSE, showticklabels = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE)
        )
      return(p)
    })
  })
  
  # Transform parameters output - now includes all parameters
  output$transform_params <- renderPrint({
    cat("Current Transformation Parameters:\n")
    cat("=================================\n")
    cat("Basic Parameters:\n")
    cat(sprintf("  X Translation: %d pixels\n", input$translation_x))
    cat(sprintf("  Y Translation: %d pixels\n", input$translation_y))
    cat(sprintf("  Rotation: %.1f degrees\n", input$rotation))
    cat(sprintf("  Uniform Scale: %.2fx\n", input$scale))
    
    # Only show advanced parameters if they differ from defaults
    advanced_params_used <- FALSE
    if (input$stretch_x != 1.0 || input$stretch_y != 1.0) {
      if (!advanced_params_used) { cat("\nAdvanced Parameters:\n"); advanced_params_used <- TRUE }
      cat(sprintf("  X-Axis Stretch: %.2fx\n", input$stretch_x))
      cat(sprintf("  Y-Axis Stretch: %.2fx\n", input$stretch_y))
    }
    if (input$keystone_h != 0 || input$keystone_v != 0) {
      if (!advanced_params_used) { cat("\nAdvanced Parameters:\n"); advanced_params_used <- TRUE }
      cat(sprintf("  Horizontal Keystone: %.3f\n", input$keystone_h))
      cat(sprintf("  Vertical Keystone: %.3f\n", input$keystone_v))
    }
    if (input$shear_x != 0 || input$shear_y != 0) {
      if (!advanced_params_used) { cat("\nAdvanced Parameters:\n"); advanced_params_used <- TRUE }
      cat(sprintf("  X-Shear: %.3f\n", input$shear_x))
      cat(sprintf("  Y-Shear: %.3f\n", input$shear_y))
    }
    
    if (!advanced_params_used) {
      cat("\n(Using basic parameters only)\n")
    }
  })
  
  # Metrics table
  output$metrics_table <- renderTable({
    req(values$rsml1_data, values$rsml2_data)
    
    length1 <- calculate_root_length(values$rsml1_data)
    length2 <- calculate_root_length(values$rsml2_data)
    
    # Calculate spatial distribution
    spatial1 <- calculate_spatial_distribution(values$rsml1_data)
    spatial2 <- calculate_spatial_distribution(values$rsml2_data)
    
    # Count roots
    root_count1 <- sum(sapply(values$rsml1_data$plants, function(p) length(p$roots)))
    root_count2 <- sum(sapply(values$rsml2_data$plants, function(p) length(p$roots)))
    
    # Calculate overlay stats
    overlay_stats <- calculate_overlay_stats(
      values$rsml1_data, 
      values$rsml2_data,
      list(
        translation_x = input$translation_x,
        translation_y = input$translation_y,
        rotation = input$rotation,
        scale = input$scale
      )
    )
    
    data.frame(
      Metric = c("Total Root Length (Time 1)",
                "Total Root Length (Time 2)",
                "Root Length Change",
                "Growth Rate (%)",
                "Number of Roots (Time 1)",
                "Number of Roots (Time 2)",
                "Spatial Spread X (Time 1)",
                "Spatial Spread X (Time 2)",
                "Overlay Percentage"),
      Value = c(sprintf("%.2f pixels", length1),
               sprintf("%.2f pixels", length2),
               sprintf("%.2f pixels", length2 - length1),
               sprintf("%.1f%%", ((length2 - length1) / length1) * 100),
               sprintf("%d", root_count1),
               sprintf("%d", root_count2),
               sprintf("%.1f pixels", spatial1$spread_x),
               sprintf("%.1f pixels", spatial2$spread_x),
               sprintf("%.1f%%", overlay_stats$overlap_percentage))
    )
  })
  
  # Growth chart
  output$growth_chart <- renderPlot({
    req(values$rsml1_data, values$rsml2_data)
    
    length1 <- calculate_root_length(values$rsml1_data)
    length2 <- calculate_root_length(values$rsml2_data)
    
    df <- data.frame(
      TimeStep = c("Time 1", "Time 2"),
      Length = c(length1, length2)
    )
    
    ggplot(df, aes(x = TimeStep, y = Length, fill = TimeStep)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Time 1" = "red", "Time 2" = "blue")) +
      labs(title = "Root System Growth",
           x = "Time Step",
           y = "Total Root Length (pixels)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Detailed statistics table
  output$detailed_stats <- renderDT({
    req(values$rsml1_data, values$rsml2_data)
    
    # Compile detailed root-by-root statistics
    stats_list <- list()
    idx <- 1
    
    # Process Time 1 roots
    for (plant in values$rsml1_data$plants) {
      for (root in plant$roots) {
        if (length(root$points) > 1) {
          # Calculate root length
          root_length <- 0
          for (i in 2:length(root$points)) {
            p1 <- root$points[[i-1]]
            p2 <- root$points[[i]]
            root_length <- root_length + sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
          }
          
          stats_list[[idx]] <- data.frame(
            TimeStep = "Time 1",
            PlantID = plant$id,
            RootID = root$id,
            Length = round(root_length, 2),
            NumPoints = length(root$points),
            ParentRoot = ifelse(is.na(root$parent), "Primary", root$parent)
          )
          idx <- idx + 1
        }
      }
    }
    
    # Process Time 2 roots
    for (plant in values$rsml2_data$plants) {
      for (root in plant$roots) {
        if (length(root$points) > 1) {
          # Calculate root length
          root_length <- 0
          for (i in 2:length(root$points)) {
            p1 <- root$points[[i-1]]
            p2 <- root$points[[i]]
            root_length <- root_length + sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
          }
          
          stats_list[[idx]] <- data.frame(
            TimeStep = "Time 2",
            PlantID = plant$id,
            RootID = root$id,
            Length = round(root_length, 2),
            NumPoints = length(root$points),
            ParentRoot = ifelse(is.na(root$parent), "Primary", root$parent)
          )
          idx <- idx + 1
        }
      }
    }
    
    if (length(stats_list) > 0) {
      detailed_df <- do.call(rbind, stats_list)
      
      datatable(detailed_df, 
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel')
                ),
                filter = 'top',
                rownames = FALSE) %>%
        formatStyle('TimeStep',
                   backgroundColor = styleEqual(c('Time 1', 'Time 2'), 
                                              c('#ffe6e6', '#e6e6ff')))
    } else {
      datatable(data.frame(Message = "No root data available"))
    }
  })
  
  # Download handlers
  output$download_transform <- downloadHandler(
    filename = function() {
      paste0("transform_params_", Sys.Date(), ".txt")
    },
    content = function(file) {
      lines <- c(
        "Root System Alignment - Transformation Parameters",
        "================================================",
        paste("Date:", Sys.Date()),
        "",
        "Basic Parameters:",
        paste("  X Translation:", input$translation_x, "pixels"),
        paste("  Y Translation:", input$translation_y, "pixels"),
        paste("  Rotation:", input$rotation, "degrees"),
        paste("  Uniform Scale Factor:", input$scale)
      )
      
      # Add advanced parameters if they are not at defaults
      advanced_used <- FALSE
      if (input$stretch_x != 1.0 || input$stretch_y != 1.0) {
        if (!advanced_used) { lines <- c(lines, "", "Advanced Parameters:"); advanced_used <- TRUE }
        lines <- c(lines, paste("  X-Axis Stretch:", input$stretch_x))
        lines <- c(lines, paste("  Y-Axis Stretch:", input$stretch_y))
      }
      if (input$keystone_h != 0 || input$keystone_v != 0) {
        if (!advanced_used) { lines <- c(lines, "", "Advanced Parameters:"); advanced_used <- TRUE }
        lines <- c(lines, paste("  Horizontal Keystone:", input$keystone_h))
        lines <- c(lines, paste("  Vertical Keystone:", input$keystone_v))
      }
      if (input$shear_x != 0 || input$shear_y != 0) {
        if (!advanced_used) { lines <- c(lines, "", "Advanced Parameters:"); advanced_used <- TRUE }
        lines <- c(lines, paste("  X-Shear:", input$shear_x))
        lines <- c(lines, paste("  Y-Shear:", input$shear_y))
      }
      
      if (!advanced_used) {
        lines <- c(lines, "", "Advanced Parameters: All at default values")
      }
      
      lines <- c(lines, "", 
        "Parameter Application Order:",
        "1. Center coordinates",
        "2. Apply X/Y stretching",
        "3. Apply shear transformation",
        "4. Apply keystone correction",
        "5. Apply rotation",
        "6. Apply uniform scaling",
        "7. Apply translation"
      )
      
      writeLines(lines, file)
    }
  )
  
  output$download_stats <- downloadHandler(
    filename = function() {
      paste0("root_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$rsml1_data, values$rsml2_data)
      
      length1 <- calculate_root_length(values$rsml1_data)
      length2 <- calculate_root_length(values$rsml2_data)
      
      stats <- data.frame(
        parameter = c("length_time1", "length_time2", "length_change", "growth_rate_percent"),
        value = c(length1, length2, length2 - length1, ((length2 - length1) / length1) * 100)
      )
      
      write.csv(stats, file, row.names = FALSE)
    }
  )
  
  # Download root structure plot
  output$download_root_plot <- downloadHandler(
    filename = function() {
      paste0("root_structure_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$rsml1_data, values$rsml2_data)
      
      # Extract points
      points1 <- extract_root_points(values$rsml1_data)
      points2 <- extract_root_points(values$rsml2_data)
      
      # Create combined plot
      points1$timepoint <- "Time 1"
      points2$timepoint <- "Time 2"
      combined_points <- rbind(points1, points2)
      
      p <- ggplot(combined_points, aes(x = x, y = -y, color = timepoint)) +
        geom_point(alpha = 0.7, size = 1.5) +
        scale_color_manual(values = c("Time 1" = "red", "Time 2" = "blue")) +
        labs(title = "Root System Structure Comparison",
             x = "X Position (pixels)",
             y = "Y Position (pixels)",
             color = "Time Point") +
        theme_minimal() +
        theme(aspect.ratio = 1)
      
      ggsave(file, p, width = 10, height = 8, dpi = 300)
    }
  )
  
  # Download alignment plot
  output$download_alignment_plot <- downloadHandler(
    filename = function() {
      paste0("alignment_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(values$rsml1_data, values$rsml2_data)
      
      # Extract and transform points
      points1 <- extract_root_points(values$rsml1_data)
      points2 <- extract_root_points(values$rsml2_data)
      
      points2_transformed <- transform_points(
        points2,
        input$translation_x,
        input$translation_y,
        input$rotation,
        input$scale,
        input$stretch_x,
        input$stretch_y,
        input$keystone_h,
        input$keystone_v,
        input$shear_x,
        input$shear_y
      )
      
      # Create alignment visualization
      points1$category <- "Time 1"
      points2_transformed$category <- "Time 2 (Aligned)"
      combined_points <- rbind(points1, points2_transformed)
      
      # Create subtitle with basic parameters
      subtitle_text <- sprintf("Translation: (%.1f, %.1f), Rotation: %.1f°, Scale: %.2fx",
                               input$translation_x, input$translation_y, input$rotation, input$scale)
      
      # Add advanced parameters to subtitle if they are being used
      advanced_params <- c()
      if (input$stretch_x != 1.0 || input$stretch_y != 1.0) {
        advanced_params <- c(advanced_params, sprintf("Stretch: (%.2f, %.2f)", input$stretch_x, input$stretch_y))
      }
      if (input$keystone_h != 0 || input$keystone_v != 0) {
        advanced_params <- c(advanced_params, sprintf("Keystone: (%.3f, %.3f)", input$keystone_h, input$keystone_v))
      }
      if (input$shear_x != 0 || input$shear_y != 0) {
        advanced_params <- c(advanced_params, sprintf("Shear: (%.3f, %.3f)", input$shear_x, input$shear_y))
      }
      
      if (length(advanced_params) > 0) {
        subtitle_text <- paste(subtitle_text, paste(advanced_params, collapse = ", "), sep = "\n")
      }
      
      p <- ggplot(combined_points, aes(x = x, y = -y, color = category)) +
        geom_point(alpha = 0.6, size = 1.2) +
        scale_color_manual(values = c("Time 1" = "red", "Time 2 (Aligned)" = "blue")) +
        labs(title = "Root System Alignment Results",
             subtitle = subtitle_text,
             x = "X Position (pixels)",
             y = "Y Position (pixels)",
             color = "Dataset") +
        theme_minimal() +
        theme(aspect.ratio = 1)
      
      ggsave(file, p, width = 12, height = 10, dpi = 300)
    }
  )
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("root_analysis_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(values$rsml1_data, values$rsml2_data)
      
      # Create temporary R Markdown file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Generate report data
      transform_params <- list(
        translation_x = input$translation_x,
        translation_y = input$translation_y,
        rotation = input$rotation,
        scale = input$scale
      )
      
      report_data <- generate_report_data(values$rsml1_data, values$rsml2_data, transform_params)
      
      # Write R Markdown content
      rmd_content <- paste0(
        "---\n",
        "title: 'Root System Analysis Report'\n",
        "date: '", format(Sys.Date(), "%B %d, %Y"), "'\n",
        "output: pdf_document\n",
        "---\n\n",
        "```{r setup, include=FALSE}\n",
        "knitr::opts_chunk$set(echo = FALSE)\n",
        "```\n\n",
        "## Summary\n\n",
        "This report presents the analysis of root system changes between two time steps.\n\n",
        "## Growth Metrics\n\n",
        "- **Total Root Length (Time 1):** ", sprintf("%.2f pixels", report_data$time_step1$total_length), "\n",
        "- **Total Root Length (Time 2):** ", sprintf("%.2f pixels", report_data$time_step2$total_length), "\n",
        "- **Root Length Change:** ", sprintf("%.2f pixels", report_data$growth$length_change), "\n",
        "- **Growth Rate:** ", sprintf("%.1f%%", report_data$growth$growth_rate), "\n",
        "- **Root Count Change:** ", report_data$growth$root_count_change, "\n\n",
        "## Alignment Parameters\n\n",
        "- **X Translation:** ", report_data$alignment$translation_x, " pixels\n",
        "- **Y Translation:** ", report_data$alignment$translation_y, " pixels\n",
        "- **Rotation:** ", sprintf("%.1f", report_data$alignment$rotation), " degrees\n",
        "- **Scale Factor:** ", sprintf("%.2f", report_data$alignment$scale), "\n",
        "- **Overlay Percentage:** ", sprintf("%.1f%%", report_data$alignment$overlap_percentage), "\n\n",
        "## Methodology\n\n",
        "Root systems were digitized from microscopy images using SmartRoot/ImageJ and exported as RSML files. ",
        "The RSML files contain all digitized root polyline coordinates and properties. ",
        "Alignment was performed using feature-based matching of root structures, ",
        "followed by geometric transformation optimization to align the root systems between time points.\n\n",
        "## Data Quality\n\n",
        "- **Time Step 1 Roots:** ", report_data$time_step1$root_count, "\n",
        "- **Time Step 2 Roots:** ", report_data$time_step2$root_count, "\n"
      )
      
      writeLines(rmd_content, temp_rmd)
      
      # Render to PDF
      rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)
      
      # Clean up
      unlink(temp_rmd)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)