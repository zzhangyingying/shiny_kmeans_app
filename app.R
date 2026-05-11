
##############################################################################
## KMeans Cluster Prediction Shiny App
## Auto-generated: 2026-05-04 15:36:37
## Usage: 
##   1. Place kmeans_model.rds and this app.R in the same folder
##   2. Open app.R in RStudio and click "Run App"
##   3. Or run: shiny::runApp("path/to/folder")
##############################################################################

library(shiny)
library(ggplot2)
library(factoextra)
library(ggsci)

# ---- Load model ----
library(tidyr)
model <- readRDS("kmeans_model.rds")
km_res       <- model$km_res
var.list     <- model$var.list
k.num        <- model$k.num
scale.trans  <- model$scale.trans
scale_center <- model$scale_center
scale_scale  <- model$scale_scale
var_ranges   <- model$var_ranges
scaled_data  <- model$scaled_data
plot_params  <- model$plot_params

# ---- Prediction function: assign new data point to nearest cluster center ----
predict_kmeans <- function(new_data, km_model, do_scale, s_center, s_scale) {
  new_mat <- as.matrix(new_data)
  if (do_scale == "T" && !is.null(s_center) && !is.null(s_scale)) {
    new_mat <- scale(new_mat, center = s_center, scale = s_scale)
  }
  centers <- km_model$centers
  dists <- apply(centers, 1, function(ctr) {
    sqrt(sum((new_mat - ctr)^2))
  })
  cluster_id <- which.min(dists)
  list(cluster = cluster_id, distances = dists)
}

# ========== UI ==========
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f7f9fc; font-family: Segoe UI, Arial, sans-serif; }
      .main-title { text-align: center; color: #2c3e50; margin-top: 20px; margin-bottom: 5px; }
      .sub-title  { text-align: center; color: #7f8c8d; margin-bottom: 25px; font-size: 14px; }
      .sidebar-panel { background: white; border-radius: 10px; padding: 20px; 
                       box-shadow: 0 2px 10px rgba(0,0,0,0.08); }
      .result-box { background: white; border-radius: 10px; padding: 25px; 
                    box-shadow: 0 2px 10px rgba(0,0,0,0.08); margin-bottom: 15px; }
      .cluster-badge { font-size: 36px; font-weight: bold; text-align: center; 
                       margin: 10px 0; padding: 15px; border-radius: 15px; }
      .distance-bar-container { margin: 5px 0; }
      .distance-bar { height: 22px; border-radius: 4px; text-align: right; 
                      padding-right: 8px; color: white; font-size: 12px; line-height: 22px; }
    "))
  ),
  
  h2(class = "main-title", 
     sprintf("KMeans Cluster Prediction (K=%d)", k.num)),
  p(class = "sub-title", 
    sprintf("Variables: %s  |  Created: %s", 
            paste(var.list, collapse = ", "), 
            format(model$created_time, "%Y-%m-%d %H:%M"))),
  
  fluidRow(
    column(4,
      div(class = "sidebar-panel",
          h4("Input New Sample Features"),
          hr(),
              sliderInput("ALT", label = "ALT",
                          min = 1, max = 674,
                          value = 20.06, step = 6.73),
              sliderInput("GGT", label = "GGT",
                          min = 4, max = 652,
                          value = 30.1272, step = 6.48),
              sliderInput("non_HDL_C_mmol", label = "non_HDL_C_mmol",
                          min = 0.63, max = 15.47,
                          value = 3.1611, step = 0.1484),
              sliderInput("TyG", label = "TyG",
                          min = 6.902, max = 11.6895,
                          value = 8.5096, step = 0.0479),
              sliderInput("bmi", label = "bmi",
                          min = 14.533, max = 24.9998,
                          value = 21.6128, step = 0.1047),
          hr(),
          actionButton("predict_btn", "Predict Cluster", 
                       class = "btn btn-primary btn-lg btn-block",
                       style = "width:100%; margin-top:10px;"),
          br(), br(),
          actionButton("reset_btn", "Reset to Mean", 
                       class = "btn btn-default btn-block",
                       style = "width:100%;")
      )
    ),
    
    column(8,
      div(class = "result-box",
          h4("Prediction Result"),
          uiOutput("result_display")
      ),
      div(class = "result-box",
          h4("Distance to Each Cluster Center"),
          uiOutput("distance_display")
      ),
      div(class = "result-box",
          h4("Cluster Centers Comparison"),
          tableOutput("centers_table")
      ),
      fluidRow(
        column(6,
          div(class = "result-box",
              h4("PCA Cluster Plot (Convex)"),
              plotOutput("pca_plot", height = "420px")
          )
        ),
        column(6,
          div(class = "result-box",
              h4("Cluster Centers Radar Chart"),
              plotOutput("radar_plot", height = "420px")
          )
        )
      )
    )
  )
)

# ========== Server ==========
server <- function(input, output, session) {
  
  # Reset button
  observeEvent(input$reset_btn, {
    for (v in var.list) {
      updateSliderInput(session, v, value = var_ranges[[v]]$mean)
    }
  })
  
  # Prediction logic
  pred_result <- eventReactive(input$predict_btn, {
    new_vals <- sapply(var.list, function(v) input[[v]])
    new_df <- as.data.frame(t(new_vals))
    colnames(new_df) <- var.list
    predict_kmeans(new_df, km_res, scale.trans, scale_center, scale_scale)
  })
  
  # Cluster colors
  cluster_colors <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", 
                      "#F39B7F", "#8491B4", "#91D1C2", "#DC0000",
                      "#7E6148", "#B09C85")
  
  # Result display
  output$result_display <- renderUI({
    res <- pred_result()
    clr <- cluster_colors[res$cluster]
    
    tagList(
      div(class = "cluster-badge",
          style = sprintf("background: %s22; color: %s; border: 3px solid %s;", 
                          clr, clr, clr),
          sprintf("Cluster %d", res$cluster)),
      
      div(style = "text-align:center; color:#7f8c8d; margin-top:10px;",
          sprintf("This sample is assigned to Cluster %d of %d. Euclidean distance to center: %.4f",
                  res$cluster, k.num, res$distances[res$cluster]))
    )
  })
  
  # Distance bars
  output$distance_display <- renderUI({
    res <- pred_result()
    max_dist <- max(res$distances)
    
    bars <- lapply(seq_along(res$distances), function(i) {
      pct <- (1 - res$distances[i] / (max_dist * 1.1)) * 100
      pct <- max(pct, 8)
      clr <- if (i == res$cluster) cluster_colors[i] else "#bdc3c7"
      lbl <- sprintf("Cluster %d: %.3f", i, res$distances[i])
      
      div(class = "distance-bar-container",
          div(class = "distance-bar",
              style = sprintf("width: %s%%; background: %s;", round(pct), clr),
              lbl))
    })
    
    tagList(bars)
  })
  
  # Cluster centers table
  output$centers_table <- renderTable({
    centers_df <- as.data.frame(km_res$centers)
    centers_df <- cbind("Cluster" = paste0("Cluster ", 1:nrow(centers_df)), 
                        round(centers_df, 4))
    
    # Append current input row
    res <- tryCatch(pred_result(), error = function(e) NULL)
    if (!is.null(res)) {
      new_vals <- sapply(var.list, function(v) input[[v]])
      new_row <- as.data.frame(t(new_vals))
      colnames(new_row) <- var.list
      if (scale.trans == "T" && !is.null(scale_center)) {
        new_scaled <- scale(new_row, center = scale_center, scale = scale_scale)
        new_row_show <- as.data.frame(new_scaled)
      } else {
        new_row_show <- new_row
      }
      new_row_show <- cbind("Cluster" = sprintf(">> New Sample (-> Cluster %d)", res$cluster),
                            round(new_row_show, 4))
      centers_df <- rbind(centers_df, new_row_show)
    }
    
    centers_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # PCA cluster plot (convex)
  output$pca_plot <- renderPlot({
    fviz_cluster(km_res, data = scaled_data,
                 geom = plot_params$geom,
                 ellipse.type = "convex",
                 star.plot = as.logical(plot_params$star.plot),
                 palette = plot_params$palette,
                 repel = as.logical(plot_params$repel),
                 show.clust.cent = as.logical(plot_params$show.clust.cent),
                 ellipse = as.logical(plot_params$ellipse),
                 ggtheme = theme_bw()) +
      ggtitle(sprintf("KMeans Clusters (K=%d) - PCA", k.num))
  })
  
  # Radar chart (polar bar chart)
  output$radar_plot <- renderPlot({
    centers_df <- as.data.frame(km_res$centers)
    centers_df$Cluster <- paste0("Cluster ", 1:nrow(centers_df))
    
    long_df <- tidyr::pivot_longer(centers_df, cols = -Cluster, 
                                    names_to = "Variable", values_to = "Value")
    
    ggplot(long_df, aes(x = Variable, y = Value, fill = Cluster)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
      coord_polar() +
      theme_minimal(base_size = 13) +
      scale_fill_manual(values = cluster_colors[1:k.num]) +
      labs(x = NULL, y = NULL, fill = "Cluster") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(size = 11, face = "bold"))
  })
}

shinyApp(ui = ui, server = server)
