# Check if the necessary packages are installed
pkgInstalled <- function(pkgName) pkgName %in% rownames(installed.packages())

# If the package is not installed, install it
if (!pkgInstalled("shiny")) install.packages("shiny")
if (!pkgInstalled("ggplot2")) install.packages("ggplot2")
if (!pkgInstalled("dplyr")) install.packages("dplyr")
if (!pkgInstalled("GGally")) install.packages("GGally")
if (!pkgInstalled("RColorBrewer")) install.packages("RColorBrewer")
if (!pkgInstalled("scales")) install.packages("scales")

# Load necessary packages
library("shiny")
library("ggplot2")
library("dplyr")
library("GGally")
library("RColorBrewer")
library("scales")

fb <- read.csv("dataset_Facebook.csv", sep = ";", header = TRUE)
fb <- fb[complete.cases(fb), ]

# Function used to scale values between 0 and 1
my_scale <- function(x){
  (x-min(x))/(max(x) - min(x))
}

day.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

scaled.df <- fb
scaled.df$Category <- my_scale(fb$Category)
scaled.df$Post.Month <- my_scale(fb$Post.Month)
scaled.df$Post.Weekday <- my_scale(fb$Post.Weekday)
scaled.df$Post.Hour <- my_scale(fb$Post.Hour)

scaled.hours.df <- data.frame("Label" = seq(1,23,2), "Scaled" = my_scale(seq(1,23,2)))

ui <- fluidPage(
  mainPanel(h1("Facebook Metrics Investigation"),
    tabsetPanel(
      tabPanel(title = "Heatmap", 
               h2("Investigate the Heatmap"), 
               p("Discover how the frequencies of different metrics vary over different days of the week and months of the year. Furthermore, see these metrics and how many interactions there are with the different types of objects."),
               plotOutput("heatmap"),
               selectInput("metric", choices = c("Comments", "Likes", "Shares"), label = "Metric"),
               selectInput("hm.type", choices = c("Link", "Photo", "Status", "Video"), label = "Object Type")),
      tabPanel(title = "Small Multiples",
               h2("Investigate the Small Multiples Plot"),
               p("Learn how the different metrics vary over the course of a year based upon different types of aggregation. Whether you aggregate by the average, overall total, or even the number of instances in this dataset for each metric, you'll be able to track that feature for each type of object."),
               selectInput("sm.metric", choices = c("Comments", "Likes", "Shares"), label = "Metric"),
               selectInput("agg", choices = c("Average", "Total", "Count"), label = "Aggregate"),
               plotOutput("sm.mult")),
      tabPanel(title = "Parallel Coordinates",
               h2("Investigate the Parallel Coordinates Plot"),
               p("Track the relationships across different categories and different time measures (i.e month, day of the week, and hour in the day) for each type of object."),
               plotOutput("parallel"),
               selectInput("pc.type", choices = c("Link", "Photo", "Status", "Video"), label = "Object Type"))
    )
  )
)

server <- function(input, output) {
  #################
  #### HEATMAP ####
  #################
  counts <- reactive({
    filtered.df <- fb %>% filter(Type == input$hm.type)
    if (input$metric == "Comments"){
      heatmap.data <- aggregate(comment~Post.Month+Post.Weekday, filtered.df, sum)
      colnames(heatmap.data)[names(heatmap.data) == "comment"] <- "metric"
      heatmap.data
    } else if (input$metric == "Likes"){
      heatmap.data <- aggregate(like~Post.Month+Post.Weekday, filtered.df, sum)
      colnames(heatmap.data)[names(heatmap.data) == "like"] <- "metric"
      heatmap.data
    } else {
      heatmap.data <- aggregate(share~Post.Month+Post.Weekday, filtered.df, sum)
      colnames(heatmap.data)[names(heatmap.data) == "share"] <- "metric"
      heatmap.data
    }
  })
  output$heatmap <- renderPlot({
    ggplot(counts(), aes(x = Post.Month, y = Post.Weekday)) +
      geom_tile(aes(fill = metric)) + 
      theme(panel.background = element_blank(),
            axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            axis.text = element_text(size = 15), 
            legend.position = "bottom",
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 20)) +
      guides(fill = guide_colorbar(title.position = "bottom", barwidth = 30, title.hjust = 0.5)) +
      scale_fill_gradientn(name = paste("Number of ", input$metric, sep = ""), colours=brewer.pal(9,"YlGnBu"), labels = comma) +
      scale_y_continuous(breaks = 1:7,labels = c("Mon","Tue","Wed","Thu","Fri","Sat", "Sun")) +
      scale_x_continuous(breaks = 1:12, labels = month.abb[1:12])
  })
  ##############################
  #### PARALLEL COORDINATES ####
  ##############################
  parallels <- reactive({
    fb <- read.csv("dataset_Facebook.csv", sep = ";", header = TRUE)
    fb <- fb[complete.cases(fb), ]
    fb %>% filter(Type == input$pc.type)
  })
  output$parallel <- renderPlot({
    ggparcoord(parallels(), columns = c(3,4,5,6), scale = "uniminmax", mapping = aes(colour = "blue")) + # groupColumn = "Type"
      scale_color_manual(values = c("blue" = "blue")) +
      scale_x_discrete(labels = c("Category", "Month", "Weekday", "Hour"), position = "top") +
      scale_y_continuous(breaks = c(0, 0.5, 1), labels = seq(1,3,1)) +
      theme(axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 20, colour = "black", face = "bold"),
            panel.background = element_blank(),
            legend.position = "none") +
      geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), color = "black", size = 1) +
      geom_segment(aes(x = 2, xend = 2, y = 0, yend = 1), color = "black", size = 1) +
      geom_segment(aes(x = 3, xend = 3, y = 0, yend = 1), color = "black", size = 1) +
      geom_segment(aes(x = 4, xend = 4, y = 0, yend = 1), color = "black", size = 1) +
      geom_label(data = scaled.df, aes(x = 1, y = Category, label = fb$Category), inherit.aes = FALSE, size = 6, label.r=unit(0, "lines"), label.padding=unit(0.1, "lines"), color="white") + 
      annotate("text", x = rep(1,3), y = seq(0,1,1/2), label = c(1:3), size = 6) +
      geom_label(data = scaled.df, aes(x = 2, y = Post.Month, label = month.abb[fb$Post.Month]), inherit.aes = FALSE, size = 6.5, label.r=unit(0, "lines"), label.padding=unit(0.1, "lines"), color="white") + 
      annotate("text", x = rep(2,12), y = seq(0,1,1/11), label = month.abb[1:12], size = 6.5) +
      geom_label(data = scaled.df, aes(x = 3, y = Post.Weekday, label = day.abb[fb$Post.Weekday]), inherit.aes = FALSE, size = 6.5, label.r=unit(0, "lines"), label.padding=unit(0.1, "lines"), color="white") + 
      annotate("text", x = rep(3,7), y = seq(0,1,1/6), label = c("Mon","Tue","Wed","Thu","Fri","Sat", "Sun"), size = 6.5) +
      geom_label(data = scaled.hours.df, aes(x = 4, y = Scaled, label = Label), inherit.aes = FALSE, size = 6, label.r=unit(0, "lines"), label.padding=unit(0.1, "lines"), color="white") +
      annotate("text", x = rep(4,12), y = seq(0,1,1/11), label = seq(1,23,2), size = 6)
  }, height = "auto", width = 1000)
  #########################
  #### SMALL MULTIPLES ####
  #########################
  sm.agg <- reactive({
    if (input$sm.metric == "Comments"){
      if (input$agg == "Average"){
        sm.data <- aggregate(comment~Post.Month+Type, fb, mean)
        colnames(sm.data)[names(sm.data) == "comment"] <- "metric"
        sm.data
      } else if (input$agg == "Total") {
        sm.data <- aggregate(comment~Post.Month+Type, fb, sum)
        colnames(sm.data)[names(sm.data) == "comment"] <- "metric"
        sm.data
      } else {
        sm.data <- aggregate(comment~Post.Month+Type, fb, length)
        colnames(sm.data)[names(sm.data) == "comment"] <- "metric"
        sm.data
      }
    } else if (input$sm.metric == "Likes"){
      if (input$agg == "Average"){
        sm.data <- aggregate(like~Post.Month+Type, fb, mean)
        colnames(sm.data)[names(sm.data) == "like"] <- "metric"
        sm.data
      } else if (input$agg == "Total") {
        sm.data <- aggregate(like~Post.Month+Type, fb, sum)
        colnames(sm.data)[names(sm.data) == "like"] <- "metric"
        sm.data
      } else {
        sm.data <- aggregate(like~Post.Month+Type, fb, length)
        colnames(sm.data)[names(sm.data) == "like"] <- "metric"
        sm.data
      }
    } else {
      if (input$agg == "Average"){
        sm.data <- aggregate(share~Post.Month+Type, fb, mean)
        colnames(sm.data)[names(sm.data) == "share"] <- "metric"
        sm.data
      } else if (input$agg == "Total") {
        sm.data <- aggregate(share~Post.Month+Type, fb, sum)
        colnames(sm.data)[names(sm.data) == "share"] <- "metric"
        sm.data
      } else {
        sm.data <- aggregate(share~Post.Month+Type, fb, length)
        colnames(sm.data)[names(sm.data) == "share"] <- "metric"
        sm.data
      }
    }
  })
  output$sm.mult <- renderPlot({
    ggplot(sm.agg(), aes(x = Post.Month, y = metric)) +
      geom_line(size = 1.5, color = "blue") +
      scale_x_continuous(breaks = 1:12, labels = month.abb[1:12], name = "Month") +
      scale_y_continuous(labels = comma) +
      facet_grid(Type~.) +
      ggtitle(paste(input$sm.metric, " Aggregated by ", input$agg, sep = "")) +
      theme(axis.text = element_text(size = 15),
            axis.title.y = element_blank(),
            axis.ticks = element_line(colour = "grey90"),
            axis.title = element_text(face = "bold", size = 22),
            strip.text.y = element_text(size = 25),
            plot.title = element_text(face = "bold", size = 23, hjust = 0.5),
            panel.grid.major.y = element_line(colour = "grey90", size=0.5, linetype="dashed"),
            panel.grid.major.x = element_line(colour = "grey90", size=0.5, linetype="dashed"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color = "grey40", size=0.5))
  })
}

shinyApp(ui = ui, server = server)