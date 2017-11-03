

################## ggplot2 ###############

library(ggplot2)

Freshmen = read.csv("E:/UConn/RA/New Notes/Data visualization data/Freshmen.csv")
regional_crimedata = read.csv("E:/UConn/RA/New Notes/Data visualization data/Regional Crime Data.csv")
cities = read.csv("E:/UConn/RA/New Notes/Data visualization data/Cities.csv")
Carvana = read.csv("E:/UConn/RA/New Notes/Data visualization data/Carvana.csv")
credit_scoring  = read.csv("E:/UConn/RA/New Notes/Data visualization data/Credit Scoring.csv")

attach(Freshmen)


######## Basic scatter plot

ggplot(Freshmen,aes(x = GPA,y = Miles.from.Home)) +
  geom_point(col = "red") +
    ggtitle("Scatter plot of GPA vs High School GPA")


######## grammar of ggplot2

ggplot(Freshmen, aes(x = GPA,y = High.School.GPA)) +
  geom_point(col = "blue",pch = 4,cex = 4) +
    ggtitle("Scatter plot of GPA vs High School GPA") 

# adding facet as per college

ggplot(Freshmen, aes(x = GPA,y = High.School.GPA)) +
  geom_point(col = "blue") +
  facet_grid(.~College) + 
  ggtitle("Scatter plot of GPA vs High School GPA") 

# 
View(Freshmen)

ggplot(Freshmen, aes(x = GPA,y = High.School.GPA)) +
  geom_point(col = "blue") +
  facet_grid(.~Accommodations) + 
  ggtitle("Scatter plot of GPA vs High School GPA") 


# adding a linear model line

ggplot(Freshmen, aes(x = GPA,y = High.School.GPA)) +
  geom_point(col = "blue") +
  facet_grid(.~College) + 
  stat_smooth(method = "lm",se=F,col = "red") + 
  ggtitle("Scatter plot of GPA vs High School GPA") 


ggplot(Freshmen, aes(x = College,y = GPA)) +
  geom_point() +
  stat_summary(geom = "point", fun.y = "median", colour = "red", size = 4) + 
  ggtitle("Plot of GPA by College highlighting mean GPA") 


# adding coordinates

ggplot(Freshmen, aes(x =  Miles.from.Home,y = GPA)) +
  geom_point(col = "blue") +
  #facet_grid(.~College) + 
  #stat_smooth(method = "lm",se=F,col = "red") + 
  #scale_x_continuous("Miles from home",limits = c(10,500)) + 
  #scale_y_continuous("GPA",limits = c(2,4)) + 
  ggtitle("Scatter plot of GPA vs Miles from home") 

max(Miles.from.Home)

# Adding theme

ggplot(Freshmen, aes(x = GPA,y = High.School.GPA)) +
  geom_point(col = "blue") +
  facet_grid(.~College) + 
  stat_smooth(method = "lm",se=F,col = "red") + 
  scale_x_continuous("GPA",limits = c(1,4)) + 
  scale_y_continuous("High.School.GPA",limits = c(2,4)) + 
  theme_bw() + 
  ggtitle("Scatter plot of GPA vs High School GPA") 

####### Density plot 1 D

ggplot(data = Freshmen,aes(x = GPA)) +
  geom_density(col = "red",alpha = .3,fill = "blue") + 
    ggtitle("Density distribution of GPA")


######### Rug plot - 1 D

ggplot(data = Freshmen,aes(x = GPA)) +
  geom_rug(col = "red",alpha = .5,sides = "bi") + 
  ggtitle("Rug plot distribution of GPA")


######### histogram overlay density 1 D

ggplot(Freshmen, aes(x=GPA)) + 
  geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
     binwidth=.5,colour="black", fill="white") +
       geom_density(alpha=.2, fill="red") + # Overlay with transparent density plot
        ggtitle("Overlay with density plot")

######### bar graph density 1 D

ggplot(data = Freshmen,aes(x = Attends.Office.Hours)) +
  geom_bar(alpha = .3,col = "red",fill = "red",position = "dodge") + 
    geom_text(stat='Count',aes(label=..count..),vjust=-1) + 
      ggtitle("Distribution of Attend Office Hours")

######### dot plot 1 D

ggplot(Freshmen,aes(x = GPA)) + 
  geom_dotplot(col = "blue") + 
    ggtitle("Dotplot of GPA")

######### area plot

ggplot(data = Freshmen,aes(x = GPA)) + 
  geom_area(aes(y = ..density..),stat = "bin",fill = "maroon") + 
    ggtitle("Area plot of GPA")

######### box plot 2 D

ggplot(data = Freshmen,aes(x = Attends.Office.Hours,y = GPA),fill = Attends.Office.Hours) +
  geom_boxplot(alpha = 0.3,fill = "blue") +
    ggtitle("Boxplot of GPA vs Attend office hours")

######### scatter plot 2 D

ggplot(data = Freshmen,aes(x = GPA,y = High.School.GPA)) +
  geom_point(alpha = 0.5,color = "red",stat = "identity") + 
    ggtitle("Scatter plot between GPA and High School GPA")

######### rug plot 2 D rug

ggplot(data =Freshmen, aes(x = GPA,y = High.School.GPA)) +
  geom_rug(col = "red",alpha = .5,sides = "bl") + 
  ggtitle("Rug plot of GPA vs High School GPA")

# Heatmap or a smooth scatter

######### violin plot 2 D

ggplot(Freshmen,aes(x = College,y = Miles.from.Home)) + 
  geom_violin(col = "blue") + 
    ggtitle("Violin plot of College vs Miles from Home")

######### overlay plot 1X and multiple y around same axis

ggplot(Freshmen, aes(Miles.from.Home)) +    # basic graphical object
  geom_line(aes(y=GPA), colour="red") +         # first layer
    geom_line(aes(y=High.School.GPA), colour="green") + # second layer
      ggtitle("Overlay plot of GPA and High School GPA vs Miles from Home")

######### contour plot

ggplot(data=Freshmen,aes(GPA,High.School.GPA,Miles.from.Home)) + 
  geom_density2d(aes(colour=..level..)) + 
    scale_colour_gradient(low="green",high="red") + 
      geom_point() + 
        ggtitle("Contour plot for GPA, High School GPA vs Miles from home")

######### bubble plot

ggplot(data = Freshmen,aes(x = GPA,y = High.School.GPA)) +
  geom_point(alpha = 0.5,color = "blue",stat = "identity",size = Years.Off) + 
    ggtitle("Bubble plot between GPA and High School GPA")


attach(regional_crimedata)

######### bubble plot 

ggplot(data = regional_crimedata,aes(x = Violent.Rate,y = Property.Rate)) +
  geom_point(stat = "identity",alpha = 0.5,aes(color = Region,size = Population)) + 
  ggtitle("Bubble plot between Property Rate by Violent Rate")


cities = read.csv("E:/UConn/RA/OPIM 5604/Class Notes/Session 2/Data/Cities.csv")

str(cities)

library(maps)
n = data.frame(table(Carvana$VNST))

View(n)

states = data.frame(n$Var1)

ggplot(Carvana,aes(map_id=Carvana$VNST)) +
  geom_map(map=Carvana$VNST, col="white", fill="grey")

ggplot() + geom_map(data=cities,map = cities, aes(x=cities$X,y = cities$Y,map_id = cities$city))

whaworld = map_data("world")

View(world)

ggplot() + geom_map(data=world, map=world, aes(map_id=region))

ggplot() + geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region))

View(Carvana)
carvanam <- reshape2::melt(carvana, id = 1)

mv = c("Size","VNST","VehBCost")
cvn11 = Carvana[mv]

cvn11

cvn12 = aggregate.data.frame(cvn11,list(cvn11$VNST,cvn11$Size),FUN = mean)
cvn12


########
#####################3 gain inferences from

ggplot(Carvana,aes(x = Carvana$Nationality)) + 
  geom_bar(fill = "navy blue") + 
  ggtitle("Bar plot of Nationality")

ggplot(Carvana,aes(x = Carvana$VehBCost, y = Carvana$VehYear))
geom_point()  
library(ggplot2)

ggplot(Freshmen, aes(x = GPA,y = High.School.GPA)) +
  geom_point(col = "blue") +
  facet_grid(.~College) + 
  stat_smooth(method = "lm",se=F,col = "red") + 
  scale_x_continuous("GPA",limits = c(1,4)) + 
  scale_y_continuous("High.School.GPA",limits = c(2,4)) + 
  theme_bw() + 
  ggtitle("Scatter plot of GPA vs High School GPA") 


View(airquality)

ggplot(airquality,aes(x = airquality$Wind,y = airquality$Temp)) + 
  geom_line()

ggplot(airquality,aes(x = airquality$Month,y = airquality$Temp)) + 
  geom_jitter()


######### maps

View(USArrests)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)

if (require(maps)) {
  states_map <- map_data("state")
  ggplot(crimes, aes(map_id = state)) +
    geom_map(aes(fill = Murder), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat)
  
  last_plot() + coord_map()
  ggplot(crimesm, aes(map_id = state)) +
    geom_map(aes(fill = value), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    facet_wrap( ~ variable)
}


######### 
###################################################################

library(shiny)
#runExample("01_hello")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Freshmen with requested number of bins
  
  # This expression that generates a histogram is wrapped in a call to renderPlot to indicate that:

  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- Freshmen$Miles.from.Home
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Distance students travel from home (in miles)",
         main = "Histogram of miles from home")
    
  })
  
}


shinyApp(ui, server)


###############################


# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Shiny Text"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("rock", "pressure", "cars")),
    
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    verbatimTextOutput("summary"),
    
    tableOutput("view")
  )
))


server.R
library(shiny)
library(datasets)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})


datasetInput <- reactive({
  switch(input$dataset,
         "rock" = rock,
         "pressure" = pressure,
         "cars" = cars)
})


output$view <- renderTable({
  head(datasetInput(), n = input$obs)
})

#######################################


View(cities)

ids = seq(1,52)

values = data.frame(
  id = ids,
  value = cities$city
)

position = data.frame(
  id = ids,
  x = cities$X,
  y = cities$Y
)


ggplot(values) + geom_map(aes(map_id = id,x = df2$x,y = df2$y),map = df2)
install.packages("ggmap")
library(ggmap)
library(dplyr)
library("maps")
library("ggplot2")
View(USArrests)

usa = map_data("cities")

df.merge = left_join(cities,usa)


ggplot(data=cities) + geom_map(aes(fill=cities$POP, map_id=city),map=position) 


ggplot(cities,aes(x = cities$X,y = cities$Y,group = cities$State)) + geom_polygon(aes(fill = cities$POP))

###############################################

UIT = shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Shiny Text"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("Freshmen", "Carvana", "cars")),
    
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    verbatimTextOutput("summary"),
    
    tableOutput("view")
  )
))


# Define server logic required to summarize and view the selected dataset
serverT = shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "Freshmen" = Freshmen,
           "Carvana" = Carvana,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})

shinyApp(UIT, serverT)

######## checking reactivity 

datasetInput <- reactive({
  switch(input$dataset,
         "Freshmen" = Freshmen,
         "Carvana" = Carvana,
         "cars" = cars)
})


#  output$view <- renderTable({
#    head(datasetInput(), n = input$obs)
#  })

#Now that we've taken a deeper look at some of the core concepts, let's revisit the source code and try to understand what's going on in more depth. The user interface definition has been updated to include a text-input field that defines a caption. Other than that it's very similar to the previous example:
  
  # Define UI for dataset viewer application
UIR = shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Reactivity"),
  
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    textInput("caption", "Caption:", "Data Summary"),
    
    selectInput("dataset", "Choose a dataset:", 
                choices = c("Freshmen", "Carvana", "cars")),
    
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("caption")), 
    
    verbatimTextOutput("summary"), 
    
    tableOutput("view")
  )
))




# Define server logic required to summarize and view the selected dataset
serverR = shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers (it 
  #     only executes a single time)
  #
  datasetInput <- reactive({
    switch(input$dataset,
           "Freshmen" = Freshmen,
           "Carvana" = Carvana,
           "cars" = cars)
  })
  
  # The output$caption is computed based on a reactive expression that
  # returns input$caption. When the user changes the "caption" field:
  #
  #  1) This expression is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  # Note that because the data-oriented reactive expressions below don't 
  # depend on input$caption, those expressions are NOT called when 
  # input$caption changes.
  output$caption <- renderText({
    input$caption
  })
  
  # The output$summary depends on the datasetInput reactive expression, 
  # so will be re-executed whenever datasetInput is invalidated
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # The output$view depends on both the databaseInput reactive expression
  # and input$obs, so will be re-executed whenever input$dataset or 
  # input$obs is changed. 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})


shinyApp(UIR,serverR)
  