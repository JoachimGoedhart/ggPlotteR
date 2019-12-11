# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ggPlotteR - coding a plot line by line: Shiny app for teaching/demonstrating ggplot2
# Created by Joachim Goedhart (@joachimgoedhart), first version 2019
# Takes tidy data as input
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Joachim Goedhart (C) 2019
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# To implement:
# Facetting
# Stats (boxplot, mean, median, violinplot, raincloudplot)
# User-defined code

library(shiny)
library(ggplot2)

# data from: https://github.com/resbaz/r-novice-gapminder-files
df_gapminder <- read.csv("gapminder-FiveYearData.csv", na.strings = "")

# Create a reactive object here that we can share between all the sessions.
vals <- reactiveValues(count=0)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("ggPlotteR - coding a plot line by line"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width=3,
          conditionalPanel(
            condition = "input.tabs=='Plot'",
              checkboxInput(inputId = "start", label = "Initiate ggplot", value = TRUE),
            h3('Map data to coordinates'),

              selectInput("x_var", label = "Select variable for x-axis", choices = ""),
              selectInput("y_var", label = "Select variable for y-axis", choices = ""),
            
            h3("Select 'Geom'"),
              selectInput("geom", label = NA, choices = list("-"="-", "geom_point"="geom_point", "geom_jitter"="geom_jitter", "geom_dotplot"="geom_dotplot")),
            conditionalPanel(condition = "input.geom=='geom_line'",  
            selectInput('grouping', label="Group", choices = list("-"="-"), selected = "-")
            ),
            h3("Modify the appearance"),
             #size (only when y-variable is specified)
              conditionalPanel(condition = "input.y_var!='-'",
                 selectInput("map_size", label = "Select a variable that reflects size", choices = list("No"="No"), selected = "No"),
                 conditionalPanel(condition = "input.map_size=='No'",
                       sliderInput("size", "Set a general size:", min = 0, max = 10, value = 1)
                       ),hr()),
              
            #color or fill
              selectInput("map_color", label = "Select a variable that reflects color", choices = list("No"="No"), selected = "No"),
            conditionalPanel(condition = "input.map_color=='No'",
                             textInput("color", "Set a general color:", value='black')
            ),
            hr(),
            #alpha
            selectInput("map_alpha", label = "Select a variable that reflects transparancy", choices = list("No"="No"), selected = "No"),
            conditionalPanel(condition = "input.map_alpha=='No'",
                             sliderInput("alpha", "Set a general transparancy (alpha):", min = 0, max = 1, value = 1)
            ),
            
            hr(),

            
           
            ######### Change Scales ###########
            
            actionLink("toggle_scales", h4("⇩ Change Scales")),
            conditionalPanel(
              condition = "input.toggle_scales % 2 == 1",

                             
                             #range_x
                             textInput("range_x", "Range x-axis (min,max)", value = ""),
                             #range_y
                             checkboxInput(inputId = "log_x", label = "Use Log10 scale for x-axis", value = FALSE),
                             
                             #range_y
                             textInput("range_y", "Range y-axis (min,max)", value = ""),
                             #log_y
                             checkboxInput(inputId = "log_y", label = "Use Log10 scale for y-axis", value = FALSE)
                             

                             #Close Scaling-section
            ),
            #aspect ratio
            

            hr(),
            
            ######### Change Labels ###########
            
            actionLink("toggle_labels", h4("⇩ Change Labels")),
            conditionalPanel(
              condition = "input.toggle_labels % 2 == 1",
              
                            textInput("lab_x", "X-axis:", value = ""),
                            textInput("lab_y", "Y-axis:", value = "")
                            #title
                            #size_label

            ),
            
            hr(),
            
            ######### Change Theme ###########
            
            actionLink("toggle_themes", h4("⇩ Change Theme")),
            conditionalPanel(
              condition = "input.toggle_themes % 2 == 1",
                selectInput("theme", label = NA, choices = list("-"="theme_grey", "theme_light"="theme_light", "theme_minimal"="theme_minimal", "theme_classic"="theme_classic", "theme_dark"="theme_dark"), selected ="-"),
                numericInput("theme_size", "Theme size:", value = "11"),
                checkboxInput(inputId = "no_grid", label = "Remove gridlines", value = FALSE),
              NULL
              
            ),
            hr(),
              
            ######### Custom Input ###########             
            
            actionLink("toggle_custom", h4("⇩ Custom code")),
            conditionalPanel(
              condition = "input.toggle_custom % 2 == 1",
              
              textInput("code", "Code", value = "")),

            
              NULL),
              conditionalPanel(
                  condition = "input.tabs=='Data'",
              h4("Data upload"),
              
              radioButtons(
                "data_input", "",
                choices = 
                  list("Example data (Gapminder)" = 1,
                       "Upload TXT or CSV file" = 3
                  )
                ,
                selected =  1),
              
              conditionalPanel(
                condition = "input.data_input=='3'",
        
                fileInput("upload", NULL, multiple = FALSE),

                  radioButtons(
                    "upload_delim", "Delimiter",
                    choices =
                      list("Comma" = ",",
                           "Tab" = "\t",
                           "Semicolon" = ";",
                           "Space" = " "),
                    selected = ",")

                ),

              NULL
              ),
          conditionalPanel(
            condition = "input.tabs=='About'",
            
            #Session counter: https://gist.github.com/trestletech/9926129
            h4("About"),  "There are currently", 
            verbatimTextOutput("count"),
            "session(s) connected to this app."  
          )
                   
                   
      ),   #Close sidebarPanel

      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(id="tabs",
                    tabPanel("Plot",h3("R-code and Plot",br(),br(),
                                       actionButton("settings_copy", icon = icon("clone"),
                                                    label = "Copy R-code")
                                       
                                       ),
                             splitLayout(cellWidths = c("40%", "60%"),
                                        (verbatimTextOutput("cooltext")), plotOutput("coolplot"))
                              ),
                    tabPanel("Data", h4("Data as provided"),tableOutput("data_uploaded")),
                    tabPanel("About", includeHTML("about.html"))
                    )
        
      )   #Close mainPanel
      

   ) #Close sidebarLayout
) #Close fluidPage

server <- function(input, output, session) {

  isolate(vals$count <- vals$count + 1)
  ###### DATA INPUT ###################
  
df_upload <- reactive({
    
    if (input$data_input == 1) {
      data <- df_gapminder

    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else  {
        isolate({

            # data <- read_delim(file_in$datapath,
            #                    delim = input$upload_delim,
            #                    col_names = TRUE)
          
          data <- read.csv(file=file_in$datapath,
                             sep = input$upload_delim)
          # observe({print(input$upload$name)})
           
        })
      }
    }
    return(data)
  })
  
  
  #### DISPLAY UPLOADED DATA (as provided) ##################
  
output$data_uploaded <- renderTable(
    
    #    observe({ print(input$tidyInput) })
    df_upload(),
    rownames = FALSE,
    options = list(pageLength = 100, autoWidth = FALSE,
                   lengthMenu = c(10, 100, 1000, 10000)),
    editable = FALSE,selection = 'none'
  )
  
  
  
  
  
  
  geom.selected <- "-"
  #Retrieve the currently selected geom and use as default, even when y_var changes
  observe({
    geom.selected <<- input$geom
  })
  
  observeEvent(input$y_var, {
    if (input$y_var=="-")  {
      updateSelectInput(session, "geom", choices = list("-"="-", "geom_density"="geom_density", "geom_dotplot"="geom_dotplot", "geom_histogram" = "geom_histogram"))
    } 
    
    else if (input$y_var!="-") {
      updateSelectInput(session, "geom", choices = list("-"="-", "geom_point"="geom_point", "geom_jitter"="geom_jitter", "geom_line"="geom_line"), selected = geom.selected)

    }
  })

  ##### Get Variables from the input ##############
  
  observe({
    df <- df_upload()
    var_names  <- names(df)
    varx_list <- c("-", var_names)

    # Get the names of columns that are factors. These can be used for coloring the data with discrete colors
    nms_fact <- names(Filter(function(x) is.factor(x) || is.integer(x) ||
                               is.logical(x) ||
                               is.character(x),
                             df))
    nms_var <- names(Filter(function(x) is.integer(x) ||
                              is.numeric(x) ||
                              is.double(x),
                            df))

    vary_list <- c("-",nms_var)
    mapping_list_num <- c("No",nms_var)
    mapping_list_all <- c("No",var_names)
    facet_list <- c(".",nms_fact)
    
    updateSelectInput(session, "x_var", choices = varx_list)
    updateSelectInput(session, "y_var", choices = vary_list)
    updateSelectInput(session, "map_size", choices = mapping_list_num)
    updateSelectInput(session, "map_color", choices = mapping_list_all)
    updateSelectInput(session, "map_alpha", choices = mapping_list_all)
    updateSelectInput(session, "grouping", choices = varx_list)
  })
  

  ##### Render the plot ############
  
output$coolplot <- renderPlot({
    #Clean the canvas if ggplot is not initiated
    if(input$start != TRUE) {return(NULL)}
    df <- as.data.frame(df_upload())
    p <- eval(parse(text = r_code()))
    p
  })
  
output$cooltext <- renderText({

  c <- paste0("####### Coding a ggplot, line-by-line #######\n")
  c <- paste0(c,"#Load the ggplot2 package (once per session):\nlibrary(ggplot2)\n")
#  c <- paste0(c,"#To read data from a file use:\n")
  
  if (input$data_input == 3) {c <- paste0(c,"#To use data from the csv file type:\n df <- read.csv('",input$upload$name,"')\n\n")}
  
  c <- paste0(c,"##############################################\n\n")
  c <- paste0(c,"#This is the R-code to generate the plot:\n")
    c <- paste0(c,as.character(r_code()))
    }
    )


r_code <- renderText({
  c <- ""

  
  if(input$start == TRUE) {c <- paste(c,"ggplot(data = df) +\n")}
  if (input$x_var != "-") c <- paste0(c,"  aes(x=",input$x_var,") +\n")
  if (input$y_var != "-") c <- paste0(c,"  aes(y=",input$y_var,") +\n")
  if (input$geom != "-") {
        #define geom and open bracket
        c <- paste0(c, "  ",input$geom, "(") 

        if (input$map_size=="No" && input$size!="1") {c <- paste0(c, "size = ",input$size,", ")}
        if (input$map_color=="No" && input$color!="black") {c <- paste0(c, "color = '",input$color,"', ")}
        if (input$map_alpha=="No" && input$alpha!="1") {c <- paste0(c, "alpha = ",input$alpha,", ")}
        
        #close bracket
        c <- paste0(c,") +\n") 
  }
  
  if (input$grouping != "-")  c <-paste0(c,"  aes(group=",input$grouping, ") +\n")
  

  if(input$map_size!="No") c <-paste0(c,"  aes(size=",input$map_size, ") +\n")
  
  #Add color to point/jitter
  if(input$map_color!="No" && input$y_var!="-") c <-paste0(c,"  aes(color=",input$map_color, ") +\n")
  #Add fill to his/density/dotplot
  if(input$map_color!="No" && input$y_var=="-") c <-paste0(c,"  aes(fill=",input$map_color, ") +\n")

  if(input$map_alpha!="No") c <-paste0(c,"  aes(alpha=",input$map_alpha, ") +\n")  
  
  if(input$code!="") c <-paste0(c,"  ",input$code," +\n")  
    
  if (input$log_x) c <-paste0(c,"  scale_x_log10() +\n")
  if (input$log_y) c <-paste0(c,"  scale_y_log10() +\n")
  
  if (input$range_x !="") c <-paste0(c,"  coord_cartesian(xlim=c(",input$range_x,")) +\n")
  if (input$range_y !="") c <-paste0(c,"  coord_cartesian(ylim=c(",input$range_y,")) +\n")
  
  if(input$lab_x!="") c <- paste0(c, "  labs(x = '",input$lab_x,"')+\n")
  if(input$lab_y!="") c <- paste0(c, "  labs(y = '",input$lab_y,"')+\n")          

#  if (input$theme!="-") c <- paste0(c,"  ",input$theme,"() +\n")  
  
  if (input$theme != "theme_grey" || input$theme_size!="11" ) {
    #define theme and open bracket
    c <- paste0(c, "  ",input$theme, "(") 
      if (input$theme_size!="11") c <- paste0(c,"base_size=", input$theme_size,"")
    #close bracket
    c <- paste0(c,") +\n")
  }
  
  #Remove Grid
  if (input$no_grid) c <- paste0(c,"  theme(panel.grid = element_blank()) +\n")

  c <- paste0(c,"  NULL")
  
  })

observeEvent(input$settings_copy , {
  showModal(urlModal(url=r_code(), title = "R-code to create the plot"))
})


########### Update count #########
# Reactively update the client.
output$count <- renderText({
  vals$count
})

# When a session ends, decrement the counter.
session$onSessionEnded(function(){
  isolate(vals$count <- vals$count - 1)
})

######## The End; close server ########################
} #Close server


# Run the application 
shinyApp(ui = ui, server = server)

