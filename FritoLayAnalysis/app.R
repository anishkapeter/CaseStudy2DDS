#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(aws.s3)
library(dplyr)
library(GGally)



data = read.csv("https://raw.githubusercontent.com/anishkapeter/CaseStudy2DDS/main/data.csv")


header <- dashboardHeader(title = "Frito Lay Analysis")


sidebar <- dashboardSidebar(width = 250,
                            conditionalPanel(
                              condition = "input.tabselected == '1'",
                              selectInput('select1', 'Select a Graph',choices = c("Attrition Percentage by Over Time" = "APOT",
                                                                                 "Attrition by Monthly Income" = "AMI",
                                                                                 "Attrition by Age" = "AA"))
                              ),
                            conditionalPanel(
                              condition = "input.tabselected == '2'",
                              selectInput('select2', 'Select a Graph',choices = c("Job Role vs Job Satisfaction" = "JRJS",
                                                                                 "Job Role vs Work Life Balance" = "JRWLB",
                                                                                 "Job Role vs Years At Company" = "JRYAC",
                                                                                 "Job Role vs Attrition" = "JRA",
                                                                                 "Attrition vs Job Satisfaction" = "AJS")),
                                          
                            ))

# Body ----
body <- dashboardBody(
  mainPanel(
    tabsetPanel(
      id = "tabselected", # Add ID here
      tabPanel("Attrition Prediciton Analysis",value = "1",
               conditionalPanel(
                 condition = "input.select1 == 'APOT'",
                 h4("Attrition Percentage By Over Time"), 
                 plotOutput("APOTPlot")
               ),
               conditionalPanel(
                 condition = "input.select1 == 'AMI'",
                 h4("Attrition by Monthly Income"), 
                 plotOutput("AMIPlot")
               ),
               conditionalPanel(
                 condition = "input.select1 == 'AA'",
                 h4("Attrition by Age"), 
                 plotOutput("AAPlot")
               )
      ),
      tabPanel("Job Role Related Trends", value = "2",
               conditionalPanel(
                 condition = "input.select2 == 'JRJS'",
                 h4("Job Role vs Job Satisfaction"), 
                 radioButtons("satisfactlevel","Select an option",
                              choices = list("Low Satisfaction" = 1, "Medium Satisfaction" = 2,
                                             "High Satisfaction" = 3, "Very High Satisfaction" = 4), 
                              selected = 1),
                 plotOutput("JRJSPlot")
               ),
               conditionalPanel(
                 condition = "input.select2 == 'JRWLB'",
                 h4("Job Role vs Work Life Balance"), 
                 radioButtons("balancelevel","Select an option",
                              choices = list("Low Work Life Balance" = 1, "Medium Work Life Balance" = 2,
                                             "High Work Life Balance" = 3, "Very High Work Life Balance" = 4), 
                              selected = 1),
                 plotOutput("JRWLBPlot")
               ),
               conditionalPanel(
                 condition = "input.select2 == 'JRYAC'",
                 h4("Job Role vs Years At Company"),
                 plotOutput("JRYACPlot")
               ),
               conditionalPanel(
                 condition = "input.select2 == 'JRA'",
                 h4("Job Role vs Attrition"), 
                 checkboxGroupInput("Jobrole","Select an option",
                              choices = list("Sales Executive"="Sales Executive",
                                             "Research Director" = "Research Director",
                                             "Manufacturing Director" ="Manufacturing Director" ,  
                                             "Research Scientist"="Research Scientist",
                                             "Sales Representative"="Sales Representative",
                                             "Healthcare Representative"="Healthcare Representative",
                                             "Manager"="Manager",
                                             "Human Resources"="Human Resources",
                                             "Laboratory Technician"="Laboratory Technician"), 
                              selected = "Sales Executive"),
                 plotOutput("JRAPlot")
               ),
               conditionalPanel(
                 condition = "input.select2 == 'AJS'",
                 h4("Attrition vs Job Satisfaction"),
                 plotOutput("AJSPlot")
               ),
               
               
      )
    )
  )
)


# UI ----
ui <- dashboardPage(header, sidebar, body, skin = "black")

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$JRJSPlot <- renderPlot({
    ggplot(data, aes(x=JobRole, fill = JobSatisfaction)) + geom_bar(stat="count",position = "dodge")
    
    individualjobrole = data %>% 
      count(JobRole,JobSatisfaction) 
    
    totaljobrole = data %>% 
      group_by(JobRole) %>% 
      summarise(total = n())
    
    jobrolessatisfaction = merge(individualjobrole,totaljobrole, by="JobRole")
    
    percentjobrolesatisfaction = jobrolessatisfaction %>% 
      summarise(percent = n/total*100)
    
    percentagessatisfaction = data.frame(jobrolessatisfaction,percentjobrolesatisfaction)
    
    color = c("#FF6961","#FDFD96", "#A7C7E7","#C3B1E1")
    percentagessatisfaction %>% 
      filter(JobSatisfaction == input$satisfactlevel) %>% 
      ggplot(aes(x=JobRole, y = percent, fill = JobSatisfaction)) +
        geom_bar(stat = "identity" , position = "dodge",color = "black") + 
        ggtitle("Job Satisfaction for Each Role") +
        xlab("Job Role") + 
        ylab("Percentage") +
        coord_flip() +
        theme(plot.background = element_rect("#f5f3ed"), 
              legend.background = element_blank()) + 
        scale_fill_manual(values = color)
  })
  output$JRWLBPlot <- renderPlot({
    individualworklife = data %>% 
      count(JobRole,WorkLifeBalance) 
    
    totalworklife = data %>% 
      group_by(JobRole) %>% 
      summarise(total = n())
    
    jobrolesbalance = merge(individualworklife,totalworklife, by="JobRole")
    
    percentjobrolebalance = jobrolesbalance %>% 
      summarise(percent = n/total*100)
    
    percentagebalance = data.frame(jobrolesbalance,percentjobrolebalance)
    
    color = c("#FAA0A0", "#FDFD96","#C1E1C1", "#C3B1E1", "#F8C8DC")
    percentagebalance %>% 
      filter(WorkLifeBalance == input$balancelevel) %>%
      ggplot(aes(x=JobRole, y = percent, fill = WorkLifeBalance)) +
        geom_bar(stat = "identity" , position = "dodge",color = "black") + 
        ggtitle("Work Life Balance Rating for Each Role") +
        xlab("Job Role") + 
        ylab("Percentage") +
        coord_flip() +
        theme(plot.background = element_rect("#f5f3ed"), 
            legend.background = element_blank()) + 
        scale_fill_manual(values = color) 
  })
  output$JRYACPlot <- renderPlot({
    color = c("#FF6961","#FAA0A0", "#FAC898","#FDFD96","#C1E1C1", "#A7C7E7", "#C3B1E1", "#F8C8DC","#D1C3B7")
    ggplot(data, aes(fill=JobRole, x = YearsAtCompany)) + 
      geom_boxplot() + 
      ggtitle("Years with Company by Role") +
      xlab("Years with Company") + 
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.background = element_rect("#f5f3ed"), 
            legend.background = element_blank()) +
      scale_fill_manual(values = color)
  })
  output$JRAPlot <- renderPlot({
    color = c("#FF6961","#A7C7E7", "#C3B1E1", "#F8C8DC","#D1C3B7")
    data %>% 
      filter(JobRole == input$Jobrole) %>%
      ggplot(aes(fill=Attrition, y = JobRole)) + 
        geom_bar(position = "fill") + 
        scale_x_continuous(labels = scales::percent) + 
        ggtitle("Percentage of Attrition by Job Role") + 
        xlab("Job Role") +
        ylab("Percentage")+
        scale_fill_manual(values = color) +
        theme(plot.background = element_rect("#f5f3ed"), 
              legend.background = element_blank())
  })
  output$AJSPlot <- renderPlot({
    ggplot(data,aes(fill=Attrition, y = JobSatisfaction)) + 
    geom_bar(position = "fill") + 
    scale_x_continuous(labels = scales::percent) + 
    ggtitle("Percentage of Attrition by Job Role") + 
    xlab("Job Role") +
    ylab("Percentage")+
    scale_fill_manual(values = color) +
    theme(plot.background = element_rect("#f5f3ed"), 
          legend.background = element_blank())
  })
  output$APOTPlot <- renderPlot({
    colors = c("#FF6961","#FDFD96")
    ggplot(data, aes(fill=Attrition, y = OverTime)) + 
      geom_bar(position = "fill") + 
      scale_x_continuous(labels = scales::percent) + 
      ggtitle("Percentage of Attrition by Over Time") + 
      xlab("Over Time") +
      ylab("Percentage")+
      scale_fill_manual(values = colors) +
      theme(plot.background = element_rect("#f5f3ed"), 
            legend.background = element_blank())
  })
  output$AMIPlot <- renderPlot({
    colors = c("#FF6961","#FDFD96")
    ggplot(data = data, aes(fill = Attrition, x = MonthlyIncome)) + 
      geom_density() + 
      facet_wrap(~Attrition) +
      ggtitle("Attrition by Monthly Income") +
      xlab("Monthly Income") + 
      ylab("Density") +
      scale_fill_manual(values = colors) + 
      theme(plot.background = element_rect("#f5f3ed"), legend.position = "none")
  })
 output$AAPlot <- renderPlot({
   data$Age_Bin <- cut(data$Age, breaks=c(18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 61), right = FALSE)
   ggplot(data = data, aes(fill = Attrition, x = Age_Bin)) + geom_bar(position = "dodge")
   
   colors = c("#FF6961","#FDFD96")
   ggplot(data, aes(fill=Attrition, y = Age_Bin)) + 
     geom_bar(position = "fill") + 
     scale_x_continuous(labels = scales::percent) + 
     ggtitle("Percentage of Attrition by Age Group") + 
     xlab("Percentage") +
     ylab("Age Group")+
     scale_fill_manual(values = colors) +
     theme(plot.background = element_rect("#f5f3ed"), 
           legend.background = element_blank())
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
