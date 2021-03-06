library(shiny)
library(tidyverse)
library(vegan)

##Filter out just the data we want to use
CatCount=read_csv("CaterpillarsCountData.csv")%>%
    select(SiteName,Region,LocalDate,SurveyLocationCode,OfficialPlantSpecies,UpdatedArthropodGroup,ArthropodQuantity) %>%
    replace(is.na(.),0) %>% filter(UpdatedArthropodGroup != "none") %>%
    filter(Region != "ON")

##Get site-specific species richness
CatDiv=CatCount %>%
    filter(UpdatedArthropodGroup!="none") %>%
    group_by(SiteName,Region,LocalDate,SurveyLocationCode,OfficialPlantSpecies) %>%
    summarize_at(vars(UpdatedArthropodGroup),NROW)

##Figure out the most common tree species
CatTree=CatDiv %>%
    group_by(Region,OfficialPlantSpecies) %>%
    summarize_at(vars(SurveyLocationCode),NROW)%>%
    slice_max(SurveyLocationCode,n=5)%>%
    group_by(OfficialPlantSpecies) %>%
    summarize_at(vars(SurveyLocationCode),NROW)

##Filter out just the tree species that are most common in 3+ states
CatPlant=CatDiv %>%
    filter(OfficialPlantSpecies=="American beech"|OfficialPlantSpecies=="Red maple"|
           OfficialPlantSpecies=="Sugar maple"|OfficialPlantSpecies=="Sweetgum"|
           OfficialPlantSpecies=="Great rhododendron"|OfficialPlantSpecies=="Northern spicebush")

##Make a table that can be read by vegdist
CatDist=CatCount %>%
    group_by(SiteName,Region,LocalDate,SurveyLocationCode,OfficialPlantSpecies) %>%
    pivot_wider(names_from=UpdatedArthropodGroup,values_from=ArthropodQuantity,values_fn=length) %>%
    replace(is.na(.),0)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Beta-Diversity of Bugs"),
    p("Measuring patterns in diversity is crucial in understanding how ecosystems 
                       function and the importance of certain species across the landscape. 
                       These measurements largely stem from the number of species found in a 
                       local area (alpha-diversity) and differences between the species found in 
                       multiple communities (beta-diversity)."),
    p("The data for this app comes from Caterpillars Count!, a community science program whose goal 
                        is to measure phenology, or the seasonal variation, of arthropods. A subset 
                        of the data is included in this app, focusing on the state where the data 
                        was collected and the most commonhost plants that were sampled. You can 
                        find more information at the project website:"),
    p(a(href="https://caterpillarscount.unc.edu/", "https://caterpillarscount.unc.edu/")),
    p("This app is part of a teaching module created by Jasper S. Leavitt at East Carolina 
                        University. The goal is to allow students to change the features being 
                        compared to see how beta-diversity changes based on outside variables. 
                        The code can be found here: "),
    p(a(href="https://github.com/leavittj/BIOL7970Module", 
                        "https://github.com/leavittj/BIOL7970Module")),

    ## Sidebar with our two variables we can test 
    sidebarLayout(
        sidebarPanel(
            radioButtons("variable",
                        "Choose a feature to focus on:",
                        choices=c(State="Region",
                          Plant="OfficialPlantSpecies")
            ),
    ## Only show the checkbox for the group you've selected above
        conditionalPanel(
            condition="input.variable=='Region'",
            checkboxGroupInput("treat",
                               "Select the states you want to compare:",
                               sort(unique(CatDiv$Region))
                               )
            ),
        conditionalPanel(
            condition="input.variable=='OfficialPlantSpecies'",
            checkboxGroupInput("treat2",
                               "Select the host plants you want to compare:",
                               sort(unique(CatPlant$OfficialPlantSpecies)))
            )
        
        ),
        # Show a plot and a summary table
        mainPanel(
           plotOutput("divPlot"),
           dataTableOutput("divData")
        )
    
    ),
    ##Suppresses initial warning message about renaming columns that don't exist 
    #(comes up when there aren't any options selected yet)
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    )
)

# Define server logic required
server <- function(input, output) {

    output$divPlot <- renderPlot({
        ## If you've selected State, make this plot
        if (input$variable == "Region") {
            CatReg <- CatDiv %>%
                filter(Region %in% input$treat) %>%
                group_by(Region) %>%
                summarize_at(vars(UpdatedArthropodGroup), mean)
            ggplot(CatReg, aes(x=Region,y=UpdatedArthropodGroup)) +
                geom_col() + theme_classic() +
                labs(x = "State", y = "Average Richness")
        ## If not, make this plot    
        } else {
           CatPlant <- CatDiv %>%
               filter(OfficialPlantSpecies %in% input$treat2) %>%
               group_by(OfficialPlantSpecies) %>%
               summarize_at(vars(UpdatedArthropodGroup), mean)
           ggplot(CatPlant, aes(x=OfficialPlantSpecies,y=UpdatedArthropodGroup)) +
               geom_col() + theme_classic() +
               labs(x = "Host Species", y = "Average Richness")
        }
       
    })
    
    output$divData <- renderDataTable({
        if (input$variable == "Region") {
            Statelist = list()
            for (i in input$treat) {
                Filter = assign(paste("State", i, sep="_"), filter(CatDist, Region == i))
                StateDist = vegdist(Filter[,c(6:20)],method="bray")
                Statelist[[i]] = c(i, mean(StateDist))
            }
            Statedf = do.call("rbind", Statelist)
            Statedf = Statedf %>% as_tibble(.name = Beta.Diversity) %>%
                rename(`Beta-Diversity` = V2) %>%
                rename(State = V1) %>%
                mutate_at(2, funs(as.numeric)) %>%
                mutate_at(2, round, 3)
            
        } else {
            Plantlist = list()
            for (i in input$treat2) {
                Filter2 = assign(paste("Plant", i, sep="_"), filter(CatDist, OfficialPlantSpecies == i))
                PlantDist = vegdist(Filter2[,c(6:20)],method="bray")
                Plantlist[[i]] = c(i, mean(PlantDist))
            }
            Plantdf = do.call("rbind", Plantlist)
            Plantdf = Plantdf %>% as_tibble(.name = Beta.Diversity) %>%
                rename(`Beta-Diversity` = V2) %>%
                rename(`Host Plant` = V1) %>%
                mutate_at(2, funs(as.numeric)) %>%
                mutate_at(2, round, 3)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
