server <- function(input, output, session) {
    
    output$beers <- renderTable({
        req(input$beers)
        beers <- read.csv(input$beers$datapath)
        head(beers)
    })
    
    output$breweries <- renderTable({
        req(input$breweries)
        breweries <- read.csv(input$breweries$datapath)
        head(breweries)
    })
    
    output$ABVhist <- renderPlot({
        beers <- read.csv(input$beers$datapath)
        breweries <- read.csv(input$breweries$datapath)
        mergedata <- merge(x = beers, y = breweries,
                           by.x = "Brewery_id", by.y = "Brew_ID", all = TRUE)
        
        mergedata <- mergedata %>% filter(State %in% input$id)
        
        if(input$dist == "hist")
        {   
            hist(mergedata$ABV,
                 probability = TRUE,
                 xlab = "ABV",
                 main = "Alcohol by Volume (ABV)")
        }
        else
        {
            boxplot(mergedata$ABV,
                    main = "Alcohol by Volume (ABV)",
                    horizontal = TRUE)
        }
    })
    
    output$IBUhist <- renderPlot({
        beers <- read.csv(input$beers$datapath)
        breweries <- read.csv(input$breweries$datapath)
        mergedata <- merge(x = beers, y = breweries,
                           by.x = "Brewery_id", by.y = "Brew_ID", all = TRUE)
        
        mergedata <- mergedata %>% filter(State %in% input$id) %>% filter(!is.na(IBU))
        mergedata <- as.vector(mergedata$IBU)
        
        if(input$dist == "hist")
        {
            hist(mergedata,
                 probability = TRUE,
                 xlab = "IBU",
                 main = "International Bitterness Units (IBU)")
        }
        else
        {
            boxplot(mergedata,
                    main = "International Bitterness Units (IBU)",
                    horizontal = TRUE)
        }
    })
    
    observe({
        req(input$breweries)
        breweries <- read.csv(input$breweries$datapath)
        breweries <- breweries[order(breweries$State),]
        state <- unique(breweries$State)
        
        updatePickerInput(
            session,
            "id",
            choices = unique(breweries$State),
            select = state
        )
    })
    
    output$scatter <- renderPlot({
        beers <- read.csv(input$beers$datapath)
        breweries <- read.csv(input$breweries$datapath)
        mergedata <- merge(x = beers, y = breweries,
                           by.x = "Brewery_id", by.y = "Brew_ID", all = TRUE)
        
        mergedata <- mergedata %>% filter(State %in% input$id)
        
        if(input$line == 0)
        {
            ggplot(data=mergedata,
                   aes(x=ABV, y=IBU),
                   main = "ABV vs. IBU Scatter Plot") + 
                geom_point()
        }
        else
        {
            ggplot(data=mergedata,
                   aes(x=ABV, y=IBU),
                   main = "ABV vs. IBU Scatter Plot") +
                geom_point() + 
                geom_smooth(method = 'lm')
        }
    })
    
    output$usmap <-renderPlot({
        breweryDataOrig = as.data.frame(read.csv(input$breweries$datapath))
        
        Breweries.in.each.state = breweryDataOrig %>% group_by(State) %>% tally()
        names(Breweries.in.each.state)[1] <- "state"
        Breweries.in.each.state$state <-
            str_replace_all(string=Breweries.in.each.state$state, 
                            pattern=" ", repl="")
        
        centroid_labels <- 
            utils::read.csv(system.file("extdata",
                                        paste0("us_", "states", "_centroids.csv"), 
                                        package = "usmap"), stringsAsFactors = FALSE)
        
        labels <- 
            merge(x = centroid_labels, 
                  y = Breweries.in.each.state, 
                  by.x = "abbr", 
                  by.y = "state", 
                  all.x=TRUE)
        
        names(labels)[6] <- "breweries_count"
        
        plot_usmap(data = Breweries.in.each.state, values = "n", labels=FALSE) + 
            scale_fill_continuous(low = "white", 
                                  high = "orange", 
                                  name = "# of Breweries", 
                                  label = scales::comma) + 
            theme(legend.position = "right") + 
            theme(panel.background = element_rect(colour = "black")) + 
            labs(title = "Number of Breweries by State") + 
            geom_text(data = labels, 
                      ggplot2::aes(x = x, y = y, 
                                   label = scales::number(breweries_count,accuracy = 1)),
                      color = "black")
    })
}