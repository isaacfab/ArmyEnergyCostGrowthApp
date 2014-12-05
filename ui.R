library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Alternative Financing Forecasting Tool"),
  
  sidebarPanel(strong("Growth Rates"),
    helpText("The sliders allow for adjustment of the base model assumptions.
               The saturation point is determined by the growth or reduction
                of the cost associated with the variables listed.  
              Changing them gives an indication of
             what scenarios might be informative.  All initial values are produced
             using a historical five year average."),
    actionButton("update","Run Model"),
    # Decimal interval with step value
    sliderInput("decimal1", "Energy Savings Performance Contracts:", 
                min = -.2, max = .2, value = .014, step= 0.001),
    sliderInput("decimal1.1", "Electricity:", 
                min = -.2, max = .2, value = .0077, step= 0.001),
    sliderInput("decimal1.2", "Natural Gas:", 
                min = -.2, max = .2, value = -.0798, step= 0.001),
    sliderInput("decimal1.3", "Misc.Energy:", 
                min = -.2, max = .2, value = .0253, step= 0.001),
    sliderInput("decimal2", "Water:", 
                min = -.2, max = .2, value = .0245, step= 0.001),
    sliderInput("decimal3", "Reimbursement:", 
                min = -.2, max = .2, value = .0245, step= 0.001),
    sliderInput("decimal4", "Manpower:", 
                min = -.2, max = .2, value = .0245, step= 0.001),
    sliderInput("decimal5", "Other Alternative Financing:", 
                min = -.2, max = .2, value = .014, step= 0.001),
    sliderInput("decimal6", "UP:", 
                min = -.2, max = .2, value = .0245, step= 0.001),
    sliderInput("decimal7", "II PEG BOS Cost Inflation:", 
                min = -.2, max = .2, value = .0245, step= 0.001)
    ),
  
  mainPanel(
    helpText("The saturation level is defined as the portion
              of the II PEG BOS budget that QDPW (and other utility related
              costs) constitutes.
              The change of this level gives an indication
              of how much impact cost components like alternative financing   
              actually have.",p(),
             "The slider below allows for a one time reduction in energy
             costs.  This value is intended to simulate an immediate reduction
              due to troop level reduction."),
    sliderInput("cut", "Percent Cost Cut To Energy Use:", 
                min = 0, max = .3, value = 0, step= 0.01),
    tabsetPanel(
      tabPanel("Range of Outcomes",
    helpText("The chart shows the distribution (range) of 
              possible saturation outcomes forecasted for
               1, 3, 5 and 10 years.  As is typical with
             predictions the range of outcomes widens as 
             the forecast horizion grows."),
    tableOutput("current"),
    plotOutput("values"),
    helpText("The table shows the probability of a saturation point
             being reached or exceeded in a given year."),
    tableOutput("probs")
    ),
    tabPanel("Average Cost Growth",helpText("This plot shows the cost growth of major utilites costs 
                                              (QDPW total and energy sources) over time
                                            under the current model growth settings."),
             plotOutput("costgrowth"),helpText("This plot shows the cost growth of minor utilites costs 
                                              over time under the current model growth settings."),
             plotOutput("othercostgrowth"))
    )
    )
))