File Hierarchy 


                                                ---------------------     ---------------------
                                                |      Server       | --- |        UI         |
                                                ---------------------     ---------------------
                                                         |
                                                         |
                  ------------------------------------------------------------------------------
                  |                        |                         |                         |
          ------------------      ---------------------     ---------------------     ---------------------
          |     Energy     |      |    Population     |     |     Emissions     |     |     Prediction    |
          ------------------      ---------------------     ---------------------     ---------------------




# Energy-Consumption-Analysis
This is an R tool built to analyze energy trends in the United States.
It provides a visual UI to interpret how different factors effect the energy
and emission trends in the United States.

This app can be run from within RStudio. To do so simply download the files and
open the server.R file. Ensure you have installed all the libraries listed at
the top of server.R. Once that is done you can click on 'Run App' in the top 
right, or call runApp() in the terminal.

The first run of the program will take a few seconds to render as it fetches
data from an API. However, on subsequent starts it will cache the data and render
instantly as long as your environment hasn't been cleaned.
