# Transparency International Corruption Perceptions Index Shiny

This is my final project submission for Communicating with Data; I built a shiny app using Transparency International's data with `reactable` and a `plotly` map.

The working version of the app can be accessed from [this link](https://transparencyintl-geiwez4tia-wn.a.run.app)

The workflow was to 

1. build the app locally until I had what I wanted.
2. Use [jumping rivers great tutorial](https://www.jumpingrivers.com/blog/shiny-auto-docker/) to auto-build the Dockerfile and dependencies.
3. build the container from Dockerfile
4. push the container to cloud run

After I completed the first step, I pulled the data manipulation and almost the entire preamble to the shiny app and saved it in the `data` folder in the `DataMaker.R` file.  The Excel spreadsheet supplied by Transparency International and the RData file storing the data used in the app also reside there.