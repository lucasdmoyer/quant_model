This project was for FIN 6392 at the University of Texas at Dallas.

Here a simple quantative model is built using common finanical factors such as RSI, MACD, and 10Q report sentiment data. 

To run this project, run quant_model.R in R Studio.

To skip preprocessing, stock_data_100 can be loaded by using:

stock_data <- readRDS("stock_data_100", refhook = NULL)