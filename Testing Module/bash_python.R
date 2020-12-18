# Get data from binance running a python script from bash
screen -S pull_data_from_binance

cd /media/chris/DATA/Documents/Bot_Trading/Binance_access
python get_data.py ETHEUR 600

binance_data <- "/media/chris/DATA/Documents/Bot_Trading/binance_data"
candles <- fread(list.files(binance_data, full.names = T)[4])

