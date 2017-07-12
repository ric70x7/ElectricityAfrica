# text locations in fig_gbox.R

# Left panel
annual_data$left_y[mask00 & annual_data$iso3 == "EGY"] <- annual_data$left_y[mask00 & annual_data$iso3 == "DZA"] - 1
annual_data$left_y[mask00 & annual_data$iso3 == "DZA"] <- annual_data$left_y[mask00 & annual_data$iso3 == "DZA"] + .1
annual_data$left_y[mask00 & annual_data$iso3 == "MAR"] <- annual_data$left_y[mask00 & annual_data$iso3 == "ZAF"] - 1
annual_data$left_y[mask00 & annual_data$iso3 == "CIV"] <- annual_data$left_y[mask00 & annual_data$iso3 == "GNB"] + .5
annual_data$left_y[mask00 & annual_data$iso3 == "GNB"] <- annual_data$left_y[mask00 & annual_data$iso3 == "GNB"] - .5
annual_data$left_y[mask00 & annual_data$iso3 == "NGA"] <- annual_data$left_y[mask00 & annual_data$iso3 == "DJI"] + 1
annual_data$left_y[mask00 & annual_data$iso3 == "CMR"] <- annual_data$left_y[mask00 & annual_data$iso3 == "DJI"] - 1
annual_data$left_y[mask00 & annual_data$iso3 == "BWA"] <- annual_data$left_y[mask00 & annual_data$iso3 == "SEN"]
annual_data$left_y[mask00 & annual_data$iso3 == "GMB"] <- annual_data$left_y[mask00 & annual_data$iso3 == "ERI"] + 1
annual_data$left_y[mask00 & annual_data$iso3 == "AGO"] <- annual_data$left_y[mask00 & annual_data$iso3 == "ERI"]
annual_data$left_y[mask00 & annual_data$iso3 == "SOM"] <- annual_data$left_y[mask00 & annual_data$iso3 == "SOM"] + .3
annual_data$left_y[mask00 & annual_data$iso3 == "SDN"] <- annual_data$left_y[mask00 & annual_data$iso3 == "SDN"] - .3
annual_data$left_y[mask00 & annual_data$iso3 == "BEN"] <- annual_data$left_y[mask00 & annual_data$iso3 == "BEN"] + .4
annual_data$left_y[mask00 & annual_data$iso3 == "COG"] <- annual_data$left_y[mask00 & annual_data$iso3 == "COG"] - .4
annual_data$left_y[mask00 & annual_data$iso3 == "GIN"] <- annual_data$left_y[mask00 & annual_data$iso3 == "GIN"] - .1
annual_data$left_y[mask00 & annual_data$iso3 == "ZMB"] <- annual_data$left_y[mask00 & annual_data$iso3 == "GIN"]
annual_data$left_y[mask00 & annual_data$iso3 == "TGO"] <- annual_data$left_y[mask00 & annual_data$iso3 == "TGO"] - .4
annual_data$left_y[mask00 & annual_data$iso3 == "MDG"] <- annual_data$left_y[mask00 & annual_data$iso3 == "TGO"]
annual_data$left_y[mask00 & annual_data$iso3 == "MLI"] <- annual_data$left_y[mask00 & annual_data$iso3 == "MLI"] - .1
annual_data$left_y[mask00 & annual_data$iso3 == "KEN"] <- annual_data$left_y[mask00 & annual_data$iso3 == "MLI"]
annual_data$left_y[mask00 & annual_data$iso3 %in% c("COD", "SLE")] <- annual_data$left_y[mask00 & annual_data$iso3 == "TZA"]
annual_data$left_y[mask00 & annual_data$iso3 == "BFA"] <- annual_data$left_y[mask00 & annual_data$iso3 == "BFA"] - .3
annual_data$left_y[mask00 & annual_data$iso3 == "UGA"] <- annual_data$left_y[mask00 & annual_data$iso3 == "BFA"]
annual_data$left_y[mask00 & annual_data$iso3 == "NER"] <- annual_data$left_y[mask00 & annual_data$iso3 == "NER"] - .3
annual_data$left_y[mask00 & annual_data$iso3 %in% c("CAF", "MOZ")] <- annual_data$left_y[mask00 & annual_data$iso3 == "NER"]
annual_data$left_y[mask00 & annual_data$iso3 == "MWI"] <- annual_data$left_y[mask00 & annual_data$iso3 == "MWI"] - .2
annual_data$left_y[mask00 & annual_data$iso3 == "LSO"] <- annual_data$left_y[mask00 & annual_data$iso3 == "MWI"]
annual_data$left_y[mask00 & annual_data$iso3 == "BDI"] <- annual_data$left_y[mask00 & annual_data$iso3 == "BDI"] - .4
annual_data$left_y[mask00 & annual_data$iso3 == "RWA"] <- annual_data$left_y[mask00 & annual_data$iso3 == "BDI"]
annual_data$left_y[mask00 & annual_data$iso3 == "TCD"] <- annual_data$left_y[mask00 & annual_data$iso3 == "TCD"] - .3

annual_data$left_x[mask00 & annual_data$iso3 == "BWA"] <- annual_data$left_x[mask00 & annual_data$iso3 == "SEN"] - 1
annual_data$left_x[mask00 & annual_data$iso3 == "ERI"] <- annual_data$left_x[mask00 & annual_data$iso3 == "AGO"] - 1
annual_data$left_x[mask00 & annual_data$iso3 == "ZMB"] <- annual_data$left_x[mask00 & annual_data$iso3 == "TGO"] - 1 
annual_data$left_x[mask00 & annual_data$iso3 == "MDG"] <- annual_data$left_x[mask00 & annual_data$iso3 == "TGO"] - 1 
annual_data$left_x[mask00 & annual_data$iso3 == "KEN"] <- annual_data$left_x[mask00 & annual_data$iso3 == "MLI"] - 1 
annual_data$left_x[mask00 & annual_data$iso3 == "SLE"] <- annual_data$left_x[mask00 & annual_data$iso3 == "TZA"] - 1 
annual_data$left_x[mask00 & annual_data$iso3 == "COD"] <- annual_data$left_x[mask00 & annual_data$iso3 == "TZA"] - 2 
annual_data$left_x[mask00 & annual_data$iso3 == "UGA"] <- annual_data$left_x[mask00 & annual_data$iso3 == "BFA"] - 1 
annual_data$left_x[mask00 & annual_data$iso3 == "MOZ"] <- annual_data$left_x[mask00 & annual_data$iso3 == "NER"] - 1 
annual_data$left_x[mask00 & annual_data$iso3 == "CAF"] <- annual_data$left_x[mask00 & annual_data$iso3 == "NER"] - 2 
annual_data$left_x[mask00 & annual_data$iso3 == "LSO"] <- annual_data$left_x[mask00 & annual_data$iso3 == "MWI"] - 1 
annual_data$left_x[mask00 & annual_data$iso3 == "RWA"] <- annual_data$left_x[mask00 & annual_data$iso3 == "BDI"] - 1 

# Right panel
annual_data$right_y[mask15 & annual_data$iso3 == "LBY"] <- annual_data$right_y[mask15 & annual_data$iso3 == "LBY"] + .2
annual_data$right_y[mask15 & annual_data$iso3 == "TUN"] <- annual_data$right_y[mask15 & annual_data$iso3 == "LBY"]
annual_data$right_y[mask15 & annual_data$iso3 == "DZA"] <- annual_data$right_y[mask15 & annual_data$iso3 == "LBY"]
annual_data$right_y[mask15 & annual_data$iso3 == "EGY"] <- annual_data$right_y[mask15 & annual_data$iso3 == "LBY"]
annual_data$right_y[mask15 & annual_data$iso3 == "MAR"] <- annual_data$right_y[mask15 & annual_data$iso3 == "LBY"] - .9
annual_data$right_y[mask15 & annual_data$iso3 == "NGA"] <- annual_data$right_y[mask15 & annual_data$iso3 == "NGA"] + .3
annual_data$right_y[mask15 & annual_data$iso3 == "CMR"] <- annual_data$right_y[mask15 & annual_data$iso3 == "CMR"] - .2
annual_data$right_y[mask15 & annual_data$iso3 == "DJI"] <- annual_data$right_y[mask15 & annual_data$iso3 == "DJI"] - .1
annual_data$right_y[mask15 & annual_data$iso3 == "TGO"] <- annual_data$right_y[mask15 & annual_data$iso3 == "TGO"] + .2
annual_data$right_y[mask15 & annual_data$iso3 == "SWZ"] <- annual_data$right_y[mask15 & annual_data$iso3 == "SWZ"] - .2
annual_data$right_y[mask15 & annual_data$iso3 == "BEN"] <- annual_data$right_y[mask15 & annual_data$iso3 == "BEN"] - .2
annual_data$right_y[mask15 & annual_data$iso3 == "GMB"] <- annual_data$right_y[mask15 & annual_data$iso3 == "BEN"]
annual_data$right_y[mask15 & annual_data$iso3 == "AGO"] <- annual_data$right_y[mask15 & annual_data$iso3 == "AGO"] - .1
annual_data$right_y[mask15 & annual_data$iso3 == "KEN"] <- annual_data$right_y[mask15 & annual_data$iso3 == "KEN"] + .4
annual_data$right_y[mask15 & annual_data$iso3 == "ERI"] <- annual_data$right_y[mask15 & annual_data$iso3 == "ERI"] - .3
annual_data$right_y[mask15 & annual_data$iso3 == "SDN"] <- annual_data$right_y[mask15 & annual_data$iso3 == "SDN"] + .2
annual_data$right_y[mask15 & annual_data$iso3 == "SOM"] <- annual_data$right_y[mask15 & annual_data$iso3 == "SOM"] - .2
annual_data$right_y[mask15 & annual_data$iso3 == "LSO"] <- annual_data$right_y[mask15 & annual_data$iso3 == "ETH"]
annual_data$right_y[mask15 & annual_data$iso3 == "MLI"] <- annual_data$right_y[mask15 & annual_data$iso3 == "MOZ"]
annual_data$right_y[mask15 & annual_data$iso3 == "ZMB"] <- annual_data$right_y[mask15 & annual_data$iso3 == "ZMB"] - .1
annual_data$right_y[mask15 & annual_data$iso3 == "MRT"] <- annual_data$right_y[mask15 & annual_data$iso3 == "MRT"] + .3
annual_data$right_y[mask15 & annual_data$iso3 == "RWA"] <- annual_data$right_y[mask15 & annual_data$iso3 == "RWA"] - .1
annual_data$right_y[mask15 & annual_data$iso3 == "UGA"] <- annual_data$right_y[mask15 & annual_data$iso3 == "UGA"] + .4
annual_data$right_y[mask15 & annual_data$iso3 == "BFA"] <- annual_data$right_y[mask15 & annual_data$iso3 == "BFA"] + .1
annual_data$right_y[mask15 & annual_data$iso3 == "TZA"] <- annual_data$right_y[mask15 & annual_data$iso3 == "TZA"] - .4
annual_data$right_y[mask15 & annual_data$iso3 == "COD"] <- annual_data$right_y[mask15 & annual_data$iso3 == "TZA"]
annual_data$right_y[mask15 & annual_data$iso3 == "SLE"] <- annual_data$right_y[mask15 & annual_data$iso3 == "SLE"] + .2
annual_data$right_y[mask15 & annual_data$iso3 == "MDG"] <- annual_data$right_y[mask15 & annual_data$iso3 == "SLE"]
annual_data$right_y[mask15 & annual_data$iso3 == "NER"] <- annual_data$right_y[mask15 & annual_data$iso3 == "NER"] - .2
annual_data$right_y[mask15 & annual_data$iso3 == "LBR"] <- annual_data$right_y[mask15 & annual_data$iso3 == "NER"]
annual_data$right_y[mask15 & annual_data$iso3 == "TCD"] <- annual_data$right_y[mask15 & annual_data$iso3 == "TCD"] + .3
annual_data$right_y[mask15 & annual_data$iso3 == "BDI"] <- annual_data$right_y[mask15 & annual_data$iso3 == "BDI"] - .4


annual_data$right_x[mask15 & annual_data$iso3 == "TUN"] <- annual_data$right_x[mask15 & annual_data$iso3 == "LBY"] + 1
annual_data$right_x[mask15 & annual_data$iso3 == "DZA"] <- annual_data$right_x[mask15 & annual_data$iso3 == "LBY"] + 2
annual_data$right_x[mask15 & annual_data$iso3 == "EGY"] <- annual_data$right_x[mask15 & annual_data$iso3 == "LBY"] + 3
annual_data$right_x[mask15 & annual_data$iso3 == "GMB"] <- annual_data$right_x[mask15 & annual_data$iso3 == "BEN"] + 1
annual_data$right_x[mask15 & annual_data$iso3 == "LSO"] <- annual_data$right_x[mask15 & annual_data$iso3 == "ETH"] + 1
annual_data$right_x[mask15 & annual_data$iso3 == "MLI"] <- annual_data$right_x[mask15 & annual_data$iso3 == "MOZ"] + 1
annual_data$right_x[mask15 & annual_data$iso3 == "COD"] <- annual_data$right_x[mask15 & annual_data$iso3 == "TZA"] + 1
annual_data$right_x[mask15 & annual_data$iso3 == "MDG"] <- annual_data$right_x[mask15 & annual_data$iso3 == "SLE"] + 1
annual_data$right_x[mask15 & annual_data$iso3 == "LBR"] <- annual_data$right_x[mask15 & annual_data$iso3 == "NER"] + 1

