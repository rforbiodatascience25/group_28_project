dir.create("_raw")
input_url <- "https://ftp.ncbi.nlm.nih.gov/geo/datasets/GDS4nnn/GDS4337/soft/GDS4337_full.soft.gz"
output_file <- "_raw/full_dataset.soft.gz"
curl::curl_download(url = input_url, destfile = output_file)
