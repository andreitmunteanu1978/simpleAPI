# Use the official R image
FROM rocker/r-ver:latest

# Install required dependencies
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

# Install R packages
RUN R -e "install.packages(c('plumber','readxl','dplyr','base64enc'))"

# Copy API files into the container
COPY groupAPI.R /app/groupAPI.R
WORKDIR /app

# Expose the port
EXPOSE 8000

# Start the Plumber API
CMD ["R", "-e", "pr <- plumber::plumb('/app/groupAPI.R'); pr$run(host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8000)))"]
