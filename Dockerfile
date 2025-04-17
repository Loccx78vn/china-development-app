# Start with a base R image
FROM rocker/r-ver:4.4.2

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    nodejs \
    npm \
    pandoc \
    git \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy renv files first (for better caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/ renv/

# Install renv
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org/')"

# Restore packages using renv
RUN R -e "renv::restore()"

# Now copy the rest of the app
COPY . .

# Build the app with rhino
RUN R -e "rhino::build()"

# Expose the port Shiny will run on
EXPOSE 8080

# Run the app
CMD ["R", "-e", "options('shiny.port'=8080,shiny.host='0.0.0.0'); rhino::app()$run()"]