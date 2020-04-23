FROM rocker/r-devel
RUN apt-get update && apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev
RUN RD -e "install.packages('devtools'); install.packages('crplyr', dependencies = TRUE)"
COPY ./ /crplyr/
ENTRYPOINT RD -e "vdiffr::manage_cases('/crplyr', options = list(port = 9099, host = '0.0.0.0'))"