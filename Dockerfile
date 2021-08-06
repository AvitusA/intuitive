FROM rocker/shiny:4.1.0

RUN install2.r \
    devtools \
    dplyr \
    magrittr \
    purrr \
    rlang \
    tidyr \
    tibble \
    ggplot2

RUN rm -r /srv/shiny-server
COPY ./apps /srv/shiny-server

RUN Rscript -e "devtools::install('/srv/shiny-server/ablab_setup/ablabsetup')"

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
USER shiny
CMD ["/usr/bin/shiny-server"]