# syntax=docker/dockerfile:experimental
FROM rocker/shiny-verse:4.2

RUN --mount=type=bind,source=r-packages.txt,target=/r-packages.txt \
    install2.r --error --skipmissing --skipinstalled -n $(nproc) $(cat /r-packages.txt)

EXPOSE 3838
