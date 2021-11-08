#!/bin/bash
podman build -t intuitive .
podman run -p 3838:3838 -it intuitive
