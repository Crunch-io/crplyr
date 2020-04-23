This file helped me run vdiffr on R-devel using Docker. I hope it's doesn't need to be used in the
future but felt worth documenting. Note that this does not allow you to accept the changes, just view 
them because it creates it's own copy of the package files.

To build the image (from root of project):
```bash
docker build -f dev-misc/r-dev-docker/Dockerfile -t crplyr-rdev .
```

Then to run vdiffr:
```bash
docker run -p 9099:9099 crplyr-rdev
```

Then you can go to localhost:9099 in a browser to see the vdiffr shiny app.