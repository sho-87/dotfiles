# docker build -t <image name> .
# docker run --name <name of container> --gpus all -it <built image name>
# or use docker compose
FROM tensorflow/tensorflow:latest-gpu-jupyter

LABEL version="1.0"

RUN apt-get update && apt-get upgrade -y

WORKDIR /home

COPY requirements.txt requirements.txt
RUN pip3 install -r requirements.txt