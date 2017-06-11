#!/bin/bash -e


function build {
  cd $1
  rm -rf bin && mkdir bin
  PATH=$(pwd)/bin:$PATH stack --local-bin-path ./bin install
  upx ./bin/*
  TAG=jorpic/sonm-ico-${1}:latest
  sudo docker build -t $TAG .
  docker push $TAG
  cd -
}

build api
build price-monitor
# build invoice
# build tx-monitor
