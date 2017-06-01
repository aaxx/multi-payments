#!/bin/bash -e


function build {
  cd $1
  PATH=$(pwd)/bin:$PATH stack --local-bin-path ./bin install
  upx ./bin/*
  cd -
}

build info
build tx-monitor
