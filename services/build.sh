#!/bin/bash -e


function build {
  cd $1
  rm -rf bin && mkdir bin
  PATH=$(pwd)/bin:$PATH stack --local-bin-path ./bin install
  # upx ./bin/*
  cd -
}

build info
build invoice
build tx-monitor
