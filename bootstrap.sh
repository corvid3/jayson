#!/bin/bash

if [ ! -d $HOME/.hewg/bootstrap/crow.jayson ]; then
  mkdir -p $HOME/.hewg/bootstrap/crow.jayson
fi

cp include/jayson.hh $HOME/.hewg/bootstrap/crow.jayson/
