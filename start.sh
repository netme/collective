#!/bin/sh

APP=collective
COOKIE=collective

cd `dirname $0`
exec erl \
  -hidden \
  -config ./sys \
  -smp auto +K true +A 10 \
  -pa $PWD/ebin $PWD/deps/*/ebin \
  -name $APP \
  -setcookie $COOKIE \
  -boot start_sasl \
  -s $APP \
  -s sync 

