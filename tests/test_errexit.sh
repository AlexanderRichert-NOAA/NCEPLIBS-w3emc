#!/usr/bin/env bash

for i in 0 1; do
  ./test_errexit_${1:-"Must specify 4/d/8"} $i
  if [ $? -ne $i ]; then
    echo "Failure: $? != $i"
    exit $i
  fi
done
