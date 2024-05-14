#!/usr/bin/env bash

./test_errmsg_${1:-"Must specify 4/d/8"} 2>&1 | grep "This is an error message."
