#!/bin/sh

jsonpat -p '{ files: x } -> x >> flatten >> [fname, {"micro_level": l} | {"macro_level": l }] -> map [([x, y] -> fname + ":" + x + ":" + y), l] >> flatten' $* | sed -e 's/^"//'
