#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin bson/ebin mongodb/ebin -boot start_sasl \
    -sname awkin_dev \
    -s awkin \
    -s reloader
