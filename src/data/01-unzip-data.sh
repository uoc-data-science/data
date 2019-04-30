#!/bin/bash

unzip data/raw/clickstream/clickstream_data.zip -d /tmp/
split -b 50M /tmp/clickstream_data.csv data/interim/clickstream/split_clickstream_data.csv.
