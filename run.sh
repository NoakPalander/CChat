#!/usr/bin/env bash

make && erl -eval "cchat:server(), cchat:client(), cchat:client()"
