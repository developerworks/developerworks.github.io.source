#!/bin/bash

NPM_PATH=`which npm`
PWD=`pwd`

function check_npm_command_exists(){
    if [ ! -f "$NPM_PATH" ]; then
        echo "false";
    else
        echo "true";
    fi
}
if [ "$(check_npm_command_exists)" == "true" ]; then
    npm init custom_boostrap
else
    echo "You shoud install a ndoe.js binary package or compile it from source to make npm command available"
fi
