#!/usr/bin/env bash

if [ ! -f /home/vagrant/erlang-solutions_1.0_all.deb ]; then
    wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb > /dev/null 2>&1;
    sudo dpkg -i erlang-solutions_1.0_all.deb > /dev/null 2>&1;
fi

sudo apt-get -y update && sudo apt-get install -y erlang

if [ ! -f /usr/local/bin/rebar3 ]; then
    echo "rebar3 not installed...";
    cd /usr/local/bin;
    wget https://s3.amazonaws.com/rebar3/rebar3 > /dev/null 2>&1;
    chmod +x rebar3;

    echo "rebar3 is now installed on the guest machine.";
else
    echo "rebar3 is already installed.";
fi
    
