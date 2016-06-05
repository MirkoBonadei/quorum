#!/usr/bin/env bash

if [ ! -f /usr/local/bin/kerl ]; then
    echo "kerl not installed...";
    cd /usr/local/bin;
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl;
    chmod a+x kerl;
    echo "kerl is now installed on the guest machine.";

    echo "installing erlang dependecies...";
    apt-get install -y build-essential libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev;
    echo "erlang dependencies are now installed";
else
    echo "kerl is already installed.";
fi

if [ ! -f /usr/local/bin/rebar3 ]; then
    echo "rebar3 not installed...";
    cd /usr/local/bin;
    wget https://s3.amazonaws.com/rebar3/rebar3;
    chmod +x rebar3;

    echo "rebar3 is now installed on the guest machine.";
else
    echo "rebar3 is already installed.";
fi

if git --version; then
    echo "git is already installed.";
else
    apt-get install -y git;
    echo "git is now instaled.";
fi

if autoconf --version; then
    echo "autoconf is already installed.";
else
    sudo apt-get install -y autoconf
    echo "autoconf is now instaled.";
fi

    
