#!/bin/bash
aptitude update
aptitude upgrade
aptitude install git build-essential python-pip nodejs npm r-base
git clone https://github.com/jchillerup/marathoncafe.git
git checkout rhk
cd marathoncafe
R --save < install_packages.R
ln -s /usr/bin/nodejs /usr/bin/node
./bootstrap.sh

# print the ip
ifconfig eth0 | awk '/inet addr/{print substr($2,6)}'
