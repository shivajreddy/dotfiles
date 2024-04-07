#! /usr/bin/env nix-shell
#! nix-shell --quiet -p uxplay -i bash

set -ueo pipefail

## Clear any existing "DROP ME" rules
## ref: https://stackoverflow.com/a/63855690/12393422
while sudo iptables -L -n --line-number | grep "DROP ME" >/dev/null; do
	sudo iptables -D INPUT $(sudo iptables -L -n --line-number | grep "DROP ME" | head -1 | awk '{print $1}')
done

LOCAL_CIDR=${1:-"192.168.0.0/16"}

open-port-tcp() {
	local port=$1
	echo "Opening tcp port $port from $LOCAL_CIDR ..."
	sudo iptables \
		-I INPUT \
		-p tcp \
		-s $LOCAL_CIDR \
		--dport $port \
		-j ACCEPT \
		-m comment --comment "DROP ME"
}

close-port-tcp() {
	local port=${1:-0}
	echo "Closing tcp port $port from $LOCAL_CIDR ..."
	sudo iptables \
		-D INPUT \
		-p tcp \
		-s $LOCAL_CIDR \
		--dport $port \
		-j ACCEPT \
		-m comment --comment "DROP ME"
}

open-port-udp() {
	local port=$1
	echo "Opening udp port $port from $LOCAL_CIDR ..."
	sudo iptables \
		-I INPUT \
		-p udp \
		-s $LOCAL_CIDR \
		--dport $port \
		-j ACCEPT \
		-m comment --comment "DROP ME"
}

close-port-udp() {
	local port=${1:-0}
	echo "Closing udp port $port from $LOCAL_CIDR ..."
	sudo iptables \
		-D INPUT \
		-p udp \
		-s $LOCAL_CIDR \
		--dport $port \
		-j ACCEPT \
		-m comment --comment "DROP ME"
}

open-port-tcp 7100
open-port-tcp 7000
open-port-tcp 7001
open-port-udp 6000
open-port-udp 6001
open-port-udp 7011

# Ensure port closes if error occurs.
trap "close-port-tcp 7100 && \
      close-port-tcp 7000 && \
      close-port-tcp 7001 && \
      close-port-udp 6000 && \
      close-port-udp 6001 && \
      close-port-udp 7011
      " EXIT

# -p flag is for the ports
# -s is the resolution the programs runs at, which is my ipad resolution
# uxplay -p -s 2732x2048
uxplay -p -s 2048x2732

