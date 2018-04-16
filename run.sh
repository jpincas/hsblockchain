#!/bin/sh

echo "Changes detected. Restarting..."
pkill -f hsblockchain-exe
stack exec hsblockchain-exe &
echo "Waiting for changes..."
