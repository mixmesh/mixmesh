#!/bin/bash

if [ ! -z "${MIXMESH_BASE}" ]; then
   export MIXMESH_BASE=/home/pi/erlang
fi

if [ ! -z "${BT_DEVICE}" ]; then
   export BT_DEVICE=20_74_CF_C4_F4_A0
fi

if [ ! -z "${PLAYER}" ]; then
   export PLAYER=mother
fi

sudo -E -u pi ${MIXMESH_BASE}/mixmesh/bin/mixmesh --config ${MIXMESH_BASE}/mixmesh/etc/${PLAYER}.conf -- -detached
