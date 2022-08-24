#!/bin/bash

if [ -z "${MIXMESH_BASE}" ]; then
    MIXMESH_BASE=/home/pi/erlang
fi

if [ -z "${PLAYER}" ]; then
    >&2 echo "PLAYER is not set"
    exit 1
fi

if [ -n "${BT_HEADSET}" ]; then
    sudo hcitool cmd 0x3F 0x01C 0x01 0x02 0x00 0x01 0x01
    pacmd set-card-profile bluez_card.${BT_HEADSET} headset_head_unit
    pacmd set-default-sink bluez_card.${BT_HEADSET}.headset_head_unit
    pacmd set-default-source bluez_card.${BT_HEADSET}.headset_head_unit
fi

${MIXMESH_BASE}/mixmesh/bin/mixmesh --config ${MIXMESH_BASE}/mixmesh/etc/${PLAYER}.conf -- -detached
