#!/bin/bash

pulseaudio --start

if [ -z "${MIXMESH_BASE}" ]; then
    MIXMESH_BASE=/home/pi/erlang
fi

if [ -z "${PLAYER}" ]; then
    >&2 echo "PLAYER is not set"
    exit 1
fi

if [ -n "${BT_HEADSET}" ]; then
    touch /tmp/piboot.log
    sudo hcitool cmd 0x3F 0x01C 0x01 0x02 0x00 0x01 0x01 > /tmp/piboot.log 2>&1
    pacmd set-card-profile bluez_card.${BT_HEADSET} headset_head_unit > /tmp/piboot.log 2>&1
    pacmd set-default-sink bluez_sink.${BT_HEADSET}.headset_head_unit > /tmp/piboot.log 2>&1
    pacmd set-default-source bluez_source.${BT_HEADSET}.headset_head_unit > /tmp/piboot.log 2>&1
fi

${MIXMESH_BASE}/mixmesh/bin/mixmesh --config ${MIXMESH_BASE}/mixmesh/etc/${PLAYER}.conf -- -detached  > /tmp/piboot.log 2>&1
