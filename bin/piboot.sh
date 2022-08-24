#!/bin/bash

if [ "${USER}" != "root" ]; then
    >&2 echo "Must be root user"
    exit 1
fi

if [ -z "${MIXMESH_BASE}" ]; then
    export MIXMESH_BASE=/home/pi/erlang
fi

if [ -z "${BT_DEVICE}" ]; then
    >&2 echo "BT_DEVICE is not set"
    exit 1
fi

if [ -z "${PLAYER}" ]; then
    >&2 echo "PLAYER is not set"
    exit 1
fi

if [ ! -z "${BT_DEVICE}" ]; then
    /usr/bin/hcitool cmd 0x3F 0x01C 0x01 0x02 0x00 0x01 0x01
    sudo -E -u pi /usr/bin/pacmd set-card-profile bluez_card.${BT_DEVICE} headset_head_unit
    sudo -E -u pi /usr/bin/pacmd set-default-sink bluez_card.${BT_DEVICE}.headset_head_unit
    sudo -E -u pi /usr/bin/pacmd set-default-source bluez_card.${BT_DEVICE}.headset_head_unit
fi

sudo -E -u pi ${MIXMESH_BASE}/mixmesh/bin/mixmesh --config ${MIXMESH_BASE}/mixmesh/etc/${PLAYER}.conf -- -detached
