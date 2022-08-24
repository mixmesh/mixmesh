#!/bin/bash

if [ "${USER}" != "root" ]; then
    >&2 echo "Must be root user"
    exit 1
fi

if [ -z "${MIXMESH_BASE}" ]; then
    MIXMESH_BASE=/home/pi/erlang
fi

if [ -z "${PLAYER}" ]; then
    >&2 echo "PLAYER is not set"
    exit 1
fi

su --login pi -c "${MIXMESH_BASE}/mixmesh/bin/mixmesh --config ${MIXMESH_BASE}/mixmesh/etc/${PLAYER}.conf -- -detached"
