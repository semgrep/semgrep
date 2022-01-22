USER root

USER nobody:nogroup

# MATCH:
USER $USER:$GROUP

# MATCH:
USER ${USER}:${GROUP}
