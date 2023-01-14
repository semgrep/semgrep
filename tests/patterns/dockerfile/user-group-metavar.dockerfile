USER root

# MATCH:
USER nobody:nogroup

# MATCH:
USER someone:else

# MATCH:
USER $USER:$GROUP

# MATCH:
USER ${USER}:${GROUP}
