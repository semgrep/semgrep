# MATCH:
COPY file1 file2 /mydir/

# MATCH:
COPY --chown=someone:somegroup file1 file2 /mydir/

COPY file1 /mydir/
