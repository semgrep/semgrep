# MATCH:
RUN if [[ -e foo ]]; then \
      echo "foo exists"; \
    else \
      echo "error: foo is missing"; \
      exit 1; \
    fi
