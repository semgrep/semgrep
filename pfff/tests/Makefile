DIRS=clang/c mini ml/cmt php/codegraph php/scheck

clean:
	set -e; for i in $(DIRS); do echo $$i; $(MAKE) -C $$i $@; done
