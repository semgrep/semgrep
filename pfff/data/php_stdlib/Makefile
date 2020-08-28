ifndef PFFF_HOME
	PFFF_HOME=$(CURDIR)/../..
endif

# the perl below is because ocaml INT_MAX is smaller than PHP_INT_MAX
# and this causes the json parsing of the file to fail.
update:
	rm -rf hni/ idl/ system/
	mkdir -p hni idl system system/facebook
	$(PFFF_HOME)/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/system/idl idl/
	$(PFFF_HOME)/pfff_test -generate_php_stdlib ~/local/fbcode/hphp/facebook/extensions idl/facebook/
	./stdlib_from_hni.sh ~/local/fbcode/hphp hni
	cp -rf ~/local/fbcode/hphp/system/php/* system/
	cp -rf ~/local/fbcode/hphp/facebook/system/php/* system/facebook/
	find ~/local/fbcode/hphp/facebook/extensions/ -name "*ext_*\.php" \
             -exec cp {} system/facebook \;
	rm -rf system/code_model # too many errors for now
	php builtin_constants.php > system/constants.php

stdlib: update
	rm -rf stdlib/
	mkdir -p stdlib
	cp -r hni idl stdlib/
	for file in `find stdlib/ -name '*.php'`; do nn=`echo $$file | sed s/php/hhi/`; mv $$file $$nn; done
	# For one reason or another, we hand-roll these.
	rm stdlib/idl/builtins_asio.idl.hhi
	rm stdlib/idl/builtins_collections.idl.hhi
	rm stdlib/idl/facebook/builtins_string_buffer.idl.hhi

check:
	$(PFFF_HOME)/codegraph -lang php -build .
	cat pfff.log

for_pad:
	cd ~/local/fbcode/; git pull
	make update
	make check
