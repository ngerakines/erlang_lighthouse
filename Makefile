all:
	mkdir -p ebin/
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump

dist-src: clean
	tar zcvf erlang_lighthouse-0.1.tgz src/ support/ Makefile