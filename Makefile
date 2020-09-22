LIBS=mpa elgamal
TESTS=elgamal/test

all:
	for lib in $(LIBS) ; do \
		(cd ../$$lib && $(MAKE) all) || exit 1; \
	done
	for test in $(TESTS) ; do \
		(cd ../$$test && $(MAKE) all) || exit 1; \
	done

clean:
	for test in $(TESTS) ; do \
		(cd ../$$test && $(MAKE) clean) || exit 1; \
	done
	for lib in $(LIBS) ; do \
		(cd ../$$lib && $(MAKE) clean) || exit 1; \
	done

mrproper: clean cleanfluff

cleanfluff:
	for lib in $(LIBS) ; do \
		find ../$$lib \( -name erl_crash.dump -or -name "*~" -or -name '#*' -or -name '.#*' \) -exec rm {} \;; \
	done
