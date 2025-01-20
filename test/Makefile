all-tests: $(patsubst %.el,%.elt,$(wildcard *-test.el))
	echo OK: $^

%.elt: %.el ../lisp/*.el
	emacs -batch --eval "(package-initialize)" -l package \
		-L ../lisp --eval "(package-generate-autoloads 'color-log-mode \"../lisp\")"
	emacs -batch -l ert -L ../lisp --eval "(package-initialize)" -l $< -f ert-run-tests-batch-and-exit
	touch $@

clean:
	rm -rf *.elt ../list/*-autoloads.el
