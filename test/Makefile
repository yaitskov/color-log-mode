all-tests: $(patsubst %.el,%.elt,$(wildcard *-test.el))
	echo OK: $^

color-log-mode-autoloads.el: ../lisp/color-log-mode.el
	emacs -batch --eval "(package-initialize)" -l package \
		-L ../lisp --eval "(package-generate-autoloads 'color-log-mode \"../lisp\")"

%.elt: %.el ../lisp/*.el ../lisp/color-log-mode-autoloads.el
	emacs -batch -l ert -L ../lisp --eval "(package-initialize)" -l $< -f ert-run-tests-batch-and-exit
	touch $@

clean:
	rm -rf *.elt ../list/*-autoloads.el
