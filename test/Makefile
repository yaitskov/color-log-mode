all-tests: $(patsubst %.el,%.elt,$(wildcard *-test.el))
	echo OK: $^

%.elt: %.el ../lisp/*.el
	emacs -batch -l ert --eval "(package-initialize)" -L ../lisp -l $< -f ert-run-tests-batch-and-exit
	touch $@

clean:
	rm -rf *.elt
