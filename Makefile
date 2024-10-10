.PHONY: test
test:
	sbcl --noinform --non-interactive \
             --eval "(asdf:load-system :coalton-mode/tests)" \
	     --eval "(fiasco:run-package-tests \
                       :packages '(:coalton-mode/tests) \
                       :interactive nil)"
