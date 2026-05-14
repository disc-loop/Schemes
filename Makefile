GUILE = GUILE_LOAD_PATH=.

run:
	$(GUILE) guile the-seasoned-schemer.scm

repl:
	$(GUILE) guile
