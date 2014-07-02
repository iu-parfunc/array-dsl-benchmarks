
# Perform general setup duties before running any individual
# (per-language) benchmark suites.

setup:
	(cd DATA; $(MAKE))

clean:
