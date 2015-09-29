##  Makefile

IDRIS := idris
LIB   := freyja
OPTS  :=

.PHONY: clean lib

install: lib
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete

clobber : clean
	find . -name "*.ibc" -delete

check: clobber
	${IDRIS} --checkpkg ${LIB}.ipkg

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg
