##  Makefile

IDRIS := idris
LIB   := freyja
OPTS  :=

.PHONY: clean lib install clobber check doc

install: lib
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete

clobber : clean
	find . -name "*.ibc" -delete
	rm -f freyja.xsd

check: clobber
	${IDRIS} --checkpkg ${LIB}.ipkg

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg

freyja.xsd: freyja.rnc
	trang -I rnc -O xsd freyja.rnc freyja.xsd

sample: freyja.xsd
	xsd2inst -name pattern freyja.xsd > sample.xml
