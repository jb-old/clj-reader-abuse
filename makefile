test: _FORCE
	cake eval '(load-file "test/reader_abuse/iexprs.clj")'
	cake test -v

docs: _FORCE
	docco src/reader_abuse/*.clj

_FORCE: