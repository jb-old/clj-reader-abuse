test: _FAKE
	cake reload
	cake test -v

docs: _FAKE
	docco src/reader_abuse/*.clj

_FAKE: