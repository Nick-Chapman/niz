
BINS := \
./_build/default/bin/niz.exe \
./_build/default/bin/disassemble.exe \
./_build/default/bin/dump_dictionary.exe \
./_build/default/bin/dump_header.exe \
./_build/default/bin/dump_object_table.exe \
./_build/default/bin/niz.exe  \
./_build/default/bin/print_all_text.exe \

default:
	jbuilder build $(BINS)

clean:
	rm -rf _build

.PHONY: default clean
