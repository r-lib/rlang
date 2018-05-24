
SRC_DIR = $(CURDIR)/../../../src
ZIP_FILE = $(CURDIR)/lib.zip

lib:
	rm -f $(ZIP_FILE) && \
	cd "$(SRC_DIR)" && \
	zip $(ZIP_FILE) config.h lib.c lib/**


print-%: ; @echo $* = $($*)
