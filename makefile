CFLAGS_COMMON = -Wall -Wextra -pedantic -std=c99
TARGET = aya
PREFIX = /usr/local
DESTDIR ?=

# Default target builds for the host system
all: $(TARGET)

$(TARGET): $(TARGET).c
	$(CC) $(CFLAGS_COMMON) $(TARGET).c -o $(TARGET)

# --- Architecture Specific Targets ---

# x86_64 build
build-x86_64:
	x86_64-linux-gnu-gcc $(CFLAGS_COMMON) $(TARGET).c -o $(TARGET)-x86_64

# ARM64 build (requires aarch64 cross-compiler)
build-arm64:
	aarch64-linux-gnu-gcc $(CFLAGS_COMMON) $(TARGET).c -o $(TARGET)-arm64

# --- Installation Targets ---

# Default install (for host system)
install: all
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $(TARGET) $(DESTDIR)$(PREFIX)/bin

# Install x86_64 version
install-x86_64: build-x86_64
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $(TARGET) $(DESTDIR)$(PREFIX)/bin/$(TARGET)

# Install ARM64 version
install-arm64: build-arm64
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp $(TARGET) $(DESTDIR)$(PREFIX)/bin/$(TARGET)

# --- Cleanup ---

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/$(TARGET)

clean:
	rm -f $(TARGET) $(TARGET)-x86_64 $(TARGET)-arm64 *.o

.PHONY: all clean install uninstall build-x86_64 build-arm64 install-x86_64 install-arm64
