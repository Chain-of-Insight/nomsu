# Nomsu makefile
# To build, run `make`
# To install, 

# ========= User-controlled variables ========
LUA= lua
LUA_BIN= /usr/local/bin/$(LUA)

PREFIX=/usr/local
BIN_DIR= $(PREFIX)/bin
NOMSU_DIR= $(PREFIX)/lib/nomsu

# ========= You shouldn't need to mess with any of these variables below ================

MOON_FILES= code_obj.moon error_handling.moon nomsu.moon nomsu_compiler.moon nomsu_tree.moon parser.moon
LUA_FILES= code_obj.lua consolecolors.lua error_handling.lua nomsu.lua nomsu_compiler.lua \
		   nomsu_tree.lua parser.lua utils.lua uuid.lua
CORE_NOM_FILES= $(wildcard core/*.nom)
CORE_LUA_FILES= $(patsubst %.nom,%.lua,$(CORE_NOM_FILES))
LIB_NOM_FILES= $(wildcard lib/*.nom)
LIB_LUA_FILES= $(patsubst %.nom,%.lua,$(LIB_NOM_FILES))
PEG_FILE= nomsu.peg

NOMSU_HEADER=\#!$(LUA_BIN)\npackage.path = [[$(realpath $(NOMSU_DIR))/?.lua;]]..package.path\npackage.nomsupath = [[$(realpath $(NOMSU_DIR))]]

all: build optimize

.PHONY: test
test: build optimize
	./nomsu tests

%.lua: %.moon
	@moonc $<

%.lua: %.nom
	@./nomsu -c $<

.PHONY: check_header
check_header:
	@if [ "`head -n 3 nomsu 2>/dev/null`" != "`echo '$(NOMSU_HEADER)'`" ]; then rm -f nomsu core/*.lua lib/*.lua; fi

nomsu: nomsu.lua
	@echo '$(NOMSU_HEADER)' | cat - nomsu.lua > nomsu
	@chmod +x nomsu
	@echo "Built nomsu binary"

build: $(LUA_FILES) check_header nomsu

.PHONY: optimize
optimize: build $(CORE_LUA_FILES) $(LIB_LUA_FILES)

.PHONY: clean
clean:
	rm -rf nomsu core/*.lua lib/*.lua $(BIN_DIR)/nomsu $(NOMSU_DIR)

.PHONY: install
install: all
	mkdir -p $(BIN_DIR) && cp nomsu $(BIN_DIR)
	mkdir -p $(NOMSU_DIR) && cp -r $(LUA_FILES) $(PEG_FILE) core lib $(NOMSU_DIR)

.PHONY: uninstall
uninstall: all
	rm -rf $(NOMSU_DIR) $(BIN_DIR)/nomsu

# eof
