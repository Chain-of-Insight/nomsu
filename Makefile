# Nomsu makefile
# To build, run `make`
# To install, 

# ========= User-controlled variables ========
LUA= lua
LUA_BIN= $(shell which $(LUA))

PREFIX=/usr/local
NOMSU_BIN_DIR= $(PREFIX)/bin
NOMSU_LIB_DIR= $(PREFIX)/lib/nomsu

# ========= You shouldn't need to mess with any of these variables below ================

MOON_FILES= code_obj.moon error_handling.moon nomsu.moon nomsu_compiler.moon nomsu_tree.moon parser.moon
LUA_FILES= code_obj.lua consolecolors.lua error_handling.lua nomsu.lua nomsu_compiler.lua \
		   nomsu_tree.lua parser.lua utils.lua uuid.lua
CORE_NOM_FILES= $(wildcard core/*.nom)
CORE_LUA_FILES= $(patsubst %.nom,%.lua,$(CORE_NOM_FILES))
LIB_NOM_FILES= $(wildcard lib/*.nom)
LIB_LUA_FILES= $(patsubst %.nom,%.lua,$(LIB_NOM_FILES))
PEG_FILE= nomsu.peg
GET_VERSION= $(LUA_BIN) nomsu.lua --version

all: build optimize

.PHONY: test
test: build optimize
	./nomsu_latest tests

%.lua: %.moon
	@moonc $<

%.lua: %.nom
	@./nomsu_latest -c $<

.PHONY: check_header
check_header: version
	@if [ -f nomsu_latest ]; then \
		NOMSU_HEADER="#!$(LUA_BIN)\\npackage.path = [[$(NOMSU_LIB_DIR)/$$(cat version)/?.lua;]]..package.path\\npackage.nomsupath = [[$(NOMSU_LIB_DIR)/$$(cat version)]]"; \
		if [ "`head -n 3 nomsu_latest 2>/dev/null`" != "`echo $$NOMSU_HEADER`" ]; then \
			rm -f nomsu_latest; \
		fi; \
	fi;

version: $(PEG_FILE) nomsu.lua $(CORE_NOM_FILES) $(LIB_NOM_FILES)
	@$(LUA_BIN) nomsu.lua --version > version

nomsu_latest: nomsu.lua
	@rm -f nomsu_latest
	@NOMSU_HEADER="#!$(LUA_BIN)\\npackage.path = [[$(NOMSU_LIB_DIR)/$$(cat version)/?.lua;]]..package.path\\npackage.nomsupath = [[$(NOMSU_LIB_DIR)/$$(cat version)]]"; \
	echo $$NOMSU_HEADER | cat - nomsu.lua > nomsu_latest
	@chmod +x nomsu_latest
	@mv -f nomsu_latest nomsu`./nomsu_latest --version`
	@ln -s nomsu`$(LUA_BIN) nomsu.lua --version` nomsu_latest
	@echo "Built nomsu binary"

build: $(LUA_FILES) version check_header nomsu_latest

.PHONY: optimize
optimize: build $(CORE_LUA_FILES) $(LIB_LUA_FILES)

.PHONY: clean
clean:
	@echo "Deleting..."
	@rm -rvf nomsu`./nomsu_latest --version 2>/dev/null` nomsu_latest core/*.lua lib/*.lua

.PHONY: install
install: all
	mkdir -pv $(NOMSU_BIN_DIR) && cp -v nomsu nomsu`./nomsu_latest --version` $(NOMSU_BIN_DIR)
	mkdir -pv $(NOMSU_LIB_DIR)/`./nomsu_latest --version` && cp -rv $(LUA_FILES) $(PEG_FILE) core lib $(NOMSU_LIB_DIR)/`./nomsu_latest --version`

.PHONY: uninstall
uninstall: all
	@echo "Deleting..."
	@rm -rvf $(NOMSU_LIB_DIR)/`$(GET_VERSION)` $(NOMSU_BIN_DIR)/nomsu`$(GET_VERSION)`
	@if [ "`ls $(NOMSU_BIN_DIR)/nomsu*`" == "nomsu" ]; then rm -v $(NOMSU_BIN_DIR)/nomsu; fi
	@if [ "`ls $(NOMSU_LIB_DIR) 2>/dev/null`" == "" ]; then rm -rvf $(NOMSU_LIB_DIR); fi

# eof
