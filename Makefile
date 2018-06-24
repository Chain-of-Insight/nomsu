# Nomsu makefile
# To build, run `make`
# To install, 

# ========= User-controlled variables ========
LUA= lua
LUA_BIN= $(shell which $(LUA))

PREFIX=/usr/local
NOMSU_BIN_DIR= $(PREFIX)/bin
NOMSU_LIB_DIR= $(PREFIX)/lib/nomsu
NOMSU_SHARE_DIR= $(PREFIX)/share/nomsu

# ========= You shouldn't need to mess with any of these variables below ================

MOON_FILES= code_obj.moon error_handling.moon files.moon nomsu.moon nomsu_compiler.moon nomsu_tree.moon parser.moon
LUA_FILES= code_obj.lua consolecolors.lua error_handling.lua files.lua nomsu.lua nomsu_compiler.lua \
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
check_header: $(PEG_FILE) nomsu.lua $(CORE_NOM_FILES) $(LIB_NOM_FILES)
	@if [ -f nomsu_latest ]; then \
		NOMSU_HEADER="#!$(LUA_BIN)\\nlocal NOMSU_VERSION, NOMSU_LIB, NOMSU_SHARE = [[`$(GET_VERSION)`]], [[$(NOMSU_LIB_DIR)]], [[$(NOMSU_SHARE_DIR)]]"; \
		if [ "`head -n 3 nomsu_latest 2>/dev/null`" != "`echo $$NOMSU_HEADER`" ]; then \
			rm -f nomsu_latest; \
		fi; \
	fi;

nomsu_latest: nomsu.lua
	@rm -f nomsu_latest
	@NOMSU_HEADER="#!$(LUA_BIN)\\nlocal NOMSU_VERSION, NOMSU_LIB, NOMSU_SHARE = [[`$(GET_VERSION)`]], [[$(NOMSU_LIB_DIR)]], [[$(NOMSU_SHARE_DIR)]]"; \
	echo $$NOMSU_HEADER | cat - nomsu.lua > nomsu_latest
	@chmod +x nomsu_latest
	@mv -f nomsu_latest nomsu`$(GET_VERSION)`
	@ln -s nomsu`$(GET_VERSION)` nomsu_latest
	@echo "Built nomsu binary"

build: $(LUA_FILES) check_header nomsu_latest

.PHONY: optimize
optimize: build $(CORE_LUA_FILES) $(LIB_LUA_FILES)

.PHONY: clean
clean:
	@echo "Deleting..."
	@rm -rvf nomsu`$(GET_VERSION)` nomsu_latest core/*.lua lib/*.lua

.PHONY: install
install: all
	@echo "Nomsu will be installed to:"
	@echo "    $(NOMSU_BIN_DIR)"
	@echo "    $(NOMSU_LIB_DIR)"
	@echo "    $(NOMSU_SHARE_DIR)"
	@read -p "is that okay? [Y/n] " ans; \
	if [[ ! $$ans =~ ^[Nn] ]]; then \
		mkdir -pv $(NOMSU_BIN_DIR) $(NOMSU_LIB_DIR)/`$(GET_VERSION)` $(NOMSU_SHARE_DIR)/`$(GET_VERSION)` \
		&& cp -v nomsu nomsu`$(GET_VERSION)` $(NOMSU_BIN_DIR) \
		&& cp -rv $(LUA_FILES) $(PEG_FILE) core lib tests $(NOMSU_SHARE_DIR)/`$(GET_VERSION)`; \
	fi

.PHONY: uninstall
uninstall: all
	@echo "Nomsu will be uninstalled from:"
	@echo "    $(NOMSU_BIN_DIR)"
	@echo "    $(NOMSU_LIB_DIR)"
	@echo "    $(NOMSU_SHARE_DIR)"
	@read -p "is that okay? [Y/n] " ans; \
	if [[ ! $$ans =~ ^[Nn] ]]; then \
		echo "Deleting..."; \
		rm -rvf $(NOMSU_LIB_DIR)/`$(GET_VERSION)` $(NOMSU_SHARE_DIR)/`$(GET_VERSION)` $(NOMSU_BIN_DIR)/nomsu`$(GET_VERSION)`; \
		if [ "`ls $(NOMSU_BIN_DIR)/nomsu* 2> /dev/null`" == "nomsu" ]; then \
			rm -vf $(NOMSU_BIN_DIR)/nomsu; \
		else \
			if [ "`ls $(NOMSU_BIN_DIR)/nomsu* 2> /dev/null`" != "" ]; then \
				read -p "It looks like there are other versions of Nomsu installed. Is it okay to leave the 'nomsu' cross-version launcher in place? (recommended) [Y/n]" ans; \
				if [[ $$ans =~ ^[Nn] ]]; then \
					echo "Deleting..."; \
					rm -vf $(NOMSU_BIN_DIR)/nomsu; \
				fi; \
			fi; \
		fi; \
		if [ "`ls $(NOMSU_LIB_DIR) 2>/dev/null`" == "" ]; then rm -rvf $(NOMSU_LIB_DIR);\
		else \
			echo "Retaining $(NOMSU_LIB_DIR), since there are other files there."; \
		fi; \
		if [ "`ls $(NOMSU_SHARE_DIR) 2>/dev/null`" == "" ]; then rm -rvf $(NOMSU_SHARE_DIR);\
		else \
			echo "Retaining $(NOMSU_SHARE_DIR), since there are other files there."; \
		fi; \
	fi

# eof
