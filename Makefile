# Nomsu makefile
# To build, run `make`
# To install, 

# ========= User-controlled variables ========
LUA= lua
LUA_BIN= $(shell which $(LUA))

PREFIX=
UNINSTALL_VERSION=
# ========= You shouldn't need to mess with any of these variables below ================

MOON_FILES= code_obj.moon error_handling.moon files.moon nomsu.moon nomsu_compiler.moon \
			syntax_tree.moon containers.moon bitops.moon parser.moon pretty_errors.moon \
			string2.moon nomsu_decompiler.moon nomsu_environment.moon importer.moon
LUA_FILES= code_obj.lua consolecolors.lua error_handling.lua files.lua nomsu.lua nomsu_compiler.lua \
		   syntax_tree.lua containers.lua bitops.lua parser.lua pretty_errors.lua \
		   string2.lua nomsu_decompiler.lua nomsu_environment.lua importer.lua
CORE_NOM_FILES= $(wildcard core/*.nom)
CORE_LUA_FILES= $(patsubst %.nom,%.lua,$(CORE_NOM_FILES))
LIB_NOM_FILES= $(wildcard lib/*.nom)
LIB_LUA_FILES= $(patsubst %.nom,%.lua,$(LIB_NOM_FILES))
PEG_FILES= $(wildcard nomsu.*.peg)
GET_VERSION= $(LUA_BIN) nomsu.lua --version

all: build optimize

.PHONY: test
test: build optimize
	@echo "\033[1;4mRunning unoptimized tests...\033[0m"
	@$(LUA_BIN) nomsu.lua -O0 tools/test.nom $(CORE_NOM_FILES) $(LIB_NOM_FILES)
	@echo "\n\033[1;4mRunning optimized tests...\033[0m"
	@$(LUA_BIN) nomsu.lua -O1 tools/test.nom $(CORE_NOM_FILES) $(LIB_NOM_FILES)

%.lua: %.moon
	@moonc $<

%.lua: %.nom
	@$(LUA_BIN) nomsu.lua -c $<

.DELETE_ON_ERROR: version
version: $(LUA_FILES) $(CORE_NOM_FILES) $(LIB_NOM_FILES)
	@$(LUA_BIN) nomsu.lua --version > version || exit

build: $(LUA_FILES)

.PHONY: optimize
optimize: build $(CORE_LUA_FILES) $(LIB_LUA_FILES)

.PHONY: clean
clean:
	@echo "\033[1mDeleting...\033[0m"
	@rm -rvf version core/*.lua lib/*.lua tools/*.lua compatibility/*.lua

.PHONY: install
install: build version optimize
	@prefix="$(PREFIX)"; \
	if [[ ! $$prefix ]]; then \
		read -p $$'\033[1mWhere do you want to install Nomsu? (default: /usr/local) \033[0m' prefix; \
	fi; \
	if [[ $$prefix != "`realpath $$prefix`" ]]; then \
		echo $$'\033[1;31mWarning: '$$prefix$$' is not an absolute path. This may cause problems.\033[0m'; \
		read -p $$'\033[1mWould you rather use '`realpath $$prefix`$$' instead? (recommended)[Y/n]\033[0m ' use_real; \
		if [[ ! $$use_real =~ ^[Nn] ]]; then \
			prefix="`realpath $$prefix`"; \
		fi; \
	fi; \
	version="`cat version`"; \
	mkdir -pv $$prefix/bin $$prefix/lib/nomsu/$$version $$prefix/share/nomsu/$$version $$prefix/share/man/man1 \
	&& echo "#!$(LUA_BIN)\\nlocal NOMSU_VERSION, NOMSU_PREFIX = [[$$version]], [[$$prefix]]" | cat - nomsu.lua > $$prefix/bin/nomsu$$version \
	&& chmod +x $$prefix/bin/nomsu$$version \
	&& cp -v nomsu $$prefix/bin \
	&& cp -v doc/nomsu.1 $$prefix/share/man/man1 \
	&& cp -rv $(LUA_FILES) $(PEG_FILES) core lib compatibility tools $$prefix/share/nomsu/$$version;

.PHONY: uninstall
uninstall: version
	@prefix="$(PREFIX)"; \
	if [[ ! $$prefix ]]; then \
		read -p $$'\033[1mWhere do you want to uninstall Nomsu from? (default: /usr/local) \033[0m' prefix; \
	fi; \
	echo "\033[1mNomsu will be uninstalled from:\033[0m"; \
	echo "    $$prefix/bin"; \
	echo "    $$prefix/lib"; \
	echo "    $$prefix/share"; \
	read -p $$'\033[1mis this okay? [Y/n]\033[0m ' ans; \
	if [[ $$ans =~ ^[Nn] ]]; then exit; fi; \
	echo "\033[1mDeleting...\033[0m"; \
	version="$(UNINSTALL_VERSION)"; \
	if [[ ! $$version ]]; then version="`cat version`"; fi;\
	rm -rvf $$prefix/lib/nomsu/$$version $$prefix/share/nomsu/$$version $$prefix/bin/nomsu$$version; \
	if [[ "`find -E $$prefix/bin -type f -regex '.*/nomsu[0-9.]+\$$'`" == "" ]]; then \
		rm -vf $$prefix/bin/nomsu $$prefix/share/man/man1/nomsu.1; \
	else \
		if [ -f $$prefix/bin/nomsu ]; then \
			read -p $$'\033[1mIt looks like there are other versions of Nomsu installed. Is it okay to leave the "nomsu" cross-version launcher in place? (recommended) [Y/n]\033[0m ' ans; \
			if [[ $$ans =~ ^[Nn] ]]; then \
				echo "\033[1mDeleting...\033[0m"; \
				rm -vf $$prefix/bin/nomsu $$prefix/share/man/man1/nomsu.1; \
			fi; \
		fi; \
	fi; \
	if [ "`ls $$prefix/lib/nomsu 2>/dev/null`" == "" ]; then rm -rvf $$prefix/lib/nomsu; fi;\
	if [ "`ls $$prefix/share/nomsu 2>/dev/null`" == "" ]; then rm -rvf $$prefix/share/nomsu; fi;\
	echo $$'\033[1mDone.\033[0m';

# eof
