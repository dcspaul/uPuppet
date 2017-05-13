##########################################
# uPuppet Makefile
##########################################

PROG = uPuppet
OPTS = -fno-warn-tabs

SOURCES = $(shell find Src -name *.hs)
TOOLS = $(shell test -d Tools && find Tools -type f)
TESTS = $(shell test -d Test && find Test -type f)
UNAME = $(shell uname)
ARCH = $(shell arch)
PLATFORM = $(UNAME)-$(ARCH)
PROG_BIN = $(PROG)-$(PLATFORM)

##########################################
# Compile
##########################################

.PHONY: build

build: Bin/$(PROG)

# the generic name is linked to the binary for the current platform
# don't store this in git because it will change depending on your platform

Bin/$(PROG): Bin/$(PROG_BIN) Makefile
	@echo Installing Bin/$(PROG) for $(PLATFORM) ...
	@rm -f Bin/$(PROG)
	@ln Bin/$(PROG_BIN) Bin/$(PROG)
	@touch Bin/$(PROG)

# platform-specific binaries have names which depend on the platform
# these can be checked in to git

Bin/$(PROG_BIN): $(SOURCES) Tmp/Version.hs Makefile
	@mkdir -p Tmp/UPuppet Bin
	@cd Src; ghc $(OPTS) -outputdir ../Tmp/UPuppet -o ../Bin/$(PROG_BIN) \
		--make uPuppet.hs ../Tmp/Version.hs

# record the build time & user

Tmp/Version.hs: $(SOURCES) Makefile
	@mkdir -p Tmp
	@DATE=`date '+%H:%M:%S %a %d/%m/%y'` ;\
	 USERNAME=`git config user.name 2>/dev/null` ;\
	 USEREMAIL=`git config user.email 2>/dev/null` ;\
	 test -n "$$USERNAME" || USERNAME=$$USER ;\
	 test "$$USEREMAIL" && USERNAME="$$USERNAME <$$USEREMAIL>";\
	 echo "module UPuppet.Version ( versionString ) where" >$@ ;\
	 echo "  versionString = \"compiled at $$DATE by $$USERNAME\"" >>$@

##########################################
# Install
##########################################

.PHONY: install install-tools

install: build
	@echo Installing uPuppet ...
	@sudo cp Bin/uPuppet /usr/local/bin || exit 1
	@sudo chmod oug+x /usr/local/bin/uPuppet || exit 1

install-tools:
	@echo Installing Puppet Tools ...
	@sudo rm -rf /usr/share/puppet-tools || exit 1
	@sudo mkdir -p /usr/share/puppet-tools/environments || exit 1
	@sudo cp -R Tools/Environments/* /usr/share/puppet-tools/environments || exit 1
	@sudo cp Tools/puppet-compile.pl /usr/local/bin/puppet-compile || exit 1
	@sudo cp Tools/run-tests.pl /usr/local/bin/run-tests || exit 1
	@sudo chmod oug+x /usr/local/bin/puppet-compile || exit 1

##########################################
# Ubuntu vagrant image
##########################################

.PHONY: create-ubuntu

create-ubuntu: Vagrant/Ubuntu/$(PROG).tgz
	@echo Building Vagrant image ...
	@cd Vagrant/Ubuntu; vagrant destroy ; vagrant up

start-ubuntu:
	@echo Starting Vagrant image ...
	@cd Vagrant/Ubuntu; vagrant up

stop-ubuntu:
	@echo Stopping Vagrant image ...
	@cd Vagrant/Ubuntu; vagrant halt

.PHONY: provision-ubuntu

provision-ubuntu:
	@rm -f Tmp/Version.hs
	@$(MAKE) Tmp/Version.hs
	@$(MAKE) Vagrant/Ubuntu/$(PROG).tgz
	@$(MAKE) Vagrant/Ubuntu/$(PROG)-tools.tgz
	@$(MAKE) Vagrant/Ubuntu/$(PROG)-tests.tgz
	@echo Provisioning Vagrant image ...
	@cd Vagrant/Ubuntu; vagrant provision

Vagrant/Ubuntu/$(PROG).tgz: $(SOURCES) Tmp/Version.hs Makefile
	@echo Packaging Source Distribution: $@ ...
	@tar czf Vagrant/Ubuntu/$(PROG).tgz \
		--exclude .DS_Store --exclude ._.DS_Store \
		Src Tmp/Version.hs Makefile 

Vagrant/Ubuntu/$(PROG)-tools.tgz: $(TOOLS) Makefile
	@echo Packaging Tools: $@ ...
	@tar czf Vagrant/Ubuntu/$(PROG)-tools.tgz \
		--exclude .DS_Store --exclude ._.DS_Store \
		Tools/Environments Tools/puppet-compile.pl Tools/run-tests.pl

Vagrant/Ubuntu/$(PROG)-tests.tgz: $(TESTS) Makefile
	@echo Packaging Tests: $@ ...
	@tar czf Vagrant/Ubuntu/$(PROG)-tests.tgz \
		--exclude .DS_Store --exclude ._.DS_Store Test

##########################################
# Testing
##########################################

.PHONY: test-ubuntu

test-ubuntu: build
	@echo Running Tests ...
	@Tools/run-tests.pl $(ARGS)

##########################################
# Clean
##########################################

.PHONY: clean

clean:
	@echo Cleaning ...
	@rm -rf Tmp/UPuppet
	@rm -f Tmp/*
	@rm -f Bin/$(PROG) Bin/$(PROG_BIN)
