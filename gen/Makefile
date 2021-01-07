SHELL     := /usr/bin/env bash
TOP_DIR   := ..
MODEL_DIR := model
MODEL_ABSOLUTE_DIR := $(PWD)/model
OUT_DIR   := $(PWD)/..
BIN_DIR   := ../bin
GENERATE  := $(BIN_DIR)/amazonka-gen
BRITTANY  := $(BIN_DIR)/brittany
STYLISH   := $(BIN_DIR)/stylish-haskell

define version =
$(shell sed -n 's/^version: *\([0-9]*\.[0-9]*\.[0-9]\).*$$/\1/p' $(1))
endef

VERSION        = $(shell sed -n 's/^version: *\(.*\)$$/\1/p' amazonka-gen.cabal)
CORE_VERSION   = $(call version,$(TOP_DIR)/core/amazonka-core.cabal)
CLIENT_VERSION = $(call version,$(TOP_DIR)/gen/amazonka-gen.cabal)

MODELS ?= $(sort $(dir $(wildcard $(MODEL_ABSOLUTE_DIR)/*/*)))

.PHONY: gen format $(GENERATE)

all: gen format

debug:
	@echo $(MODELS)

gen: $(MODEL_DIR) $(GENERATE)
	$(GENERATE) \
 --out=$(OUT_DIR) \
 --library-version=$(VERSION) \
 --client-version=$(CLIENT_VERSION) \
 --core-version=$(CORE_VERSION) \
 --annexes=$(PWD)/annex \
 --configs=$(PWD)/config \
 --templates=$(PWD)/template \
 --static=$(PWD)/static \
 --retry=$(MODEL_ABSOLUTE_DIR)/_retry.json \
 $(addprefix --model=,$(MODELS))

format: $(STYLISH)
	@echo -e '\nFormatting...'
	@find \
	$(wildcard $(OUT_DIR)/amazonka-*/gen) \
	$(wildcard $(OUT_DIR)/amazonka-*/test/Test/AWS/Gen) \
	-type f \
	-name '*.hs' \
	-exec sh -c "printf ' -> %s\n' $1 {}; $(BRITTANY) --write-mode=inplace {}; $(STYLISH) -i {}" \;

$(GENERATE): $(BIN_DIR)
	stack install --nix amazonka-gen

$(BRITTANY): $(BIN_DIR)
	stack install --nix brittany

$(STYLISH): $(BIN_DIR)
	stack install --nix stylish-haskell

$(BIN_DIR):
	mkdir $@

clean-model:
	rm -rf $(MODEL_DIR)

clean: clean
	stack clean --nix
	rm -rf $(MODEL_DIR) $(OUT_DIR)/amazonka-*/gen

full-clean: clean
	rm -rf vendor $(GENERATE) $(STYLISH)

$(MODEL_DIR): vendor/botocore
	git checkout -- model
	git clean -fd model
	rsync -a --delete --exclude-from=pinned-models.txt vendor/botocore/botocore/data/ $(MODEL_DIR)

vendor/botocore:
	git clone https://github.com/boto/botocore.git $@

.PHONY: audit

ACTUAL = $(sort $(addsuffix \\n,$(notdir $(wildcard config/*.json))))
EXPECT = $(sort $(addsuffix .json\\n,$(notdir $(patsubst %/,%,$(wildcard model/*/)))))

audit: $(MODEL_DIR)
	@diff -B -y <(echo -e $(ACTUAL)) <(echo -e $(EXPECT))
