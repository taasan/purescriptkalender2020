HOOKS = $(addprefix .git/hooks/, pre-commit pre-push)

.PHONY: all
all: intl lint $(HOOKS)

hooks: $(HOOKS)

.git/hooks/%: hooks/hooks.sh
	ln -s  ../../$< $@
