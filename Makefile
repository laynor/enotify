SOURCES=header.el enotify-group.el enotify-messages.el enotify-mode-line.el enotify-network.el enotify.el footer.el

all: release/enotify.el

release/enotify.el: $(SOURCES)
	mkdir -p release
	cat $(SOURCES) > release/enotify-tmp.el
	sed s/VERSION/`cat VERSION`/g < release/enotify-tmp.el > release/enotify.el
clean:
	-rm release/*
