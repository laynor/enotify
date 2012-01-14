SOURCES=header.el enotify-group.el enotify-messages.el enotify-mode-line.el enotify-network.el enotify.el footer.el

all: enotify-big.el

enotify-big.el: $(SOURCES)
	cat $(SOURCES) > enotify-big.el
