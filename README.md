# Enotify: a networked notification system for emacs

Enotify provides some sort of system tray for emacs.

It is born as a vehicle for TDD notifications, as the writer does not like annoying popups floating around his tiled workspace.

An application can connect to enotify and send a notification message, that will discreetly appear in the emacs mode-line.

## INSTALLATION

If you are updating from an old version, please check the INCOMPATIBLE-CHANGES file
for possible breakages.

Get the code, add the enotify directory to your emacs load-path and require enotify:

```lisp
	(add-to-list 'load-path "path/to/enotify")
    (require 'enotify)
    (enotify-minor-mode t)
```

If you customized the port number related variables (namely
*enotify-port, enotify-use-next-available-port,
enotify-fallback-ports*), ensure that the form (enotify-minor-mode t)
gets evaluated /after/ your customizations, or else your changes won't
affect enotify's startup.

## USAGE

Enotify uses the TCP port 5000 by default. You can customize
`enotify-default-port' if you want.  The variable
*enotify-use-next-available-port* contains a list of ports to be used
as a fallback when binding *enotify-default-port* fails.

It is also possible to instruct enotify to try increasing port numbers
(starting from *enotify-default-port* or the last port specified in
*enotify-fallback-ports* if this s available) for a fixed number of
times or indefinately until port 65535.  This is done throug the
custom variable *enotify-use-next-available-port*.

The function *enotify-port* sends a message that displays what port
enotify is currently running on.

Messages are sent as strings and have this format:

----------------
    |<message size>|<message body>
----------------

Message bodies have the form of a keyword argument list, like

```lisp
	(:register "MySlotID")
```

The message size is intended as the length in characters of the message body.

### Enotify slots

An application that wants to send notifications should register a *slot*
- the equivalent of an icon in a system tray - before sending any notification message.

The message used to register a slot

```lisp
	(:register <slot-name> :handler-fn <message-data-processing-funcion>)
```

The function passed as :handler-fn is of the form

```lisp
	(handler-fn slot-id data)
```
whose purpose of the :handler-fn parameter will be clarified in the following section.

### Notifications

The message used to send a notification has the form

```lisp
	(:id <slot-name>
	 :notification (:text <slot text>
	                :face <slot face>
			:help <tooltip text>
			:mouse-1 <mouse-1 handler>)
	 :data <additional-data>)
```

- **data**: it will be passed to the handler function specified in the slot registration message
- **text**: the text to be displayed in the notification area
- **face**: the face used to display the text in the notification area
- **help**: tooltip text on mouse-over
- **mouse-1**: an (iteractive "e") handler function of the form

  	       ----
			(m1-handler event)
	       ----

	       It's possible to retrieve the slot id with

	       ----
			(enotify-event->slot-id event)
	       ----

## Ruby/Rails/Rspec/Watchr TDD application
As of now, [laynor/spectator-emacs][laynor/spectator-emacs] is the only application for enotify.
If you are interested, you can look at the code and see how it works.

It's best used together with [Rspec Org Formatter][RspecOrgFormatter], that provides
org-mode formatted text for rspec results.

[laynor/spectator-emacs]: https://github.com/laynor/espectator-emacs
[RspecOrgFormatter]: https://github.com/laynor/rspec_org_formatter
