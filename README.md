# clockifuck emacs

!!NOT USE THIS, org-mode it's all you need.

*but the reality is impure*

## add api key to ~/.authinfo

example
~~~
machine api.clockify.me login fuck password XXXXXXX
~~~

## example installation .emacs

~~~
(add-to-list 'load-path "/home/clockifuck/bin/clockifuck/")
(require 'clockifuck)

(setq clockifuck-clockify-workspace-id "yyyy")
~~~

##  example usage

inside emacs execute M-x **clockifuck-enable**.

inject property in entry with M-x **clockifuck-project-put**.
~~~
* TODO de nuevo en ideas
:PROPERTIES:
:CLOCKIFY-PROJECT: Myproject/ClientName
:END:
~~~

### get project

~~~
$ clockify-cli project list
~~~

### get workspace
~~~
$ clockify-cli workspaces
~~~
