# clockifuck emacs

!!NOT USE THIS, org-mode it's all you need.

*but the reality is impure*

# requirements

 * https://github.com/lucassabreu/clockify-cli
 
## install custom clockify-cli

~~~
$ go get -u github.com/bit4bit/clockify-cli
~~~

## example installation .emacs

~~~
(add-to-list 'load-path "/home/clockifuck/bin/clockifuck/")
(require 'clockifuck)

(setq clockifuck-clockify-token "xxxx")
(setq clockifuck-clockify-workspace-id "yyyy")
(setq clockifuck-clockify-path "~/go/bin/clockify-cli")
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
