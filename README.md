# clockifuck emacs

!!NOT USE THIS, org-mode it's all you need.

*but the reality is impure*

# requirements

 * https://github.com/lucassabreu/clockify-cli
 
## example installation .emacs

~~~
(add-to-list 'load-path "/home/clockifuck/bin/clockifuck/")
(require 'clockifuck)

(setq clockifuck-clockify-token "xxxx")
(setq clockifuck-clockify-workspace-id "yyyy")
~~~

##  example usage

~~~
* TODO de nuevo en ideas
:PROPERTIES:
:CLOCKIFY-PROJECTID: 5ef4b7fadda3ab39b625315c
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
