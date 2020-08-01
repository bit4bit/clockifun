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
(setq clockifuck-clockify-project-id "zzzzzz")
~~~

### get project
~~~
$ clockify-cli project list
~~~

### get workspace
~~~
$ clockify-cli workspaces
~~~
