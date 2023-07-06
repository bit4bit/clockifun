# Clockifun for emacs

!!NOT USE THIS, org-mode it's all you need.

*but the reality is impure*

Add api key to ~/.authinfo

example
~~~
machine api.clockify.me login fuck password XXXXXXX
~~~

Configure on your file .emacs

``` lisp
(add-to-list 'load-path "/home/clockifun/bin/clockifun/")
(require 'clockifun)

(setq clockifun-clockify-workspace-id "GET_IT_FROM_CLOCKIFY")
```
Or if you are using doom, it can be add in file .doom.d/config.el, be careful is needed to have the command of clockify-cli in use
``` lisp
;; clockifun
(use-package! clockifun)
(setq clockifun-clockify-workspace-id "GET_IT_FROM_CLOCKIFY")
```

## Usage
Inside emacs execute M-x **clockifun-enable**.

Inject property in entry with M-x **clockifun-project-put**.
``` org
* TODO de nuevo en ideas
:PROPERTIES:
:CLOCKIFY-PROJECT: Myproject/ClientName
:END:
```

Proyect are search at high levels.

```org
* TODO de nuevo en ideas
:PROPERTIES:
:CLOCKIFY-PROJECT: Myproject/ClientName
:END:
** TODO usa propiedades de org superior
```
