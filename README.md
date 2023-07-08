# Clockifun for emacs

!!NOT USE THIS, org-mode it's all you need.

*but the reality is impure*


Configure on your file .emacs
``` lisp
(add-to-list 'load-path "<SOURCE CODE DIRECTORY>")
(require 'clockifun)
```

### Gitea provider

Add api key to ~/.authinfo

example
~~~
machine <HOST> login <USER> password <TOKEN>
~~~

```lisp
(setq clockifun-stopwatcher clockifun-stopwatcher-gitea)
(setq clockifun-gitea-host "<HOST>")
```

#### USAGE 

clockifun can detects the issue id from the task

```org
* TASK #ISSUEID
```

usage recommendation

```org
* repository demo # run M-x clockifun-project-put
** TASK #1
** TASK #2
* repository mero # run M-x clockifun-project-put
** TASK #3
** TASK #4
```

### Clockify Provider

Add api key to ~/.authinfo

example
~~~
machine api.clockify.me login fuck password XXXXXXX
~~~


``` lisp
(setq clockifun-stopwatcher clockifun-stopwatcher-clockify)
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
Starts org-clock with `C-c C-x i` and stops with `C-c C-x o`

```org
* TODO de nuevo en ideas
```

Proyect are search at high levels.

```org
* TODO de nuevo en ideas
** TODO usa propiedades de org superior
```
