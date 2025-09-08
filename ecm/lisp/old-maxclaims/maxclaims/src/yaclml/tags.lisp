(defpackage :maxclaims/yaclml/tags
  (:use :cl))

(in-package :it.bese.yaclml)

(def-html-tag <:button :core :event :i18n
              accesskey
	      data-loading-text
              disabled
              name
              onblur
              onfocus
              tabindex
              type
              value)

(def-html-tag <:span :core :event :i18n
	      data-inline-edit)

(def-html-tag <:div :core :event :i18n
	      data-inline-edit)

(def-html-tag <:a :core :i18n :event
	      data-toggle
              accesskey
              charset
              coords
              href
              hreflang
              name
              onblur
              onfocus
              rel
              rev
              shape
              tabindex
              target
              type)

(def-empty-html-tag <:meta
    :i18n 
  charset
  content
  http-equiv
  name
  scheme)

(def-empty-html-tag <:input
    :core :event :i18n
    placeholder
    accept
    accesskey
    alt
    checked
    disabled
    maxlength
    name
    onblur
    onchange
    onfocus
    onselect
    readonly
    size
    src
    tabindex
    type
    usemap
    value
    width
    height)
