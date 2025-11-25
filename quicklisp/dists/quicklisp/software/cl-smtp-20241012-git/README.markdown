# CL-SMTP - A simple common lisp smtp client.

## Features
* It should comply with
  * [RFC 5321 - Simple Mail Transfer Protocol](https://datatracker.ietf.org/doc/html/rfc5321)
    * [RFC 7504 - SMTP 521 and 556 Reply Codes](https://datatracker.ietf.org/doc/html/rfc7504)
  * [RFC 5322 - Internet Message Format](https://datatracker.ietf.org/doc/html/rfc5322)
    * [RFC 6854 - Update to Internet Message Format to Allow Group Syntax in the "From:" and "Sender:" Header Fields](https://datatracker.ietf.org/doc/html/rfc6854)
  * [RFC 2045 - Multipurpose Internet Mail Extensions (MIME) Part One: Format of Internet Message Bodies](https://datatracker.ietf.org/doc/html/rfc2045)
  * [RFC 2231 - MIME Parameter Value and Encoded Word Extensions: Character Sets, Languages, and Continuations](https://datatracker.ietf.org/doc/html/rfc2231)
  * All non-conforming behaviour is considered a bug.
* works on *ACL*, *CCL*, *SBCL*, *CMUCL*, *OPENMCL*, *LISPWORKS*, *CLISP* and *ECL*.
* support for send attachments, thanks Brian Sorg for the implementation
* authentication support for PLAIN and LOGIN authentication method
* ssl support with [cl+ssl](https://github.com/cl-plus-ssl/cl-plus-ssl) package
* uses [cl-base64](http://git.kpe.io/?p=cl-base64.git) and [usocket](https://github.com/usocket/usocket) packages (*cl-base64* isn't a requirement on ACL)
* automatically added headers: *date*, *x-mailer* (can be disabled)
* encode [RFC 2231](https://datatracker.ietf.org/doc/html/rfc2231) string

## Get Started

You can get *cl-smtp* from *Quicklisp*, *Ultralisp*, *Ocicl*.

You can load *cl-smtp* either via `(ql:quickload 'cl-smtp)` or
`(asdf:load-system 'cl-smtp)`.

## Usage

### send-email

```common-lisp
(send-email host from to subject message
            &key ssl (port (if (eq :tls ssl) 465 25))
            cc bcc reply-to extra-headers
            html-message display-name authentication
            attachments (buffer-size 256) envelope-sender
            (external-format :utf-8) local-hostname)
```

Send email.

Returns nil or error with message

arguments:
* `host` (String) : hostname or IP address of the SMTP server
* `from` (String) : email address
* `to` (String or List of Strings): email address
* `subject` (String) : subject text
* `message` (String) : message body
 keywords:
* `ssl` (or t :starttls :tls) : if t or :STARTTLS: use the STARTTLS functionality
                                if :TLS: use TLS directly
* `port`                           : TCP port number of the SMTP server
* `cc` (String or List of Strings) : email adress carbon copy
* `bcc` (String or List of Strings) : email adress blind carbon copy
* `reply-to` (String) : email adress
* `extra-headers` (List) : extra headers as alist
* `html-message` (String) : message body formatted with HTML tags
* `display-name` (String) : displayname of the sender
* `authentication` (List) : list with 2 or elements
                                     ([:method] "username" "password")
                                     method is a keyword :plain or :login
                                     If the method is not specified, the
                                     proper method is determined automatically.
* `attachments`(Attachment Instance or String or Pathname:
               attachments to send, List of Attachment/String/Pathnames)
* `envelope-sender` (String): email adress,
                              if not set then envelope-sender = from
* `external-format` (Symbol): default :utf-8
* `local-hostname` (String) : override hostname sent in SMTP HELO command
* `message-id` (String): set custom message-id, by default a uuidv7 plus sender domain are used.

### rfc2045-q-encode-string

```common-lisp
(rfc2045-q-encode-string str &key (external-format :utf8))
```
Decodes a string to an quoted-printable string.

Returns quoted-printable string

arguments:
* `str`: the string to encode
keywords:
* `external-format` : symbol, default :utf-8

### rfc2231-encode-string

```common-lisp
(rfc2231-encode-string str &key (external-format :utf8))
```
Decodes a string to an [RFC 2231](https://datatracker.ietf.org/doc/html/rfc2231) encode string.

Returns RFC 2231 encode string

arguments:
* `str` (String): the string to encode
keywords:
* `external-format` (Symbol): default :utf-8

### write-rfc5322-message

```common-lisp
(write-rfc5322-message stream from to subject message
                       &key cc reply-to extra-headers
                       html-message display-name
                       attachments buffer-size
                       external-format)
```
Writes a [RFC 5322](https://datatracker.ietf.org/doc/html/rfc5322) compatible email to the stream.

For arguments see the `cl-smtp:send-email` documentation.

### attachment (class)

* accessor: attachment-name          : string
* accessor: attachment-data-pathname : pathname
* accessor: attachment-mime-type     : string (mime-type)

It is now possible to send a file under a different name.
See `cl-smtp:make-attachment`.

### make-attachment

```common-lisp
(make-attachment data-pathname
			           &key (name (file-namestring data-pathname))
			           (mime-type (lookup-mime-type name)))
```

Create and returns instance of `cl-smtp:attachment`.

arguments:
* `data-pathname` : pathname
 keywords:
* `name` (string): default (file-namestring data-pathname)
* `mime-type` (string): default (lookup-mime-type name)

### \*x-mailer\*

*CL-SMTP* automatically sets the *Date* and *X-Mailer* header.
```
X-Mailer: cl-smtp ((lisp-implementation-type) (lisp-implementation-version))
```

You can change the contents of the *X-Mailer* header by binding the
`cl-smtp:*x-mailer*` variable to a string of your choice, e.g.:
```common-lisp
(setf cl-smtp:*x-mailer* "my x-mailer string")
```

You can also suppress the *x-mailer* header entirely by
binding `cl-smtp:*x-mailer*` variable to `nil`
```common-lisp
(setf cl-smtp:*x-mailer* nil)
```

## Debugging
For debug output set the parameter \*debug\* to `t` (default `nil`)
```common-lisp
(setf cl-smtp:*debug* t)
```

## Contact
If you find bugs or want to send patches for enhancements, by email to
Jan Idzikowski <jidzikowski@common-lisp.net>