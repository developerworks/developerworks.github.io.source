title: XMPP客户端 Strophe.js
categories:
  - Communication
tags:
  - strophejs
toc: false
date: 2014-10-17 17:20:05
---

## addHandler

```
addHandler: function (	handler,
                        ns,
                        name,
                        type,
                        id,
                        from,
                        options	)
```
Add a stanza handler for the connection.

This function adds a stanza handler to the connection.  The handler callback will be called for any stanza that matches the parameters.  Note that if multiple parameters are supplied, they must all match for the handler to be invoked.

The handler will receive the stanza that triggered it as its argument.
**The handler should return true if it is to be invoked again; returning false will remove the handler after it returns.**

As a convenience, **the ns parameters applies to the top level element** and also any of its immediate children.  This is primarily to make matching /iq/query elements easy.

The options argument contains handler matching flags that affect how matches are determined.  Currently the only flag is matchBare (a boolean).  When matchBare is true, the from parameter and the from attribute on the stanza will be matched as bare JIDs instead of full JIDs.  To use this, pass {matchBare: true} as the value of options.  The default value for matchBare is false.

The return value should be saved if you wish to remove the handler with deleteHandler().

## Parameters

(Function) handler	The user callback.
(String) ns	The namespace to match.
(String) name	The stanza name to match.
(String) type	The stanza type attribute to match.
(String) id	The stanza id attribute to match.
(String) from	The stanza from attribute to match.
(String) options	The handler options

## Returns

A reference to the handler that can be used to remove it.