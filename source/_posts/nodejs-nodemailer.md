title: nodemailer
tags:
  - nodemailer
categories:
  - node.js
date: 2014-08-02 00:00:00
---

![nodemailer][1]

## 简介

`nodemailer` 是一个 `node.js` 的邮件客户端库, 常用语服务器端发送邮件.

项目地址:
http://www.nodemailer.com

`nodemailer` 支持的功能:

- `Unicode`
- HTML 内容, 纯文本内容
- 附件
- 在HTML中嵌入图片
- `SSL/STARTTLS`
- transport methods (传输方法)
- 自定义插件
- XOAUTH2

<!-- more -->


下面是一个发送邮件的例子:


```
var nodemailer = require('nodemailer');
// 创建可重用的transporter对象
var transporter = nodemailer.createTransport({
    service: 'Gmail',
    auth: {
        user: 'gmail.user@gmail.com',
        pass: 'userpass'
    }
});
var mailOptions = {
    from: 'Fred Foo ✔ <foo@blurdybloop.com>', // 发件人地址
    to: 'bar@blurdybloop.com, baz@blurdybloop.com', // 收件人地址列表
    subject: 'Hello ✔', // 邮件标题
    text: 'Hello world ✔', // 纯文本正文
    html: '<b>Hello world ✔</b>' // HTML正文
};
// 发送
transporter.sendMail(mailOptions, function(error, info){
    if(error){
        console.log(error);
    }else{
        console.log('Message sent: ' + info.response);
    }
});
```

## 配置

用`npm`安装

```
npm install nodemailer
```

发送邮件之前,需要创建一个`transporter`对象

```
var transporter = nodemailer.createTransport(transport);
```

这里的`transporter`是一个:

- 作为能够发送邮件的一个对象
- 一种传输机制, 如果不设置默认使用 [nodemailer-direct-transport][2], If it is a regular object nodemailer-smtp-transport is used and the value is passed as SMTP configuration.

> 传输对象只需要创建一次, 并可以在后续的邮件发送操作中重用.


## 例子

### 使用direct传输

这种方式是直接发送邮件到接受人的MX 服务器(使用25端口)

```
var nodemailer = require('nodemailer');
var transporter = nodemailer.createTransport();
transporter.sendMail({
    from: 'sender@address',
    to: 'receiver@address',
    subject: 'hello',
    text: 'hello world!'
});
```

> 使用 `direct` 传输机制不是一个可靠的方式, 因为`25`端口通常被防火墙屏蔽. 从一个`动态IP`发送的邮件通常被认为是垃圾邮件. 应该考虑使用`SMTP`.


### 使用默认的SMTP传输

[SMTP 配置选项][3]

```
var nodemailer = require('nodemailer');
var transporter = nodemailer.createTransport({
    service: 'gmail',
    auth: {
        user: 'sender@gmail.com',
        pass: 'password'
    }
});
transporter.sendMail({
    from: 'sender@address',
    to: 'receiver@address',
    subject: 'hello',
    text: 'hello world!'
});
```

> 默认的SMTP传输并不适合于大规模的邮件发送,每发送一个邮件需要建立一个新的SMTP连接会消耗大量的资源, 如果需要发送大量的邮件,考虑使用 [nodemailer-smtp-pool][4].

### 使用 transport 插件

## Available Transports

**Built in**

  * **[nodemailer-smtp-transport](https://github.com/andris9/nodemailer-smtp-transport)** for sending messages using a SMTP service
  * **[nodemailer-direct-transport](https://github.com/andris9/nodemailer-direct-transport)** for sending messages directly to recipients MX servers (zero configuration needed but unreliable)

**Install as dependencies**

  * **[nodemailer-smtp-pool](https://github.com/andris9/nodemailer-smtp-pool)** for sending messages to SMTP using pooled connections
  * **[nodemailer-ses-transport](https://github.com/andris9/nodemailer-ses-transport)** for sending messages to AWS SES
  * **[nodemailer-sendmail-transport](https://github.com/andris9/nodemailer-sendmail-transport)** for piping messages to the *sendmail* command
  * **[nodemailer-stub-transport](https://github.com/andris9/nodemailer-stub-transport)** is just for returning messages, most probably for testing purposes
  * **[nodemailer-pickup-transport](https://github.com/andris9/nodemailer-pickup-transport)** for storing messages to pickup folders
  * *add yours* (see transport api documentation [here](#transports))

## Available Plugins

  * **[nodemailer-markdown](https://github.com/andris9/nodemailer-markdown)** to use markdown for the content
  * **[nodemailer-dkim](https://github.com/andris9/nodemailer-dkim)** to sign messages with DKIM
  * **[nodemailer-html-to-text](https://github.com/andris9/nodemailer-html-to-text)** to auto generate plaintext content from html
  * *add yours* (see plugin api documentation [here](#plugin-api))

## Sending mail

Once you have a transporter object you can send mail

```javascript
transporter.sendMail(data, callback)
```

Where

  * **data** defines the mail content (see [e-mail message fields](#e-mail-message-fields) below)
  * **callback** is an optional callback function to run once the message is delivered or it failed.
    * **err** is the error object if message failed
    * **info** includes the result, the exact format depends on the transport mechanism used
      * **info.messageId** most transports *should* return the final Message-Id value used with this property
      * **info.envelope** includes the envelope object for the message
      * **info.accepted** is an array returend by SMTP transports (includes recipient addresses that were accepted by the server)
      * **info.rejected** is an array returend by SMTP transports (includes recipient addresses that were rejected by the server)
      * **info.pending** is an array returend by Direct SMTP transport. Includes recipient addresses that were temporarily rejected together with the server response
      * **response** is a string returned by SMTP transports and includes the last SMTP response from the server

> If the message includes several recipients then the message is considered sent if at least one recipient is accepted

### E-mail message fields

The following are the possible fields of an e-mail message:

  - **from** - The e-mail address of the sender. All e-mail addresses can be plain `'sender@server.com'` or formatted `'Sender Name <sender@server.com>'`, see [here](#address-formatting) for details
  - **to** - Comma separated list or an array of recipients e-mail addresses that will appear on the *To:* field
  - **cc** - Comma separated list or an array of recipients e-mail addresses that will appear on the *Cc:* field
  - **bcc** - Comma separated list or an array of recipients e-mail addresses that will appear on the *Bcc:* field
  - **replyTo** - An e-mail address that will appear on the *Reply-To:* field
  - **inReplyTo** - The message-id this message is replying
  - **references** - Message-id list (an array or space separated string)
  - **subject** - The subject of the e-mail
  - **text** - The plaintext version of the message as an Unicode string, Buffer, Stream or an object *{path: '...'}*
  - **html** - The HTML version of the message as an Unicode string, Buffer, Stream or an object *{path: '...'}*
  - **headers** - An object of additional header fields *{"X-Key-Name": "key value"}*
  - **attachments** - An array of attachment objects  (see [below](#attachments) for details)
  - **alternatives** - An array of alternative text contents (in addition to text and html parts)  (see [below](#alternatives) for details)
  - **envelope** - optional SMTP envelope, if auto generated envelope is not suitable (see [below](#smtp-envelope) for details)
  - **messageId** - optional Message-Id value, random value will be generated if not set
  - **date** - optional Date value, current UTC string will be used if not set
  - **encoding** - optional transfer encoding for the textual parts (defaults to 'quoted-printable')

All text fields (e-mail addresses, plaintext body, html body) use UTF-8 as the encoding.
Attachments are streamed as binary.

### 附件

附件对象由厦门的属性组成:

  * **filename** - filename to be reported as the name of the attached file, use of unicode is allowed
  * **cid** - optional content id for using inline images in HTML message source
  * **content** - String, Buffer or a Stream contents for the attachment
  * **path** - path to a file or an URL if you want to stream the file instead of including it (better for larger attachments)
  * **contentType** - optional content type for the attachment, if not set will be derived from the `filename` property
  * **contentDisposition** - optional content disposition type for the attachment, defaults to 'attachment'

附件可以添加任意多个.

```javascript
var mailOptions = {
    ...
    attachments: [
        {   // utf-8 string as an attachment
            filename: 'text1.txt',
            content: 'hello world!'
        },
        {   // binary buffer as an attachment
            filename: 'text2.txt',
            content: new Buffer('hello world!','utf-8')
        },
        {   // file on disk as an attachment
            filename: 'text3.txt',
            path: '/path/to/file.txt' // stream this file
        },
        {   // filename and content type is derived from path
            path: '/path/to/file.txt'
        },
        {   // stream as an attachment
            filename: 'text4.txt',
            content: fs.createReadStream('file.txt')
        },
        {   // define custom content type for the attachment
            filename: 'text.bin',
            content: 'hello world!',
            contentType: 'text/plain'
        },
        {   // use URL as an attachment
            filename: 'license.txt',
            path: 'https://raw.github.com/andris9/Nodemailer/master/LICENSE'
        }
    ]
}
```

### Alternatives

In addition to text and HTML, any kind of data can be inserted as an alternative content of the main body - for example a word processing document with the same text as in the HTML field. It is the job of the e-mail client to select and show the best fitting alternative to the reader. Usually this field is used for calendar events and such.

Alternative objects use the same options as [attachment objects](#attachments). The difference between an attachment and an alternative is the fact that attachments are placed into *multipart/mixed* or *multipart/related* parts of the message white alternatives are placed into *multipart/alternative* part.

**Usage example:**

```javascript
var mailOptions = {
    ...
    html: '<b>Hello world!</b>',
    alternatives: [
        {
            contentType: 'text/x-web-markdown',
            content: '**Hello world!**'
        }
    ]
}
```

Alternatives can be added as many as you want.

### 地址格式

所有的邮件地址格式可以是纯电子邮件地址

```
foobar@blurdybloop.com
```

也可以是带名称的格式(支持Unicode)

```
"Ноде Майлер" <foobar@blurdybloop.com>
```

还可以是一个地址对象

```
{
    name: 'Ноде Майлер',
    address: 'foobar@blurdybloop.com'
}
```

To, Cc and Bcc fields accept comma separated list of e-mails or an array of
e-mails or an array of comma separated list of e-mails - use it as you like.
Formatting can be mixed.

```
...,
to: 'foobar@blurdybloop.com, "Ноде Майлер" <bar@blurdybloop.com>, "Name, User" <baz@blurdybloop.com>',
cc: ['foobar@blurdybloop.com', '"Ноде Майлер" <bar@blurdybloop.com>, "Name, User" <baz@blurdybloop.com>']
...
```

You can even use unicode domains, these are automatically converted to punycode

```
'"Unicode Domain" <info@müriaad-polüteism.info>'
```

### SMTP 信封

SMTP envelope is usually auto generated from `from`, `to`, `cc` and `bcc` fields but
if for some reason you want to specify it yourself, you can do it with `envelope` property.

`envelope` is an object with the following params: `from`, `to`, `cc` and `bcc` just like
with regular mail options. You can also use the regular address format, unicode domains etc.

```javascript
mailOptions = {
    ...,
    from: 'mailer@kreata.ee',
    to: 'daemon@kreata.ee',
    envelope: {
        from: 'Daemon <deamon@kreata.ee>',
        to: 'mailer@kreata.ee, Mailer <mailer2@kreata.ee>'
    }
}
```

> Not all transports can use the `envelope` object, for example SES ignores it and uses the data from the From:, To: etc. headers.

### 嵌入图片

Attachments can be used as embedded images in the HTML body. To use this feature, you need to set additional property of the attachment - `cid` (unique identifier of the file) which is a reference to the attachment file. The same `cid` value must be used as the image URL in HTML (using `cid:` as the URL protocol, see example below).

**NB!** the cid value should be as unique as possible!

```javascript
var mailOptions = {
    ...
    html: 'Embedded image: <img src="cid:unique@kreata.ee"/>',
    attachments: [{
        filename: 'image.png',
        path: '/path/to/file',
        cid: 'unique@kreata.ee' //same cid value as in the html img src
    }]
}
```

## 插件系统

There are 3 stages a plugin can hook to

  1. **'compile'** is the step where e-mail data is set but nothing has been done with it yet. At this step you can modify mail options, for example modify `html` content, add new headers etc. Example: [nodemailer-markdown](https://github.com/andris9/nodemailer-markdown) that allows you to use `markdown` source instead of `text` and `html`.
  2. **'stream'** is the step where message tree has been compiled and is ready to be streamed. At this step you can modify the generated MIME tree or add a transform stream that the generated raw e-mail will be piped through before passed to the transport object. Example: [nodemailer-dkim](https://github.com/andris9/nodemailer-dkim) that adds DKIM signature to the generated message.
  3. **Transport** step where the raw e-mail is streamed to destination. Example: [nodemailer-smtp-transport](https://github.com/andris9/nodemailer-smtp-transport) that streams the message to a SMTP server.

### 包含插件

'compile' and 'stream' plugins can be attached with `use(plugin)` method

```javascript
transporter.use(step, pluginFunc)
```

Where

  * **transporter** is a transport object created with `createTransport`
  * **step** is a string, either 'compile' or 'stream' thatd defines when the plugin should be hooked
  * **pluginFunc** is a function that takes two arguments: the mail object and a callback function

## 插件 API

All plugins (including transports) get two arguments, the mail object and a callback function.

Mail object that is passed to the plugin function as the first argument is an object with the following properties:

  * **data** is the mail data object that is passed to the `sendMail` method
  * **message** is the [BuildMail](https://github.com/andris9/buildmail) object of the message. This is available for the 'stream' step and for the transport but not for 'compile'.
  * **resolveContent** is a helper function for converting Nodemailer compatible stream objects into Strings or Buffers

### resolveContent()

If your plugin needs to get the full value of a param, for example the String value for the `html` content, you can use `resolveContent()` to convert Nodemailer
compatible content objects to Strings or Buffers.

```javascript
data.resolveContent(obj, key, callback)
```

Where

  * **obj** is an object that has a property you want to convert to a String or a Buffer
  * **key** is the name of the property you want to convert
  * **callback** is the callback function with (err, value) where `value` is either a String or Buffer, depending on the input

**Example**

```javascript
function plugin(mail, callback){
    // if mail.data.html is a file or an url, it is returned as a Buffer
    mail.resolveContent(mail.data, 'html', function(err, html){
        if(err){
            return callback(err);
        }
        console.log('HTML contents: %s', html.toString());
        callback();
    });
};
```

### 'compile'

Compile step plugins get only the `mail.data` object but not `mail.message` in the `mail` argument of the plugin function. If you need to access the `mail.message` as well use 'stream' step instead.

This is really straightforward, your plugin can modify the `mail.data` object at will and once everything is finished run the callback function. If the callback gets an error object as an argument, then the process is terminated and the error is returned to the `sendMail` callback.

**Example**

The following plugin checks if `text` value is set and if not converts `html` value to `text` by removing all html tags.

```javascript
transporter.use('compile', function(mail, callback){
    if(!mail.text && mail.html){
        mail.text = mail.html.replace(/<[^>]*>/g, ' ');
    }
    callback();
});
```

See [plugin-compile.js](examples/plugin-compile.js) for a working example.

### 'stream'

Streaming step is invoked once the message structure is built and ready to be streamed to the transport. Plugin function still gets `mail.data` but it is included just for the reference, modifying it should not change anything (unless the transport requires something from the `mail.data`, for example `mail.data.envelope`).

You can modify the `mail.message` object as you like, the message is not yet streaming anything (message starts streaming when the transport calls `mail.message.createReadStream()`).

In most cases you might be interested in the [message.transform()](https://github.com/andris9/buildmail#transform) method for applying transform streams to the raw message.

**Example**

下面的插件用空格替换消息中的所有制表符.

```javascript
var transformer = new (require('stream').Transform)();
transformer._transform = function(chunk, encoding, done) {
    // replace all tabs with spaces in the stream chunk
    for(var i = 0; i < chunk.length; i++){
        if(chunk[i] === 0x09){
            chunk[i] = 0x20;
        }
    }
    this.push(chunk);
    done();
};
transporter.use('stream', function(mail, callback){
    // apply output transformer to the raw message stream
    mail.message.transform(transformer);
    callback();
});
```

See [plugin-stream.js](examples/plugin-stream.js) for a working example.

Additionally you might be interested in the [message.getAddresses()](https://github.com/andris9/buildmail#getaddresses) method that returns the contents for all address fields as structured objects.

**例子**

The following plugin prints address information to console.

```javascript
transporter.use('stream', function(mail, callback){
    var addresses = mail.message.getAddresses();
    console.log('From: %s', JSON.stringify(addresses.from));
    console.log('To: %s', JSON.stringify(addresses.to));
    console.log('Cc: %s', JSON.stringify(addresses.cc));
    console.log('Bcc: %s', JSON.stringify(addresses.bcc));
    callback();
});
```

### Transports

`Transports` 是具有方法`send`和属性`name`,`version`的对象. 另外如果`transport` 对象是一个`Event Emitter`, `log` 事件将通过`nodemailer` 传输. `transport` 对象传递给 `nodemailer.createTransport(transport)`方法创建`transporter`对象

**`transport.name`**

`transport` 对象的名称. 例如 `SMTP`, `SES`等.

```javascript
transport.name = require('package.json').name;
```

**`transport.version`**

`transport` 模块的版本, 例如 `0.1.0`

```javascript
transport.version = require('package.json').version;
```

**`transport.send(mail, callback)`**

This is the method that actually sends out e-mails. The method is basically the same as 'stream' plugin functions. It gets two arguments: `mail` and a callback. To start streaming the message, create the stream with `mail.message.createReadStream()`

Callback function should return an `info` object as the second arugment. This info object should contain `messageId` value with the Message-Id header (without the surrounding &lt; &gt; brackets)

The following example pipes the raw stream to the console.

```javascript
transport.send = function(mail, callback){
    var input = mail.message.createReadStream();
    var messageId = (mail.message.getHeader('message-id') || '').replace(/[<>\s]/g, '');
    input.pipe(process.stdout);
    input.on('end', function() {
        callback(null, {
            messageId: messageId
        });
    });
};
```

**`transport.close(args*)`**

If your transport needs to be closed explicitly, you can implement a `close` method.

This is purely optional feature and only makes sense in special contexts (eg. closing a SMTP pool).

Once you have a transport object, you can create a mail transporter out of it.

```
var nodemailer = require('nodemailer');
var transport = require('some-transport-method');
var transporter = nodemailer.createTransport(transport);
transporter.sendMail({mail data});
```

See [minimal-transport.js](examples/minimal-transport.js) for a working example.

## Delivering Bulk Mail

Here are some tips how to handle bulk mail, for example if you need to send 10 million messages at once (originally published as a [blog post](http://www.andrisreinman.com/delivering-bulk-mail-with-nodemailer/)).

  1. **Use a dedicated SMTP provider.** Do not use services that offer SMTP as a sideline or for free (thats Gmail or the SMTP of your homepage hosting company) to send bulk mail – you'll hit all the hard limits immediatelly or get labelled as spam. Basically you get what you pay for and if you pay zero then your deliverability is near zero as well. E-mail might seem free but it is only free to a certain amount and that amount certainly does not include 10 million e-mails in a short period of time.
  2. **Use a dedicated queue manager,** for example [RabbitMQ](http://www.rabbitmq.com/) for queueing the e-mails. Nodemailer creates a callback function with related scopes etc. for every message so it might be hard on memory if you pile up the data for 10 million messages at once. Better to take the data from a queue when there's a free spot in the connection pool (previously sent message returns its callback).
  3. **Use [nodemailer-smtp-pool](https://github.com/andris9/nodemailer-smtp-pool) transport.** You do not want to have the overhead of creating a new connection and doing the SMTP handshake dance for every single e-mail. Pooled connections make it possible to bring this overhead to a minimum.
  4. **Set `maxMessages` option to `Infinity`** for the nodemailer-smtp-pool transport. Dedicated SMTP providers happily accept all your e-mails as long you are paying for these, so no need to disconnect in the middle if everything is going smoothly. The default value is 100 which means that once a connection is used to send 100 messages it is removed from the pool and a new connection is created.
  5. **Set `maxConnections` to whatever your system can handle.** There might be limits to this on the receiving side, so do not set it to `Infinity`, even 20 is probably much better than the default 5. A larger number means a larger amount of messages are sent in parallel.
  6. **Use file paths not URLs for attachments.** If you are reading the same file from the disk several million times, the contents for the file probably get cached somewhere between your app and the physical hard disk, so you get your files back quicker (assuming you send the same attachment to all recipients). There is nothing like this for URLs – every new message makes a fresh HTTP fetch to receive the file from the server.
  7. If the SMTP service accepts HTTP API as well you still might prefer SMTP and not the HTTP API as HTTP introduces additional overhead. You probably want to use HTTP over SMTP if the HTTP API is bulk aware – you send a message template and the list of 10 million recipients and the service compiles this information into e-mails itself, you can't beat this with SMTP.

  [1]: https://raw2.github.com/andris9/Nodemailer/master/assets/nm_logo_200x136.png
  [2]: https://github.com/andris9/nodemailer-direct-transport
  [3]: https://github.com/andris9/nodemailer-smtp-transport#usage
  [4]: https://github.com/andris9/nodemailer-smtp-pool