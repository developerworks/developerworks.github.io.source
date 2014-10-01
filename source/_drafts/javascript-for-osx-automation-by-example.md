title: OSX Javascript自动化示例
categories:
  - OSX
tags:
  - javascript
  - automation
toc: false
date: 2014-10-01 01:40:40
---

> TODO::
> 待Yosemite正式发布,升级系统后,测试使用后再翻译.

[JavaScript for OS X Automation][1] 作为在即将发布的 `OSX 10.10 Yosemite`提供了对底层Objective-C框架的集成,现在你可以使用Javascript来开发以前用AppleScript才能开发的自动化工具.

本文以一个实际的示例来描述如何在OSX中用Javascript开发自动化工具

## 入门

进入`Applications` -> `Utilities`, 打开`ScriptEditor`, 如下图:

![Script Editor][4]

Previously, your only option for creating automated tasks was AppleScript, now you will see JavaScript has been added.

![Select JavaScript and let’s build something][5]

## Example: Automating an Email Message

If you have automated tasks in the past, then surely you have interacted with the Mail app. Below is a snippet written in `AppleScript`.

```
tell application "Mail"
    set theMessage to make new outgoing message with properties
     {visible:true, subject:"Thanks for buying from us!", content:"My Body"}
end tell
```

```
I’ve taken the same snippet and appended slightly more complex content, but wrote it entire using JavaScript.

Mail = Application('Mail');

content = "Hi Michael,\n\n"
      + "Hello, How are you! \n\nThanks for stopping by today!"
      + " We really appreciate your business \n\n"
      + "Sincerely,\n\n"
      + "Company Name";

msg = Mail.OutgoingMessage({
    subject: "Thanks for buying from us!",
    content: content,
    visible: true
});

Mail.outgoingMessages.push(msg);

Mail.activate();
```

A couple of things to note here:

- Semicolons are optional because of [Automatic Semicolon Insertion (ASI)][6].
- I instantiated a variable in the global scope; instead of using for example :
```
var Mail = Application(‘Mail);
```
- I can use either single or double quotes for hard coded values.

If this script is run, then the following email dialog will appear as shown in Figure 2

![Figure 2 : Email Message with Fields populated.][7]

### But How Do you Know What Properties are avaiable?

Good question, if you are inside of Script Editor then you can click on `Window` -> `Library` to see a list of available apps that you can interact with.
If we pick the Mail app then it will default to AppleScript, simply change that to JavaScript and you can see the properties that are available on the `OutgoingMessage`
 class that we just used in Figure 3.


![Figure 3 : Examining the OutgoingMessage class from the Documentation.][8]

## That’s Cool, but What About Other Applications?

The Notes app is another application that we can interact with, but Apple has put in security precautions to prevent malicious scripts.
We can interact with the Notes app with just a few lines of JavaScript.

```
Notes = Application('Notes');
Notes.activate();
// 延迟一秒
delay(1);
SystemEvents = Application('System Events');
Notes = SystemEvents.processes['Notes'];
// 点击
Notes.windows[0].splitterGroups[0].groups[1].groups[0].buttons[0].click();
```

If you run this application, then you will be greeted with the following dialog shown in Figure 5.

![Figure 5 : Security & Privacy Dialog.][3]

If you click on `Open System Preferences`, then you can grant this script access and the Notes app will open automatically.

I’m sure by now you are wondering how to run these scripts without being inside the `Script Editor` application.
You can do so by clicking on `File` -> `Export` then change the format to “Application” as shown in Figure 6.

![Figure 6 : From a script to an app!][9]

## Other Automation Tasks

You may not want to interact with an application, maybe you wish for your app to speak to the user. You can accomplish this with just a few lines of JavaScript

```
App = Application.currentApplication();
App.includeStandardAdditions = true;
App.say("Hello from Telerik Headquarters");
```

This will use the default voice and language installed on your system to speak the words, “Hello from Telerik Headquarters”.

You can also use it to capture input such as a Name for example:

```
App = Application.currentApplication();
App.includeStandardAdditions = true;
answer = App.displayDialog('Please enter your Name', {
  withTitle: 'Name',
  defaultAnswer: 'Telerik'
});
```

This will display a prompt (Figure 7) and you can see in the Results window what they actually typed (Figure 8). In this case, I typed my name, “Michael”.

![Figure 7 : Dialog Message from JavaScript Automation][10]

![Figure 8 : Results Window][11]

## JavaScript Automation can also be Accessed through the Terminal

Surprisingly enough, JavaScript Automation can also be helpful for those that use bash to automate tasks.
Here is an example command line that opens Safari and a new tab and navigates to http://www.telerik.com by using [osascript][12] interactive mode.
The rest is plain JavaScript that we talked about before.

```
osascript -l JavaScript -i
Safari = Application("Safari");
window = Safari.windows[0];
window.name();
tab = Safari.Tab({url:"http://www.telerik.com"});
window.tabs.push(tab);
window.currentTab = tab;
```

The below screenshot shows what it looks like in the terminal window in Figure 9

![Figure 9][13]

Safari will open and generate a new tab as shown below:

![New Tab][14]

Look back through the terminal window and notice that when I called `window.name()` it returned "Top Sites".
This could be useful for understanding what page a user is on.

## There’s an Objective-C Bridge to Advantage Of

This is useful if you wish to use frameworks not present in the Foundation framework used by default. You can take advantage of frameworks like Cocoa by implementing the following code.

```
ObjC.import('Cocoa');
str = $.NSString.alloc.initWithUTF8String('Writing text to file through obj-c bridge.');
str.writeToFileAtomically('/Users/MichaelCrump/FromObjCBridge.txt', true);
```

This code imports the Cocoa Framework, initializes an `NSString` with text and calls the `writeToFileAtomically` method passing in the file location and setting the second pararameter to true. This will ensure that the old file will not be changed or removed until the new version is completely on the disk.

We can navigate to our folder and open the text file and see the expected results in Figure 10

![Figure 10 : Obj-C Bridge Writing a File to Disk][15]

## Even more Opportunities for JavaScript Developers

We are learning more and more about the implications of JavaScript becoming a first-class citizen in Yosemite. It is also front and center for [NativeScript][16].
NativeScript enables developers to use JavaScript to build native apps leveraging readily available or custom device APIs,
such as those for camera, accelerometer, geolocation and more. There is a lot to talk about with NativeScript,
but I’ll point you to the [FAQ][17] if you want to learn more.

> NativeScript allows you to build native applications for iOS, Android and Windows Universal using JavaScript.


## Bright Future Ahead

I’m very excited about what I’ve seen so far with JavaScript for OS X Automation. Prior to writing this article, I’d never used AppleScript before, but had written JavaScript. I was able to use the syntax familiar to me and successfully write automation tasks. JavaScript developers all over the world should rejoice even if they don’t plan on writing automation tasks because it shows that skillset is becoming more and more important.


  [1]: https://developer.apple.com/library/prerelease/mac/releasenotes/InterapplicationCommunication/RN-JavaScriptForAutomation/index.html
  [2]: http://developer.telerik.com/featured/javascript-os-x-automation-example
  [3]: /assets/images/javascript-automation-for-osx/opennotesdemo.png
  [4]: /assets/images/javascript-automation-for-osx/scripteditor.png
  [5]: /assets/images/javascript-automation-for-osx/javascriptoption.gif
  [6]: http://bclary.com/2004/11/07/#a-7.9.1
  [7]: /assets/images/javascript-automation-for-osx/mailmessage.png
  [8]: /assets/images/javascript-automation-for-osx/apidocs.png
  [9]: /assets/images/javascript-automation-for-osx/exporttoapp.png
  [10]: /assets/images/javascript-automation-for-osx/promptuser.png
  [11]: /assets/images/javascript-automation-for-osx/textreturned.png
  [12]: https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man1/osascript.1.html
  [13]: /assets/images/javascript-automation-for-osx/interactivemode1.png
  [14]: /assets/images/javascript-automation-for-osx/browser.png
  [15]: /assets/images/javascript-automation-for-osx/savefiletodisk.png
  [16]: http://developer.telerik.com/featured/nativescript-a-technical-overview/
  [17]: http://www.telerik.com/nativescript/faq
