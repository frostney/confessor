Colipas
=======

Command-line library for FreePascal/Delphi

Colipas is inspired by Commander (http://visionmedia.github.com/commander/) and Commander.js (http://tjholowaychuk.com/post/9103188408/commander-js-nodejs-command-line-interfaces-made-easy). Originally, I wanted to name the project Commander.pas, but because it's not a 1:1 port of the Commander interface, Colipas was the next name I came up with.

Sorry, if a few functions/procedures are a bit verbose. I've been doing a lot of Objective-C lately.


Features
--------

- Uses callbacks to actions  
- Utilize lists  
- Confirm messages (yes/no)  
- Prompts with additional default statement  
- Add options to your command-line apps as one or multiple char, one or more strings or both of those options  
- Options with parameter support  
- Supports LibNotify and Growl (through GrowlNotify) if available  


Usage
-----
  1. Include Colipas in your application  
  2. Sub-class TCommandApp  
  3. Implement the method Execute in your sub-class and make sure it calls the inherited method  
  4. Create an instance of your sub-class.  
  5. In your main code file add call the methods you need, for example:  
       MySubClass.Print(SomeString, [poLibNotify, poGrowl]); //< This send a notification to LibNotify or Growl if available  
       MySubClass.Option('s', 'someoption', @CallbackToMyOption); //< If the user would be call the application with the parameter  
         // -s or --someoption, CallbackToMyOption would be called, whereas CallbackMyOption should be implemented as a method of MySubClass  
  6. Call MySubClass.Execute  
  (You need to rename MySubClass to the actual class)  

Note:
  Some features are missing when using Colipas on Windows.  


See the examples for detailed information on usage.  

It is recommended to use Colipas with InstantFPC (http://wiki.lazarus.freepascal.org/InstantFPC).  


License
-------

MIT license, see LICENSE.txt for more information.  


Author
------

(C) Johannes Stein 2011  
http://www.freeze-dev.com  
johannesstein@freeze-dev.com  
