-module(chat_client).
-export([join/1]).
-import(base,[println/1,getln/1,show/1]).


join(Node) ->
  println("Please choose a name."),
  Name = getln(">"),
  join(Node, Name).


join(Node, Name) ->
  {chat_server,Node} ! {login,self(),Name},
  receive
    {logged_in,Others,Server} -> printUsers(Others),
                                 Me = self(),
                                 spawn(fun() -> user_input(Name,Server,Me)
                                       end),
                                 loop();
    {name_in_use,_}      -> println("That name is already taken. Please choose a different name."),
                                 NewName = getln(">"),
                                 join(Node, NewName)
  after 2000 -> println("Server not reachable")
  end.

printUsers([]) -> println("There is no one else in the chat room right now.");
printUsers([X]) -> io:put_chars(color:blue(X)++" is here.\n");
printUsers([X|Xs]) -> io:put_chars(color:blue(X)++" is here.\n"),
                      printUsers(Xs).

loop() ->
  receive
    {message,Name,Msg} -> io:put_chars(color:blue(Name)++": "++Msg++"\n"),
                          loop();
    {new_login,Name}   -> io:put_chars(color:blue(Name)++" joined the chat.\n"),
                          loop();
    {new_logout,Name}  -> io:put_chars(color:blue(Name)++" left the chat.\n"),
                          loop();
    {whoshere,Names}   -> io:put_chars(color:green("------CHATROOM PARTICIPANTS------\n")),
                          println("You are here."),
                          printUsers(Names),
                          io:put_chars(color:green("---------------------------------\n")),
                          loop();
    {whispersent}      -> println("Message sent!"),
                          io:put_chars(color:green("----------------------------------\n")),
                          loop();
    {whisper, name_not_found} -> println("The user you tried to message could not be found."),
                                 io:put_chars(color:green("----------------------\n")),
                                 loop();
    {whisper_received, Msg, FromName} -> io:put_chars(color:green("------PRIVATE MESSAGE RECEIVED------\n")),
                                                  io:put_chars(color:cyan(FromName)++": "++Msg++"\n"),
                                                  io:put_chars(color:green("------------------------------------\n")),
                                                  loop();
    logout             -> ok
  end.

user_input(Name,Server,CPid) ->
  Str = string:trim(io:get_line(color:red(Name)++"> ")),
  case Str of
    ".bye"        -> Server ! {logout,CPid},
                     CPid ! logout;
    ".whosthere"  -> Server ! {whosthere,CPid},
                     user_input(Name,Server,CPid);
    ".pm"         -> io:put_chars(color:green("------SEND A PRIVATE MESSAGE------\n")),
                     println("Please enter the username you want to send a message to."),
                     WU = string:trim(io:get_line("> ")),
                     println("Please enter the message."),
                     Msg = string:trim(io:get_line("> ")),
                     if WU == Name -> println("You cannot send a message to yourself."),
                                                   io:put_chars(color:green("----------------------------------\n")),
                                                   user_input(Name, Server,CPid);
                          true                  -> Server ! {whisper, Msg, WU, CPid},
                                                   user_input(Name, Server, CPid)
                     end;
    ".help"      -> io:put_chars(color:green("------LIST OF COMMANDS------\n")),
                    io:put_chars(color:purple(".bye")++"        to logout\n"),
                    io:put_chars(color:purple(".whosthere")++"  to see list of chatroom participants\n"),
                    io:put_chars(color:purple(".pm")++"         to private message someone\n"),
                    io:put_chars(color:purple(".help")++"       to see list of commands\n"),
                    io:put_chars(color:green("----------------------------\n")),
                    user_input(Name,Server,CPid);
    "."++_       -> io:put_chars(color:green("Command not recognized.")),
                    user_input(Name,Server,CPid);
    _               -> Server!{message,Str,CPid},
                       user_input(Name,Server,CPid)
  end.

