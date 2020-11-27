# chat
A very small command-line based chatting program in Erlang. This was part of an assignment at CAU Kiel. Additionally, `color.erl` is made by eproxus (link: [https://github.com/eproxus/color](https://github.com/eproxus/color)).

## Usage
You will need to download `chat_client.erl`, `color.erl`, and `base.erl`. (You will also need to install Erlang if you do not already have Erlang on your computer, see [https://www.erlang.org/downloads](https://www.erlang.org/downloads).) You will also have to compile all three files in order to run them.

### To start your own server:
Start a node using the command:
```
erl -sname <server_name>
```

Start the server:
```
chat_server:start().
```

### To join as a client:

Start your own node using the command:
```
erl -sname <name>
```

Then connect to the server:
```
chat_client.join(<server_name>).
```

After that, you should be in the chat, so long as the server you are connecting to is valid and running!

### Commands
To logout:
```
.bye
```

To private message someone in the chat:
```
.pm
```

To see the list of commands:
```
.help
```

To see the list of participants in the chat:
```
.whosthere
```

