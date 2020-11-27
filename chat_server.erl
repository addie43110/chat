-module(chat_server).
-export([start/0]).
-import(base,[println/1,getln/1,show/1]).

start() -> Server_pid = spawn(fun() -> process_flag(trap_exit,true),
                                       server([]) end),
           register(chat_server,Server_pid),
           Server_pid.

server(Clients) ->
  println(Clients),
  receive
    {login,CPid,Name} -> case lookupName(Name, Clients) of
                            nothing   -> println("login: "++Name),  
                                         link(CPid),                         
			                                   broadcast({new_login,Name},Clients,none),
                                         CPid ! {logged_in,names(Clients),self()},
                                         server([{Name,CPid}|Clients]);
                            {just,_}  -> println("name already in use: "++Name),
                                         CPid ! {name_in_use, self()},
                                         server(Clients)
                         end;
    {message,Msg,CPid} -> println("message: "++Msg),
                          case lookup2(CPid,Clients) of
                            nothing -> println("Unknown Client"),
                                       server(Clients);
                            {just,Name} ->
  		               broadcast({message,Name,Msg},Clients,CPid),
                               server(Clients)
                          end;
    {logout,CPid} -> case lookup2(CPid,Clients) of    
                       nothing -> println("Unknown Client"),
                                  server(Clients); 
                       {just,Name} ->
                         println("logout: "++Name),
                         unlink(CPid),
                         broadcast({new_logout,Name},Clients,CPid),
                         server(lists:delete({Name,CPid},Clients))
                     end;
    {whosthere,CPid} -> case lookup2(CPid, Clients) of 
                            nothing       -> println("Unknown client"),
                                             server(Clients);
                            {just, Name}  -> CPid!{whoshere, lists:delete(Name, names(Clients))},
                                             server(Clients)
                        end;
    {whisper, Msg, ToName, FromPid} -> case lookup2(FromPid, Clients) of
                                        nothing -> println("Unknown client"),
                                                   server(Clients);
                                        {just,FromName} -> case lookupName(ToName, Clients) of
                                                            nothing -> FromPid ! {whisper, name_not_found},
                                                                       server(Clients);
                                                            {just, ToPid} -> FromPid ! {whispersent},
                                                                             ToPid ! {whisper_received, Msg, FromName},
                                                                             server(Clients)
                                                           end
                                       end;
    {'EXIT',CPid,_} -> self() ! {logout,CPid},
                       server(Clients);
    MSG -> println(MSG),
           server(Clients)
  end.

broadcast(_,[],_) -> ok;
broadcast(M,[{_,CPid}|Clients],CPid)  -> broadcast(M,Clients,CPid);
broadcast(M,[{_,CPid1}|Clients],CPid) -> CPid1 ! M,
                                         broadcast(M,Clients,CPid).

names(Clients) -> lists:map(fun({N,_}) -> N end,Clients).

lookup2(_,[]) -> nothing;
lookup2(K,[{V,K}|_]) -> {just,V};
lookup2(K,[_|VKs]) -> lookup2(K,VKs).

lookupName(_,[]) -> nothing;
lookupName(N, [{N,P}|_]) -> {just, P};
lookupName(N, [_|VKs]) -> lookupName(N, VKs).
