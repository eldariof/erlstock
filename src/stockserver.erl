-module(stockserver).
-author('Eldar Abdrazakov').

-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0, get_count/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
-export([
         add_stock/3, retrieve_stock/1, stock_list/0]).

-record(state, {}).
-record(stock, {stockid, name, price, count, timestamp}).
         
-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8081).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(init) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [init], []).
stop() ->
  gen_server:cast(stockserver, stop).
get_count() ->
  gen_server:call(?SERVER, get_count).   
  
init([]) ->
  stockserver_start(),
  {ok, #state{}};
init([init]) ->
  stockserver_start(init),
  {ok, #state{}}.
    
stockserver_start(init) ->
  deinit(),
  mnesia_init_env(),
  init_schema(),
  yaws_start().
    
stockserver_start() ->
  mnesia_init_env(),
  mnesia:start(),
  yaws_start().
    
deinit() ->
  mnesia:delete_schema([node()]).
    
mnesia_init_env() ->
  application:set_env(mnesia, dump_log_write_threshold, 60000),
  application:set_env(mnesia, dc_dump_limit, 40).
    
init_schema() ->
  mnesia:create_schema([node()]),
  mnesia:change_config(extra_db_nodes, [node()]),
  mnesia:start(),
  case init_tables() of
       {error, Reason} ->
           io:format("Could not create tables. Reason: ~w", [Reason]);
       success ->
           io:format("Tables are created.", [])
  end.
    
init_tables() ->
  case mnesia:create_table(stock,
                         [{disc_copies, [node()]},
                          {attributes, record_info(fields,stock)}])
  of {atomic, ok} ->	
	success;                
  {aborted, Reason} ->
        {error, Reason}
  end.
    
yaws_start() ->
  code:add_path("./deps/yaws/ebin"),
  Docroot = "www",
  Logdir = "log",
  Tmpdir = "tmp",
  yaws:start_embedded(Docroot,[{port,?DEFAULT_PORT},
                               {servername,"localhost"},
                               {dir_listings, true},
                               {listen,{0,0,0,0}},
                               {flags,[{auth_log,false},{access_log,false}]}],
                      [{enable_soap,false},   % <== THIS WILL ENABLE SOAP IN A YAWS SERVER!!
                       {trace, false},
                       {tmpdir,Tmpdir},{logdir,Logdir},
                       {flags,[{tty_trace, false}, {copy_errlog, true}]}]).
    
handle_call({add_stock, Name, Price, Count}, _From, State) ->
  Reply = ss_add_stock(Name, Price, Count),
  {reply, Reply, State};
handle_call({retrieve_stock, Name}, _From, State) ->
  Reply = ss_retrieve_stock(Name),
  {reply, Reply, State};
handle_call({stock_list}, _From, State) ->
  Reply = ss_stock_list(),
  {reply, Reply, State};
handle_call({stop}, _From, State) ->
  Reply = ss_stop(),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

add_stock(Name, Price, Count) ->
  gen_server:call(?SERVER, {add_stock, Name, Price, Count}).
  
retrieve_stock(Name) ->
  gen_server:call(?SERVER, {retrieve_stock, Name}).
  
stock_list() ->
  gen_server:call(?SERVER, {stock_list}).
  
ss_add_stock(Name, Price, Count) ->
  Timestamp = calendar:local_time(),
  Stock = #stock{stockid=uuid:uuid4(), name=Name, price=Price, count=Count, timestamp=Timestamp},
  Fun = fun() ->
    mnesia:write(Stock) 
  end,
  mnesia:transaction(Fun).
  
ss_retrieve_stock(Name) ->
  Fun = fun() ->
          mnesia:read({stock, Name})
  end,
  mnesia:transaction(Fun).
    
ss_stock_list() ->
  Fun = fun() ->
  	    MatchHead = #stock{stockid='$1', _ = '_'},
            Guards = [],
            Results = ['$_'],
            MatchSpec = [{MatchHead, Guards, Results}],
            mnesia:select(stock, MatchSpec, read)
  end,
  case mnesia:transaction(Fun) of
     {atomic, Result} -> Result;
     {aborted, Reason} -> Reason
  end.
  
ss_stop() ->
  yaws:stop(),
  mnesia:stop().
  
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.
    
handle_info(_Info, State) ->
  {noreply, State}.
  
terminate(_Reason, _State) ->
  ss_stop(),
  ok.
    
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.