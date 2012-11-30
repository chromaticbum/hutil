-module(hless_srv).

-behaviour(gen_server).

%% API
-export([start_link/0,
        compile/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    less_vm}).

%%%===================================================================
%%% API
%%%===================================================================

compile(Contents) ->
  gen_server:call(?MODULE, {compile, Contents}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{less_vm = new_less_vm()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({compile, Less}, _From, State) ->
  #state{less_vm = LessVm} = State,
  Reply = do_compile(LessVm, Less),

  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_compile(LessVm, {file, File}) ->
  case file:read_file(File) of
    {ok, Contents} -> do_compile(LessVm, Contents);
    {error, Reason} -> {error, Reason}
  end;
do_compile(LessVm, LessIo) ->
  Less = iolist_to_binary(LessIo),
  case js:call(LessVm, <<"compile">>, [Less]) of
    {ok, {struct, Json}} ->
      case proplists:get_value(<<"success">>, Json) of
        undefined ->
          Reason = proplists:get_value(<<"error">>, Json),
          {error, Reason};
        Css ->
          {ok, Css}
      end;
    {error, Reason} -> {error, Reason}
  end.

new_less_vm() ->
  {ok, LessVm} = js_driver:new(),

  LessJs = filename:join(filename:absname(code:priv_dir(hutil)), "less.js"),
  js_driver:define_js(LessVm, <<"window = {};">>),
  js_driver:define_js(LessVm, {file, LessJs}),

  LessVm.
