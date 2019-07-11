%%%-------------------------------------------------------------------
%% @doc chovya top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chovya_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, config_parser:get_config()).

init(Config) ->
    MySQLConnectionArgs = proplists:get_value(mysql_connnection_args, Config),
    MySQLPoolSup = {
        chovya_db_connection,
        {chovya, start_link, [MySQLConnectionArgs]},
        permanent,
        5000,
        supervisor,
        [chovya]
    },

    {ok, {
        #{strategy => one_for_all, intensity => 0, period => 1},
        [MySQLPoolSup]
    }}.

%% internal functions
