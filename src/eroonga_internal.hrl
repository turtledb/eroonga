%% =============================================================================
%% Copyright 2013 AONO Tomohiko
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% =============================================================================

%% == ~/include/groonga.h ==

%% -- define --
-define(GRN_CTX_MORE,  (1 bsl 0)).
-define(GRN_CTX_TAIL,  (1 bsl 1)).
-define(GRN_CTX_HEAD,  (1 bsl 2)).
-define(GRN_CTX_QUIET, (1 bsl 3)).
-define(GRN_CTX_QUIT,  (1 bsl 4)).

%% -- enum: grn_content_type --
-define(GRN_CONTENT_NONE,    0).
-define(GRN_CONTENT_TSV,     1).
-define(GRN_CONTENT_JSON,    2).
-define(GRN_CONTENT_XML,     3).
-define(GRN_CONTENT_MSGPACK, 4).

%% == ~/lib/com.h ==

%% -- define --
-define(GRN_COM_PROTO_GQTP, "\xc7").


%% == other ==

-define(ISSET(B,P), (P =:= B band P)).

-type(filename() :: file:filename()).
-type(property() :: proplists:property()).

-type(socket() :: port()).                      % gen_tcp:socket()

-type startlink_ret() :: {ok,pid()}|ignore|{error,_}.
-type stop_ret() :: ok.

-type sup_name() :: {local,atom()}|{global,atom()}.
-type sup_ref() :: atom()|{atom(),node()}|{global,atom()}|pid().
