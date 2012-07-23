%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%%   @description
%%      erlang SVG library   
-module(svg).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

%% primitives
-export([init/2, set/3, add/2, export/1, write/2]).
%% containers
-export([g/1]).
%% shapes
-export([rect/2, circle/2, ellipse/2, path/1, text/2]).

%%
%%
-define(XML, "<?xml version=\"1.0\" standalone=\"no\"?>\n").
-define(DOCTYPE, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
	\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n").
-define(VSN, "1.1").

%%
%%
-type attributes() :: [{atom(), atom() | list()}].
-type element()    :: {atom(), attributes(), list()}.
-type point()      :: {integer(), integer()}.
-type curve()      :: {point(), point(), point()} | 
                      {point(), point()}.
-type size()       :: {integer(), integer()}.

%%%------------------------------------------------------------------
%%%
%%% primitives 
%%%
%%%------------------------------------------------------------------

%%
%% init(Width, Height) -> Svg
%%    Width  = integer()
%%    Height = integer()
%%
%% create empty svg document
-spec init(integer(), integer()) -> element().

init(Width, Height) ->
   {svg, [{version, ?VSN}, {width, Width}, {height, Height}], []}.


%%
%% set(Attr, Val, Element0) -> Element
%% set(List, Element0)      -> Element
%%    Attr = atom()
%%    Val  = atom() | list()
%%
%% set attribute of element
-spec set(atom(), atom() | list(), element()) -> element().

set(Attr, Val, {Tag, Attrs, Inner}) ->
   {Tag, set(Attr, Val, Attrs), Inner}; 

set(Attr, Val, List) when is_list(List) ->
   case lists:keytake(Attr, 1, List) of
      false         -> [{Attr, Val} | List];
      {value, _, A} -> [{Attr, Val} | A]
   end.


%%
%% add(Inner, Element0) -> Element
%%    Inner   = element() | [element()] 
%%    Element = element()
%%
%% add inner element(s) to container
-spec add(element(), element()) -> element().

add(E, {Tag, Attrs, Inner}) when is_tuple(E) ->
   {Tag, Attrs, Inner ++ [E]};
add(E, {Tag, Attrs, Inner}) when is_list(E) ->
   {Tag, Attrs, Inner ++ E}.

%%
%% export(Svg) -> IOList
%%    Svg = element()
%%
%% export svg to iolist
-spec export(element()) -> iolist().

export({svg, Attr, Svg}) ->
   xmerl:export_simple(
      [{svg, Attr, Svg}], 
      xmerl_xml, 
      [{prolog, ?XML ++ ?DOCTYPE}]
   ).

%%
%% write(Filename, Svg) -> ok | {error, ...}
%%
%% writes SVG document to file
-spec write(list(), element()) -> atom() | {error, any()}.

write(Filename, Svg) ->
   file:write_file(
      Filename,
      list_to_binary(
         lists:flatten(
            export(Svg)
         )
      )
   ).

%%%------------------------------------------------------------------
%%%
%%% container
%%%
%%%------------------------------------------------------------------

%%
%% g(Attr) -> Element
%%    Attr = attributes() list of attributes
%%
%% create grouping element
-spec g(attributes()) -> element().

g(Attr) ->
   {g, Attr, []}.

%%%------------------------------------------------------------------
%%%
%%% shapes
%%%
%%%------------------------------------------------------------------

%%
%% rect(Point, Size) -> Element
%%   
%% defines rectangle base of top-left corner poins and width, height
-spec rect(point(), size()) -> element().

rect({X,Y}, {W,H}) ->
   {rect, [{x, coord(X)}, {y, coord(Y)}, {width, coord(W)}, {height, coord(H)}], []}.   

%%
%% circle(Point, R) -> Element
%%
%% defines a circle based on center point and radius
-spec circle(point(), integer()) -> element().

circle({X,Y}, R) ->
   {circle, [{cx, coord(X)}, {cy, coord(Y)}, {r, coord(R)}], []}.

%%
%% ellipse(Point, {Rx, Ry}) -> Element
%%
%% defines an ellipse based on center point and two radii. 
-spec ellipse(point(), {integer(), integer()}) -> element().

ellipse({X,Y}, {Rx, Ry}) ->
   {ellipse, [{cx, coord(X)}, {cy, coord(Y)}, {rx, coord(Rx)}, {ry, coord(Ry)}], []}.

%%
%% path(Path) -> element()
%%    Path = [point() | curve()] list of point, curve 
%%
%% defines a path
-spec path([point() | curve()]) -> element().

path([{X0, Y0} | Tail]) ->
   Ptail = lists:map(fun p4p/1, Tail),
   Path  = [ [$M, 32, coord(X0), 32, coord(Y0)] | Ptail ],
   {path, [{d, lists:flatten(Path)}], []}.

%%
%% text(Point, Text) -> Element
%%
%% defines a text
-spec text(point(), list()) -> element().
text({X,Y}, Text) ->
   {text, [{x, coord(X)}, {y, coord(Y)}], [Text]}.



%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% point for path
p4p({{Cx1, Cy1}, {Cx2, Cy2}, {X, Y}}) ->
   % absolute curve-to
   [32, $C, 32, coord(Cx1), 32, coord(Cy1), 32, coord(Cx2), 32, coord(Cy2), 32, coord(X), 32, coord(Y)];
p4p({{Cx2, Cy2}, {X, Y}}) ->
   % absolute curve-to (first control point is second)
   [32, $S, 32, coord(Cx2), 32, coord(Cy2), 32, coord(X), 32, coord(Y)];
p4p({X, Y}) ->
   % absolute line-to
   [32, $L, 32, coord(X), 32, coord(Y)];
p4p('Z') ->
   % close path
   $Z.


%%
%% numerical coordinate value to list
coord(X) when is_integer(X) ->
   integer_to_list(X);
coord(X) when is_float(X) ->
   io_lib:format("~.1f", [X]).





