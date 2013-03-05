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
%% meta data
-export([defs/0]).
%% containers
-export([g/0, g/1]).
%% shapes
-export([rect/1, rect/2, circle/1, circle/2, ellipse/2, path/1, path/2, text/3, text/2, span/1, span/2]).
%% clipping
-export([clip_path/1, clip_path/2]).
%% transform
-export([transform/2, translate/2, scale/1]).


%%
%%
-define(XML, "<?xml version=\"1.0\" standalone=\"no\"?>\n").
-define(DOCTYPE, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
	\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n").
-define(VSN,   "1.1").
-define(SVG, "http://www.w3.org/2000/svg").

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
   {svg, [
      {xmlns,   ?SVG}, 
      {version, ?VSN}, 
      {width,   Width}, 
      {height,  Height},
      {viewBox, lists:flatten(io_lib:format("0 0 ~b ~b", [Width, Height]))}
   ], []}.


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
%%% control
%%%
%%%------------------------------------------------------------------

%%
%% defs() -> Element
%%
%% 
-spec defs() -> element().

defs() ->
   {defs, [], []}.

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

g() ->
   g([]).

g(Attr) ->
   {g, Attr, []}.

%view({X,Y}, {W,H}) ->
%   {view, [{'viewBox', [coord(X), $ , coord(Y), $ , coord(W), $ , coord(H)]}], []}.   


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

rect(Rect) ->
   rect(Rect, []).   

rect({{X,Y}, {W,H}}, Attr) ->
   {rect, [{x, coord(X)}, {y, coord(Y)}, {width, coord(W)}, {height, coord(H)} | Attr], []}.   

%%
%% circle(Point, R) -> Element
%%
%% defines a circle based on center point and radius
-spec circle(point(), integer()) -> element().

circle(Circle) ->
   circle(Circle, []).

circle({{X,Y}, R}, Attr) ->
   {circle, [{cx, coord(X)}, {cy, coord(Y)}, {r, coord(R)} | Attr], []}.

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

%%
%%
path(Path) ->
   path(Path, []).

path([{X0, Y0} | Tail], Attr) ->
   Ptail = lists:map(fun p4p/1, Tail),
   Path  = [ [$M, 32, coord(X0), 32, coord(Y0)] | Ptail ],
   {path, [{d, lists:flatten(Path)} | Attr], []}.

%%
%% text(Point, Text) -> Element
%%
%% defines a text
text(Point, Text) ->
   text(Point, Text, []).

-spec text(point(), list()) -> element().
text({X,Y}, Text, Attr) ->
   {text, [{x, coord(X)}, {y, coord(Y)} | Attr], [Text]}.

span(Text) ->
   span(Text, []).

span(Text, Attr) ->
   {tspan, Attr, [Text]}.

%%%------------------------------------------------------------------
%%%
%%% clipping
%%%
%%%------------------------------------------------------------------

%%
%%
-spec clip_path(atom() | list()) -> element().

clip_path(Id) ->
   clip_path(Id, []).

-spec clip_path(atom() | list(), attributes()) -> element().

clip_path(Id, Attr) ->
   {'clipPath', [{id, Id} | Attr], []}.

%%%------------------------------------------------------------------
%%%
%%% transform
%%%
%%%------------------------------------------------------------------


%%
%% transform path to bounding box
transform(Path, {{X,Y},{W,H}}) ->
   {{Xmin, Xmax}, {Ymin, Ymax}} = minmax(Path),
   Xscale = case (Xmax - Xmin) / W of
      0.0 -> Xmax / W;
      XS  -> XS
   end,
   Yscale = case (Ymax - Ymin) / H of
      0.0 -> Ymax / W;
      YS  -> YS
   end,
   lists:map(
      fun({Xp, Yp}) ->
         {X + (Xp - Xmin) / Xscale, H + Y - (Yp - Ymin) / Yscale}
      end,
      Path
   ).



%%
%%
translate(X, Y) ->
   [" translate", $(, coord(X), 32, coord(Y), $), $ ].

scale(X) ->
   [" scale", $(, coord(X), $), $ ].


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% point for path - convert coordinate 
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

%%
%% 
minmax([{X0, Y0} | _] = Path) ->
   X = {X0, erlang:element(1, lists:last(Path))},
   Y = lists:foldl(
      fun({_, Y}, {Min0, Max0}) ->
         {erlang:min(Min0, Y), erlang:max(Max0, Y)}
      end,
      {Y0, Y0},
      Path
   ),
   {X, Y}.

