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
-module(svg_test).
-include_lib("eunit/include/eunit.hrl").

rect_test() ->
   ok = svg:write("rect.svg",
      svg:add(
   	   svg:add(
   	      svg:rect({{50,50}, {100,100}}),
   	      svg:g([
   	      	{fill,   '#666'}, 
   	      	{stroke, '#333'},
   	      	{'stroke-width', 3}
   	      ])
   	   ),
   	   svg:init(200, 200)
      )
   ).


circle_test() ->
   ok = svg:write("circle.svg",
      svg:add(
   	   svg:add(
   	      svg:circle({{100,100}, 50}),
   	      svg:g([
   	      	{fill,   '#666'}, 
   	      	{stroke, '#333'},
   	      	{'stroke-width', 3}
   	      ])
   	   ),
   	   svg:init(200, 200)
      )
   ).

ellipse_test() ->
   ok = svg:write("ellipse.svg",
      svg:add(
   	   svg:add(
   	      svg:ellipse({100,100}, {90,50}),
   	      svg:g([
   	      	{fill,   '#666'}, 
   	      	{stroke, '#333'},
   	      	{'stroke-width', 3}
   	      ])
   	   ),
   	   svg:init(200, 200)
      )
   ).

path_test() ->
   ok = svg:write("path.svg",
      svg:add(
   	   svg:add(
   	      svg:path([{0,50},{50,200},{100,0},{150,200},{200,0}]),
   	      svg:g([
   	      	{fill,   'none'}, 
   	      	{stroke, '#333'},
   	      	{'stroke-width', 3}
   	      ])
   	   ),
   	   svg:init(200, 200)
      )
   ).

closed_path_test() ->
   ok = svg:write("pathz.svg",
      svg:add(
   	   svg:add(
   	      svg:path([{0,50},{50,200},{100,0},{150,200},'Z']),
   	      svg:g([
   	      	{fill,   '#666'}, 
   	      	{stroke, '#333'},
   	      	{'stroke-width', 3}
   	      ])
   	   ),
   	   svg:init(200, 200)
      )
   ).   

curve_test() ->
   ok = svg:write("curve.svg",
      svg:add(
         svg:add(
            svg:path([
               {100,200},
               {{100,100},{250,100},{250,200}},
               {{400,300},{400,200}}
            ]),
            svg:g([
               {fill,   'none'}, 
               {stroke, '#333'},
               {'stroke-width', 3}
            ])
         ),
         svg:init(500, 400)
      )
   ).

text_test() ->
   ok = svg:write("text.svg",
      svg:add(
         svg:add(
            svg:text({20,100}, "Hello SVG!"),
            svg:g([
               {'font-family',   'Times'},
               {'font-size',     38},
               {fill,   '#666'}, 
               {stroke, '#333'},
               {'stroke-width', 1}
            ])
         ),
         svg:init(200, 200)
      )
   ).

horizontal_layout_test() ->
   ok = svg:write("horizontal.svg",
      svg:add(
         svg:add(
            [
               svg:rect({100,200}, [{fill, '#333'}]),
               svg:rect({100,200}, [{fill, '#666'}]),
               svg:rect({100,200}, [{fill, '#999'}])
            ],
            svg:layout(horizontal, [
               {stroke, '#eee'},
               {'stroke-width', 1}
            ])
         ),
         svg:init(300, 200)
      )
   ).

vertical_layout_test() ->
   ok = svg:write("vertical.svg",
      svg:add(
         svg:add(
            [
               svg:rect({200,100}, [{fill, '#333'}]),
               svg:rect({200,100}, [{fill, '#666'}]),
               svg:rect({200,100}, [{fill, '#999'}])
            ],
            svg:layout(vertical, [
               {stroke, '#eee'},
               {'stroke-width', 1}
            ])
         ),
         svg:init(200, 300)
      )
   ).



