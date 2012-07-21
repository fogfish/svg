-module(svg_test).
-include_lib("eunit/include/eunit.hrl").

rect_test() ->
   ok = svg:write("rect.svg",
      svg:add(
   	   svg:add(
   	      svg:rect({50,50}, {100,100}),
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
   	      svg:circle({100,100}, 50),
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

