% plot: draw plots by writing files in svg format that can be displayed
%   using a web-browser or anything else that supports svg.  I wrote
%   this based on a few examples from:
%     https://www.w3.org/TR/SVG/
%   I've certainly skipped over *many* things one can do using svg.
%   For this first prototype, I am checking that the options to various
%   svg tags are ones that I've seen in the document above.  That way,
%   we can report errors here in the Erlang code rather than writing
%   a messed up svg file and having some cryptic error in the display
%   software.

-module(plot).

-export([plot/3, plot/2, plot/1, plot_svg/2, test/0]).

% plot_svg(Points, Options) -- create a SVG representation of
%   the plot of Points using the given options.
plot_svg(Points, Options) when is_list(Points) ->
  SVG_Options =
    [ {viewBox, {0, 0, 1, 1}} |
      case get_opt([width, height], Options) of
	[none, none] -> [{width, 800}, {height, 500}];
	[W, none] when is_number(W) -> [{width, W}, {height, 5*W/8}];
	[{W, U}, none] -> [{width, {W, U}}, {height, {5*W/8, U}}];
	[none, H] when is_number(H) -> [{width, 8*H/5}, {height, H}];
	[none, {H, U}] -> [{width, {8*H/5}, U}, {height, {8*H/5, U}}];
	[W, H] -> [{width, W}, {height, H}]
      end
    ],
  Plot_Options = [
    {stroke, get_opt(stroke, Options, "blue")},
    {stroke_width, get_opt(stroke_width, Options, {0.3, '%'})}
  ],
  FontSize = get_opt(font_size, Options, 0.03),
  MajorTickSize = get_opt(tick_size, Options, 0.015),
  MinorTickSize = get_opt(minor_tick_size, Options, 0.01),

  [AX, AY] = get_opt([x_axis, y_axis], Options, []),
  {AX_Coords, AX_Text} = lists:unzip(AX),
  {AY_Coords, AY_Text} = lists:unzip(AY),
      
  LeftLabelMargin = FontSize*lists:max([0 |
    [ lists:sum([charWidth(C) || C <- Label]) || Label <- AY_Text ]]),
  LeftTickMargin = case LeftLabelMargin of
    _ when LeftLabelMargin > 0 -> MajorTickSize;
    _ when AY_Text == [] -> 0;
    _ -> MinorTickSize/2
  end,
  LeftMargin = 0.01 + LeftLabelMargin + LeftTickMargin,
  
  BottomLabelMargin = case lists:any(fun(Label) -> Label =/= [] end, AX_Text) of
    true  -> FontSize;
    false -> 0
  end,
  BottomTickMargin = case BottomLabelMargin of
    _ when BottomLabelMargin > 0 -> MajorTickSize;
    _ when AX_Text == [] -> 0;
    _ -> MinorTickSize/2
  end,
  BottomMargin = 0.01 + BottomLabelMargin + BottomTickMargin,
  {XX, YY} = coords(Points),
  DX = (Xmax = lists:max(AX_Coords ++ XX)) - (Xmin = lists:min(AX_Coords ++ XX)),
  DY = (Ymax = lists:max(AY_Coords ++ YY)) - (Ymin = lists:min(AY_Coords ++ YY)),
  Xscale = case DX > 1.0e-12*max(abs(Xmin), abs(Xmax)) of
    true  -> (0.99-LeftMargin)/DX;
    false -> infinity
  end,
  Yscale = case DY > 1.0e-12*max(abs(Ymin), abs(Ymax)) of
    true  -> (0.99-FontSize-BottomMargin)/DY;
    false -> infinity
  end,
  FixX = case Xscale of
    infinity -> fun(_) -> (LeftMargin + 0.99)/2 end;
    _ -> fun(X) -> Xscale*(X-Xmin) + LeftMargin end
  end,
  FixY = case Yscale of
    infinity -> fun(_) -> (BottomMargin + 0.01 + FontSize)/2 end;
    _ -> fun(Y) -> Yscale*(Ymax-Y) + 0.01 + FontSize end
  end,
  Plots = curves(Points, Plot_Options, {FixX, FixY}),
  AxisX = case AX of
    [] -> [];
    _ ->
      [ { polyline,
          svg:polyline_make([ {stroke, "black"}, {stroke_width, {0.2, '%'}},
                              {points, [ {FixX(Xmin), FixY(Ymin)},
			      		 {FixX(Xmax), FixY(Ymin)}]}]) } |
	[     case Text of
		[] ->
		  PosX = FixX(X),
		  PosY = FixY(Ymin),
		  { polyline, % no label, a "minor" tick
		    svg:polyline_make([ {stroke, "black"}, {stroke_width, {0.2, '%'}},
					{points, [ {PosX, PosY-(MinorTickSize/2)},
					           {PosX, PosY+(MinorTickSize/2)}]}]) };
		_  ->
		  PosX = FixX(X),
		  PosY = FixY(Ymin),
		  [ { polyline, % has a label, a "major" tick
		      svg:polyline_make([ {stroke, "black"}, {stroke_width, {0.2, '%'}},
					  {points, [ {PosX, PosY+(MajorTickSize/2)},
					             {PosX, PosY-(MajorTickSize/2)} ]}]) },
		    { text, 
		      svg:text_make([ {x, PosX}, {y, PosY+MajorTickSize+FontSize}, {font_size, FontSize},
				      {font_family, "sans-serif"}, {anchor, middle},
				      {elements, [{string, Text}]}]) }
		  ]
	      end
	  ||  {X, Text} <- AX
	]
      ]
  end,
  AxisY = case AY of
    [] -> [];
    _ ->
      [ { polyline,
          svg:polyline_make([ {stroke, "black"}, {stroke_width, {0.2, '%'}},
                              {points, [ {FixX(Xmin), FixY(Ymin)},
			      		 {FixX(Xmin), FixY(Ymax)}]}]) } |
	[     case Text of
		[] ->
		  PosX = FixX(Xmin),
		  PosY = FixY(Y),
		  { polyline, % no label, a "minor" tick
		    svg:polyline_make([ {stroke, "black"}, {stroke_width, {0.2, '%'}},
					{points, [ {PosX-(MinorTickSize/2), PosY},
					           {PosX+(MinorTickSize/2), PosY}]}]) };
		_  ->
		  PosX = FixX(Xmin),
		  PosY = FixY(Y),
		  [ { polyline, % has a label, a "major" tick
		      svg:polyline_make([ {stroke, "black"}, {stroke_width, {0.2, '%'}},
					  {points, [ {PosX+(MajorTickSize/2), PosY},
					             {PosX-(MajorTickSize/2), PosY} ]}]) },
		    { text, 
		      svg:text_make([ {x, PosX-MajorTickSize}, {y, PosY + 0.3*FontSize}, {font_size, FontSize},
				      {font_family, "sans-serif"}, {anchor, 'end'},
				      {elements, [{string, Text}]}]) }
		  ]
	      end
	  ||  {Y, Text} <- AY
	]
      ]
  end,
  Elements = [Plots, AxisX, AxisY],
  svg:svg_make(SVG_Options ++ [{elements, lists:flatten(Elements)}]).

get_opt([Key | Tl], Options, Default) ->
  [get_opt(Key, Options, Default) | get_opt(Tl, Options, Default)];
get_opt([], _, _) -> [];
get_opt(Key, Options, Default) ->
  case lists:keyfind(Key, 1, Options) of
    {Key, V} -> V;
    false -> Default
  end.
get_opt(Key, Options) -> get_opt(Key, Options, none).

charWidth(C) when $0 =< C, C =< $9 -> 0.56; % sans-serif
charWidth($.) -> 0.28; % sans-serif
charWidth($+) -> 0.59; % Monospace
charWidth($-) -> 0.59; % Monospace
charWidth($e) -> 0.56; % sans-serif
charWidth(C) when $a =< C, C =< $z -> 0.72; % sans-serif, w
charWidth(C) when $A =< C, C =< $Z -> 0.94; % sans-serif, W
charWidth(_) ->  1.00. % sans-serif, @

coords([]) -> {[],[]};
coords(Points=[{X1,Y1}|_]) when is_number(X1), is_number(Y1)-> lists:unzip(Points);
coords([{plot, Points, _Options} | Tl]) ->
  {XX,YY} = coords(Points),
  {TlX, TlY} = coords(Tl),
  {XX++TlX, YY++TlY}.
  
curves([], _, _) -> [];
curves(Points=[{_,_}|_], Plot_Options, {FixX, FixY}) ->
  {polyline, svg:polyline_make([{points, [{FixX(X), FixY(Y)} || {X, Y} <- Points]} | Plot_Options])};
curves([{plot, Points, Options} | Tl], Plot_Options, FixXY) ->
  [   curves(Points, Options ++ Plot_Options, FixXY)
    | curves(Tl, Plot_Options, FixXY) ].


plot(File, Points, Options) when is_pid(File) ->
  io:format(File, "~s", [svg:svg_toString(plot_svg(Points, Options))]);
%
plot(FileName, Points, Options) when is_list(FileName) ->
  File = case file:open(FileName, [write]) of
    {ok, FFile} -> FFile;
    {error, Reason} ->
      io:format("could not open file \"~s\" to write: ~p~n", [FileName, Reason]),
      error(Reason)
  end,
  try plot(File, Points, Options)
  catch What:Why ->
    error({What, Why, erlang:get_stacktrace()})
  end,
  file:close(File).
plot(Points, Options) -> plot("plot.svg", Points, Options).
plot(Points) -> plot(Points, []).

test() ->
  {ok, F} = file:open("test.svg", [write]),
  io:format(F, "~s~n", [svg:svg_toString(plot([{I/40, math:cos(I/40)} || I <- lists:seq(0,200)],
    [ {x_axis, [{N, [$0 + N]} || N <- lists:seq(0,5)]},
      {y_axis, [{N/2, lists:flatten(io_lib:format("~4.1f", [N/2]))} || N <- lists:seq(-2,2)]
               ++ [{N/20, []} || N <- lists:seq(-20,20), (N rem 10) /= 0]}
      ]))]),
  file:close(F).
