% svg: draw plots by writing files in svg format that can be displayed
%   using a web-browser or anything else that supports svg.  I wrote
%   this based on a few examples from:
%     https://www.w3.org/TR/SVG/
%   I've certainly skipped over *many* things one can do using svg.
%   For this first prototype, I am checking that the options to various
%   svg tags are ones that I've seen in the document above.  That way,
%   we can report errors here in the Erlang code rather than writing
%   a messed up svg file and having some cryptic error in the display
%   software.

-module(svg).

-export([svg_make/1, svg_make/2, is_svg/1, is_svg/2, svg_toString/1, svg_toString/2,
         polyline_make/1, polyline_make/2, is_polyline/1, is_polyline/2,
	 polyline_toString/1, polyline_toString/2,
         text_make/1, text_make/2, is_text/1, is_text/2,
	 text_toString/1, text_toString/2,
	 is/2, is/3, maybe/2
	 ]).

-export([coord_toString/1, coord_toString/2, num_toString/1, num_toString/2]).


% An Erlang record for the svg element.
%  I'm supporting a subset of the attributes described at
%    https://www.w3.org/TR/SVG/struct.html#SVGElement
-record(svg, { height={500,px},
	       preserveAspectRatio=none,
	       version="1.1",
	       viewBox={0,0,1000,1500},
	       width={800,px},
	       xmlns="http://www.w3.org/2000/svg",
	       elements=[]
	     }).

svg_fields() ->
  #svg{width=width, height=height, preserveAspectRatio=preserveAspectRatio,
       version=version, viewBox=viewBox, xmlns=xmlns, elements=elements}.

svg_field_checks() ->
  [ {height,              coord},
    {preserveAspectRatio, aspectRatioSpec},
    {version,             string},
    {viewBox,             box},
    {width,               coord},
    {xmlns,               xmlns},
    {elements,            svg_element_list}
  ].

svg_make(Fields, OtherStuff) -> make_record(svg, Fields, OtherStuff).
svg_make(Fields) -> svg_make(Fields, true).

svg_toString(none, _) -> none;
svg_toString(SVG, Indent) ->
  I5 = Indent+5,
  NextIndent = indent_step(Indent),
  [ io_lib:format("<svg width=\"~s\"", [num_toString(SVG#svg.width, indent_step(I5))]),
    [ [ $\n, spaces(I5),
	io_lib:format(Fmt, [Str])
      ] || {Fmt, Str} <- [
	{"height=\"~s\"",       num_toString(SVG#svg.height, indent_step(I5))},
	{"viewBox=~s",          viewBox_toString(SVG#svg.viewBox, indent_step(I5))},
	{"xmlns=~s",            xmlns_toString(SVG#svg.xmlns, indent_step(I5)) },
	{"version=\"~s\"",  SVG#svg.version }
      ]
    ],
    ">\n",
    [ [ spaces(NextIndent),
	svg_element_toString(E, NextIndent),
	$\n
      ] || E <- SVG#svg.elements
    ],
    spaces(Indent), "</svg>"
  ].

svg_toString(SVG) -> svg_toString(SVG, 0).


% An Erlang record for the polyline element.
-record(polyline,
        { points=[],
          stroke=none,
	  stroke_width=none
	}).

polyline_fields() ->
  #polyline{points=points, stroke=stroke, stroke_width=stroke_width}.

polyline_field_checks() ->
  [ {points,       point_list},
    {stroke,       maybe_string},
    {stroke_width, maybe_coord}
  ].

polyline_make(Fields, OtherStuff) -> make_record(polyline, Fields, OtherStuff).
polyline_make(Fields) -> polyline_make(Fields, true).

polyline_toString(none, _) -> none;
polyline_toString(P, Indent) ->
  I10 = Indent+10,
  [ "<polyline fill=\"none\"",
    [    maybe_format(Fmt, Str)
      || {Fmt, Str} <- [
        {" stroke=\"~s\"",        P#polyline.stroke},
        {" stroke-width=\"~s\"",  num_toString(P#polyline.stroke_width, indent_step(I10))}
      ]
    ],
    $\n, spaces(I10), "points=\"",
    point_list_toString(P#polyline.points, I10+8),
    "\" />"
  ].
polyline_toString(P) -> polyline_toString(P, 0).

point_list_toString(none, _) -> none;
point_list_toString([P1, P2, P3, P4, P5 | Tl], Indent) ->
  [ point_list_toString([P1, P2, P3, P4]),
    $\n, spaces(Indent),
    point_list_toString([P5 | Tl], Indent)
  ];
point_list_toString(ShortList, _) -> point_list_toString(ShortList).
point_list_toString([]) -> [];
point_list_toString([Point | Tl]) ->
  [ point_toString(Point), "  " | point_list_toString(Tl) ].

point_toString({X, Y}) -> [ coord_toString(X), ", ", coord_toString(Y) ].

% An Erlang record for a text element.
-record(text, 
        { x=0, y=0,
          font_size={0.03, '%'},
	  font_family="sans-serif",
	  anchor=start,
	  elements=[]
	}).

text_fields() ->
  #text{x=x,y=y,font_size=font_size,font_family=font_family,anchor=anchor,elements=elements}.

text_field_checks() ->
  [ {x,           coord},
    {y,           coord},
    {font_size,   coord},
    {font_family, string},
    {anchor,      anchor},
    {elements,    text_element_list}
  ].

text_make(Fields, OtherStuff) -> make_record(text, Fields, OtherStuff).
text_make(Fields) -> text_make(Fields, true).

text_toString(none, _) -> none;
text_toString(T, Indent) ->
  I6 = Indent+6,
  [ "<text",
    [    maybe_format(Fmt, Str)
      || {Fmt, Str} <- [
        {" x=\"~s\"",            num_toString(T#text.x, indent_step(I6))},
        {" y=\"~s\"",            num_toString(T#text.y, indent_step(I6))},
        {" font-size=\"~s\"",    num_toString(T#text.font_size, indent_step(I6))},
        {" font-family=\"~s\"",  T#text.font_family},
	{" text-anchor=\"~s\"",  anchor_toString(T#text.anchor)}
      ]
    ],
    " >\n",
    [    [ spaces(indent_step(Indent)),
           text_element_toString(E,indent_step(Indent)), $\n
	 ]
      || E <- T#text.elements
    ],
    spaces(Indent), "</text>"
  ].
text_toString(T) -> text_toString(T, 0).

anchor_toString(none) -> none;
anchor_toString('end') -> "end";
anchor_toString(A) -> atom_to_list(A).

coord_toString(X) when is_integer(X), -1000 < X, X < 10000 ->
  io_lib:format("~4b", [X]);
coord_toString(X) when is_integer(X) -> io_lib:format("~b", [X]);
coord_toString(X) when is_float(X), 0.1 < abs(X), abs(X) < 999.0 ->
  io_lib:format("~10.5f", [X]);
coord_toString(X) when is_float(X), 1.0e-3 < abs(X), abs(X) < 1.0e6 ->
  io_lib:format("~f", [X]);
coord_toString(X) when is_float(X) -> io_lib:format("~12.3e", [X]);
coord_toString({X,'%'}) -> [ coord_toString(X), $% ];
coord_toString({X,U}) -> [ coord_toString(X), atom_to_list(U) ].

coord_toString(none, _) -> none;
coord_toString(X, _) -> coord_toString(X).

num_toString(X) ->
  lists:dropwhile(fun(C) -> C == $\s end, lists:flatten(coord_toString(X))).
num_toString(none, _) -> none;
num_toString(X, _) -> num_toString(X).

svg_element_toString(none, _) -> none;
svg_element_toString({polyline, P}, Indent) -> polyline_toString(P, Indent);
svg_element_toString({text, T}, Indent) -> text_toString(T, Indent);
svg_element_toString({Type, _}, _) ->
  error({bad_element_type, Type}).

text_element_toString({string, S}, _) -> S.

viewBox_toString(none, _) -> none;
viewBox_toString({Xmin, Ymin, Xmax, Ymax}, _) ->
  io_lib:format("\"~w ~w ~w ~w\"", [Xmin, Ymin, Xmax, Ymax]).

% xmlns -- just strings for now.
xmlns_toString(none, _) -> none;
xmlns_toString(X, _) -> [$", X, $"].

% type recognizers that we use
is_anchor(A) -> lists:member(A, [start, middle, 'end', inherit]).
is_aspectRatioSpec(A) ->
  lists:member(A, [ none, xMinYMin, xMidYMin, xMaxYMin, xMinYMid, xMidYMid,
		    xMaxYMid, xMinYMax, xMidYMax, xMaxYMax ]).

is_box(T) when is_tuple(T), tuple_size(T) == 4 ->
  lists:all(fun(X) -> is_number(X) end, tuple_to_list(T)).

is_coord(X) when is_number(X) -> true;
is_coord({X, U}) when is_number(X) -> is_unit(U);
is_coord(_) -> false.

is_element(ElementTypes, {ElementType, ElementValue}) ->
  case lists:member(ElementType, ElementTypes) of
    true -> is(ElementType, ElementValue);
    false -> false
  end.

is_point({X,Y}) -> is_coord(X) andalso is_coord(Y);
is_point(_) -> false.

is_point_list(L) -> is_list_of_type(point, L).

is_polyline(P, Verbose) -> is_valid_record(polyline, P, Verbose).
is_polyline(P) -> is_valid_record(polyline, P, false).

% This definition of "string" may not be quite the same as the one defined or
%   assumed by the W3C working group, but we'll live with it until it's shown
%   to be a problem.
is_string(S) -> io_lib:printable_latin1_list(S).

is_svg(S, Verbose) -> is_valid_record(svg, S, Verbose).
is_svg(S) -> is_valid_record(svg, S, false).

is_svg_element(E) -> is_element([polyline, text], E).
is_svg_element_list(L) -> is_list_of_type(svg_element, L).

is_text(T, Verbose) -> is_valid_record(text, T, Verbose).
is_text(T) -> is_valid_record(text, T, false).

is_text_element(E) -> is_element([string], E).
is_text_element_list(L) -> is_list_of_type(text_element, L).

is_unit(U) -> lists:member(U, [em, ex, px, pt, pc, cm, mm, in, '%']).

% I don't want to figure out all of the rules for xml names.
%   For now, strings that I'll assume that the string is a valid URI (which
%   is roughly equivalent to being a valid URL).
is_xmlns(S) -> is_string(S).

% a few weaker versions that allow the atom of 'none'
maybe_coord(none) -> true;
maybe_coord(C) -> is_coord(C).

maybe_string(none) -> true;
maybe_string(S) -> is_string(S).

is(anchor, X) -> is_anchor(X);
is(aspectRatioSpec, X) -> is_aspectRatioSpec(X);
is(box, X) -> is_box(X);
is(coord, X) -> is_coord(X);
is(point, X) -> is_point(X);
is(point_list, X) -> is_point_list(X);
is(polyline, X) -> is_polyline(X);
is(string, X) -> is_string(X);
is(svg, X) -> is_svg(X);
is(svg_element, X) -> is_svg_element(X);
is(svg_element_list, X) -> is_svg_element_list(X);
is(text, X) -> is_text(X);
is(text_element, X) -> is_text_element(X);
is(text_element_list, X) -> is_text_element_list(X);
is(unit, X) -> is_unit(X);
is(xmlns, X) -> is_xmlns(X);
is(maybe_coord, X) -> maybe(coord, X);
is(maybe_string, X) -> maybe(string, X).

is(polyline, X, Verbose) -> is_polyline(X, Verbose);
is(svg, X, Verbose) -> is_svg(X, Verbose);
is(text, X, Verbose) -> is_text(X, Verbose).

maybe(coord, X) -> maybe_coord(X);
maybe(string, X) -> maybe_string(X).

fields(polyline) -> polyline_fields();
fields(svg) -> svg_fields();
fields(text) -> text_fields().

make_record(polyline) -> #polyline{};
make_record(svg) -> #svg{};
make_record(text) -> #text{}.

field_checks(polyline) -> polyline_field_checks();
field_checks(svg) -> svg_field_checks();
field_checks(text) -> text_field_checks().

% a utility for constructing records
make_record(RecordType, [], FieldRef) when is_tuple(FieldRef) -> make_record(RecordType);
make_record(RecordType, [{Field, Value} | Tl], FieldRef) when is_tuple(FieldRef) ->
  case index(Field, FieldRef) of
    I when is_integer(I) ->
      setelement(I, make_record(RecordType, Tl, FieldRef), Value);
    _ -> error({unknown_field, polyline, Field})
  end;
make_record(RecordType, Fields, Verbose) when is_boolean(Verbose) ->
  New = make_record(RecordType, Fields, fields(RecordType)),
  case is(RecordType, New, Verbose) of
    true -> New;
    false -> error({bad_field, RecordType})
  end.


is_valid_record(_, Fields, _, _, WhichField)
    when WhichField == tuple_size(Fields)+1 -> no_error;
is_valid_record(R, Fields, FieldChecks, Verbose, WhichField) ->
  F = element(WhichField, Fields),
  FieldType = case lists:keyfind(F, 1, FieldChecks) of
    {_, FType} -> FType;
    false -> error({unknown_field, F})
  end,
  case is(FieldType, element(WhichField, R)) of
    true -> is_valid_record(R, Fields, FieldChecks, Verbose, WhichField+1);
    _ -> {"wrong type for field ~p", [F]}
  end.

is_valid_record(RecordType, X, Verbose) ->
  Fields = fields(RecordType),
  FieldChecks = field_checks(RecordType),
  Msg = case {tuple_size(X), tuple_size(Fields)} of
    {0, 0} -> error({bad_spec, is_valid_record, no_fields});
    {N, N} ->
      case element(1, X) == element(1, Fields) of
        true -> is_valid_record(X, Fields, FieldChecks, Verbose, 2);
	false -> { "wrong type for record: expected ~w, got ~w",
	           [RecordType, element(1, X)] }
      end;
    {N1, N2} when N1 < N2 -> {"not enough fields, found ~b, expected ~b", [N1-1, N2-1]};
    {N1, N2} when N1 > N2 -> {"too many fields, found ~b, expected ~b", [N1-1, N2-1]}
  end,
  case Msg of
    no_error -> true;
    {ErrMsgFmt, ErrMsgData} ->
      case Verbose of
        false -> ok;
	true  -> io:format("#~w:  " ++ ErrMsgFmt ++ "~n", [RecordType | ErrMsgData])
      end
  end,
  Msg == no_error.

is_list_of_type(Type, List) -> lists:all(fun(X) -> is(Type, X) end, List).

maybe_format(_, none) -> [];
maybe_format(Fmt, Str) -> io_lib:format(Fmt, [Str]).

spaces(0) -> [];
spaces(N) when is_integer(N), N > 0 -> [$\s | spaces(N-1)].

index(_, [], _) -> none;
index(Key, [Key | _], Pos) -> Pos;
index(Key, [_ | Tl], Pos) -> index(Key, Tl, Pos+1).
index(Key, List) when is_list(List) -> index(Key, List, 1);
index(Key, Tuple) when is_tuple(Tuple) -> index(Key, tuple_to_list(Tuple)).

indent_step(Indent) -> Indent+2.
