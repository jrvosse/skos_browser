:- module(skos_concepts,
	  [concept_results/3,
	   http_concepts/1
	  ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

:- use_module(library(skos/util)).

:- http_handler(skosapi(conceptschemes), http_conceptschemes, []).
:- http_handler(skosapi(concepts), http_concepts, []).

:- multifile
	cliopatria:conceptscheme_property/3,
	cliopatria:concept_property/4.

%%	http_concept_schemes(+Request)
%
%       API handler to fetch concept schemes

http_conceptschemes(Request) :-
	http_parameters(Request,
			[ offset(Offset,
				[integer, default(0),
				 description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20),
				 description('maximum number of results returned')]),
			  query(Query,
				[optional(true),
				 description('keyword query to filter the results by')])
			]),
	findall(Label-Concept, concept_scheme(Query, Concept, Label), Concepts),
	sort(Concepts, Sorted),
	length(Sorted, Total),
	list_offset(Sorted, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	maplist(conceptscheme_result, LimitResults, JSONResults),
	reply_json(json([offset=Offset,
			 limit=Limit,
			 totalNumberOfResults=Total,
			 results=JSONResults])).

concept_scheme(Query, C, Label) :-
	var(Query),
	!,
	skos_is_vocabulary(C),
	rdf_display_label(C, Label).
concept_scheme(Query, C, Label) :-
	skos_is_vocabulary(C),
	once(label_prefix(Query, C, Lit)),
	literal_text(Lit, Label).


%%	http_concepts(+Request)
%
%       API handler to fetch top concepts

http_concepts(Request) :-
	http_parameters(Request,
			[ parent(Parent,
				 [description('Concept or concept scheme from which we request the concepts')]),
			  type(Type,
			       [oneof(topconcept,inscheme,child,descendant,related),
				default(inscheme),
				description('Method to determine the concepts')]),
			  offset(Offset,
				[integer, default(0),
				 description('Start of the results returned')]),
			  limit(Limit,
				[integer, default(20),
				 description('maximum number of results returned')]),
			  query(Query,
				[optional(true),
				 description('keyword query to filter the results by')]),
			  graph(Graphs,
				[zero_or_more,
				 description('Named graph to restrict the concepts by')
				])
			]),
	%@TBD use Graphs in concept_of
	findall(Label-Concept, concept_of(Type, Parent, Query, Concept, Label), Concepts),
	sort(Concepts, Sorted),
	length(Sorted, Total),
	list_offset(Sorted, Offset, OffsetResults),
	list_limit(OffsetResults, Limit, LimitResults, _),
	concept_results(LimitResults, Graphs, JSONResults),
	reply_json(json([parent=Parent,
			 offset=Offset,
			 limit=Limit,
			 totalNumberOfResults=Total,
			 results=JSONResults])).

concept_of(Type, Parent, Query, Concept, Label) :-
	var(Query),
	!,
	concept(Type, Parent, Concept),
	rdf_display_label(Concept, Label).

concept_of(Type, Parent, Query, Concept, Label) :-
	concept(Type, Parent, Concept),
	once(label_prefix(Query, Concept, Lit)),
	literal_text(Lit, Label).

concept(inscheme, ConceptScheme, Concept) :- !,
	skos_in_scheme(ConceptScheme, Concept).
concept(topconcept, ConceptScheme, Concept) :- !,
	skos_top_concept(ConceptScheme, Concept).
concept(child, Parent, Concept) :-
	skos_parent_child(Parent, Concept).
concept(descendant, Parent, Concept) :-
	skos_descendant_of(Parent, Concept).
concept(related, Parent, Concept) :-
	skos_related_concept(Parent, Concept).


		 /*******************************
		 *	  Concept JSON		*
		 *******************************/

%%	conceptscheme_result(+Pair:label-uri, -JSON_Object)

conceptscheme_result(Label-URI, json(JSON)) :-
	JSON = [id=URI, label=Label, hasNext=true|More],
	findall(Key= Value, conceptscheme_result_property(Key, URI, Value), More).

conceptscheme_result_property(Key, URI, Value) :-
	catch(cliopatria:conceptscheme_property(Key, URI, Value), _, fail).

%%	concept_results(+Pair, +Graphs, -JSON_Object)
%
%

concept_results([], _, []).
concept_results([C|Cs], Graphs, [O|Os]) :-
	concept_result(C, Graphs, O),
	concept_results(Cs, Graphs, Os).

concept_result(Label-URI, Graphs, json(JSON)) :-
	JSON = [id=URI, label=Label|More],
	findall(Key= Value, concept_result_property(Key, URI, Graphs, Value), More).

concept_result_property(hasNext, URI, _, Boolean) :-
	has_narrower(URI, Boolean).

concept_result_property(Key, URI, Graphs, Value) :-
	catch(cliopatria:concept_property(Key, URI, Graphs, Value), _, fail).


%%	has_narrower(+Concept, -Boolean)
%
%	Boolean is true when concept has a skos:narrower concept.

has_narrower(Concept, @true) :-
	skos_parent_child(Concept, _Child),
	!.

has_narrower(_, @false).


		 /*******************************
		 *	     UTILILIES          *
		 *******************************/


%%	list_offset(+List, +N, -SmallerList)
%
%	SmallerList starts at the nth element of List.

list_offset(L, N, []) :-
	length(L, Length),
	Length < N,
	!.
list_offset(L, N, L1) :-
	list_offset_(L, N, L1).

list_offset_(L, 0, L) :- !.
list_offset_([_|T], N, Rest) :-
	N1 is N-1,
	list_offset_(T, N1, Rest).

%%	list_limit(+List, +N, -SmallerList, -Rest)
%
%	SmallerList ends at the nth element of List.

list_limit(L, N, L, []) :-
	length(L, Length),
	Length < N,
	!.
list_limit(L, N, L1, Rest) :-
	list_limit_(L, N, L1, Rest).

list_limit_(Rest, 0, [], Rest) :- !.
list_limit_([H|T], N, [H|T1], Rest) :-
	N1 is N-1,
	list_limit_(T, N1, T1, Rest).

%%	label_prefix(+Query, -R, -Lit)
%
%	True if Query matches a literal value of R.

label_prefix(Query, R, Lit) :-
	rdf_has(R, rdfs:label, literal(prefix(Query), Lit)).
label_prefix(Query, R, Lit) :-
	rdf_has(O, rdf:value, literal(prefix(Query), Lit)),
	rdf_has(R, rdfs:label, O).


%%	reply_jsonp(+JSON, +Callback)
%
%	Output an html script node, where JSON is embedded in a
%	javascript funtion.

reply_jsonp(JSON, Callback) :-
	with_output_to(string(JSONString),
		       json_write(current_output, JSON, [])),
	format('Content-type: text/javascript~n~n'),
	phrase(html([Callback,'(',JSONString,')']), HTML),
	print_html(HTML).

