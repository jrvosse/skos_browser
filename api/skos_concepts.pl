:- module(skos_concepts,
	  []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).


:- http_handler(skosapi(conceptschemes), http_conceptschemes, []).
:- http_handler(skosapi(concepts), http_concepts, []).

:- multifile
	cliopatria:conceptscheme_property/3,
	cliopatria:concept_property/3.

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
	rdf(C, rdf:type, skos:'ConceptScheme'),
	rdf_display_label(C, Label).
concept_scheme(Query, C, Label) :-
	rdf(C, rdf:type, skos:'ConceptScheme'),
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
	inscheme(ConceptScheme, Concept).
concept(topconcept, ConceptScheme, Concept) :- !,
	top_concept(ConceptScheme, Concept).
concept(child, Parent, Concept) :-
	narrower_concept(Parent, Concept).
concept(descendant, Parent, Concept) :-
	descendant(Parent, Concept).
concept(related, Parent, Concept) :-
	related_concept(Parent, Concept).

%%	inscheme(+ConceptScheme, -Concept)
%
%	True if Concept is contained in a skos:ConceptScheme by
%	skos:inScheme.

inscheme(ConceptScheme, Concept) :-
	rdf(Concept, skos:inScheme, ConceptScheme).

%%	top_concept(+ConceptScheme, -Concept)
%
%	True if Concept is a skos:hasTopConcept of ConceptScheme, or
%	inversely by skos:topConceptOf

top_concept(ConceptScheme, Concept) :-
	rdf(ConceptScheme, skos:hasTopConcept, Concept).
top_concept(ConceptScheme, Concept) :-
	rdf(Concept, skos:topConceptOf, ConceptScheme),
	\+ rdf(ConceptScheme, skos:hasTopConcept, Concept).

%%	narrower_concept(+Concept, -Narrower)
%
%	True if Narrower is related to Concept by skos:narrower or
%	inversely by skos:broader.

narrower_concept(Concept, Narrower) :-
	rdf_has(Narrower, skos:broader, Concept).
narrower_concept(Concept, Narrower) :-
	rdf_has(Concept, skos:narrower, Narrower),
	\+ rdf_has(Narrower, skos:narrower, Concept).

%%	descendant(+Concept, -Descendant)
%
%	Descendant is a child of Concept or recursively of its children

descendant(Concept, Descendant) :-
	narrower_concept(Concept, Narrower),
	(   Descendant = Narrower
	;   descendant(Narrower, Descendant)
	).

%%	related_concept(+Concept, -Related)
%
%	True if Related is related to Concept by skos:related.

related_concept(Concept, Related) :-
	rdf_has(Concept, skos:related, Related).
related_concept(Concept, Related) :-
	rdf_has(Related, skos:related, Concept),
	\+ rdf_has(Concept, skos:related, Related).




		 /*******************************
		 *	  Concept JSON		*
		 *******************************/

%%	conceptscheme_result(+Pair:label-uri, -JSON_Object)

conceptscheme_result(Label-URI, json(JSON)) :-
	JSON = [id=URI, label=Label, hasNext=true|More],
	findall(Key= Value, conceptscheme_result_property(Key, URI, Value), More).

conceptscheme_result_property(Key, URI, Value) :-
	catch(cliopatria:conceptscheme_property(Key, URI, Value), _, fail).

%%	concept_results(+Pair:label-uri, +Graphs -JSON_Object)

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
	rdf_has(_, skos:broader, Concept),
	!.
has_narrower(Concept, @true) :-
	rdf_has(Concept, skos:narrower, _),
	!.
has_narrower(_, @false).


		 /*******************************
		 *	     UTILILIES          *
		 *******************************/

%%	terms_sort_by_arg(+ListOfTerms, +N, -SortedTerms)
%
%	Sorts ListOfTerms by the nth argument of each term.

term_sort_by_arg(List, Arg, Sorted) :-
	maplist(arg_key(Arg), List, Pairs),
	keysort(Pairs, Sorted0),
	pairs_values(Sorted0, Sorted).

arg_key(N, Term, Key-Term) :-
	arg(N, Term, Key).

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

