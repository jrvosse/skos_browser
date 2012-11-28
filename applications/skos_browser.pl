:- module(skos_browser, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(yui3_beta)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

:- http_handler(skosbrowser(.), http_skos_browser, []).
:- http_handler(skosbrowser(concept), http_concept_info, []).

%%	http_skos_browser(+Request)
%
%	HTTP handler for web page that instantiates a JavaScript
%	column browser widget. It uses the skos apis defined in
%	@skos_concepts

http_skos_browser(_Request) :-
	reply_html_page(cliopatria(default),
			[ title(['SKOS vocabulary browser'])
			],
			[  \html_requires(css('skosbrowser.css')),
			   \html_requires(css('columnbrowser.css')),
			   h2('SKOS vocabulary browser'),
			   div([class('yui-skin-sam'), id(browser)], []),
			   div([id(detail)], []),
			   script(type('text/javascript'),
				  [ \yui_script
				  ])
			]).

%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script -->
	{ findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  DS = datasource,
	  Browser = browser
	},
	yui3([json([modules(json(Modules))])],
	     [datasource,'node-load'|Includes],
	     [ \skos_api_datasource(DS),
	       \skos_browser(DS, Browser),
	       \yui3_on(Browser, itemSelect, [e], \js_item_select)
	     ]).

js_item_select -->
	{ http_location_by_id(http_concept_info, URL)
	},
	html(['var url = "',URL,'"+"?concept="+e.id;']),
	yui3_load(one(id(detail)), symbol(url)).

skos_api_datasource(DS) -->
	yui3_new(DS,
		 'Y.DataSource.IO',
		 {source:''}
		 ),
	yui3_plug(DS,
		  'Y.Plugin.DataSourceJSONSchema',
		  {schema: {resultListLocator: results,
			    resultFields: [id, label, hasNext, count, class, matches, scheme],
			    metaFields: {totalNumberOfResults:totalNumberOfResults}
			   }
		  }),
	yui3_plug(DS,
		  'Y.Plugin.DataSourceCache',
		  {max:10}).

skos_browser(DS, Browser) -->
	{ http_location_by_id(http_conceptschemes, ConceptSchemes),
	  http_location_by_id(http_concepts, Concepts)
	},
	yui3_new(Browser,
		 'Y.mazzle.ColumnBrowser',
		 {datasource: symbol(DS),
		  maxNumberItems: 100,
		  columns: [
			    {   request: ConceptSchemes,
				label: 'concept scheme'
			    },
			    {   request: Concepts,
				label: concept,
				params: {type:'topconcept'},
				options: [
					  {value:inscheme, label:'all concepts'},
					  {value:topconcept, selected:true, label: 'top concepts'}
					 ]
			    },
			    {   request: Concepts,
				params: {type:child},
				options: [],
				repeat: true
			    }
			   ]
		 }),
	yui3_render(Browser, id(browser)).

js_module(resourcelist, json([fullpath(Path),
			      requires([node,event,widget])
			     ])) :-
    http_absolute_location(js('resourcelist.js'), Path, []).
js_module(columnbrowser, json([fullpath(Path),
			     requires([node,event,widget,resourcelist])
			    ])) :-
    http_absolute_location(js('columnbrowser.js'), Path, []).



		 /*******************************
		 *	    concept view	*
		 *******************************/

%%	http_concept_info(+Request)
%
%       API handler to fetch info about a URI.
%
%       @TBD support for language tags

http_concept_info(Request) :-
	http_parameters(Request,
			[  concept(C,
			       [description('Concept to request info about')])
			]),
	rdf_display_label(C, Label),
	skos_description(C, Desc),
	skos_alt_labels(C, AltLabels0),
	delete(AltLabels0, Label, AltLabels),
	skos_related_concepts(C, Related),
	format('Content-type: text/html~n~n'),
	phrase(html(\html_info_snippet(C, Label, Desc, AltLabels, Related)), HTML),
	print_html(HTML).

skos_description(C, Desc) :-
	(   rdf_has(C, skos:scopeNote, Lit)
	->  literal_text(Lit, Desc)
	;   Desc = ''
	).

skos_alt_labels(C, AltLabels) :-
	findall(AL, ( rdf_has(C, skos:altLabel, Lit),
		      literal_text(Lit, AL)
		    ),
		AltLabels0),
	sort(AltLabels0, AltLabels).

skos_related_concepts(C, Related) :-
	Concept = concept(R, Label),
	findall(Concept, ( skos_related(C, R),
			   rdf_display_label(R, Label)
		    ),
		Related).

skos_related(C, R) :-
	rdf_has(C, skos:related, R).
skos_related(C, R) :-
	rdf_has(R, skos:related, C),
	\+ rdf_has(C, skos:related, R).

html_info_snippet(URI, Label, Desc, AltLabels, Related) -->
	html(div(class(infobox),
		 [ h3([\resource_link(URI, Label)
		      ]),
		   div(class(uri), URI),
		   div(
		       [ div(class(labels),
			     \html_label_list(AltLabels)),
			 div(class(desc), Desc),
			 div(class(related),
			     \html_concept_list(Related))
		       ])
		 ])).

html_label_list([]) --> !.
html_label_list([L]) --> !,
	html(span(class(label), L)).
html_label_list([L|Ls]) -->
	html(span(class(label), [L,', '])),
	html_label_list(Ls).

html_concept_list([]) --> !.
html_concept_list([concept(URI, Label)]) --> !,
	resource_link(URI, Label).
html_concept_list([concept(URI, Label)|Cs]) -->
	html([\resource_link(URI, Label), ', ']),
	html_concept_list(Cs).

resource_link(URI, Label) -->
	{ www_form_encode(URI, EncURI)
	},
	html(a(href(location_by_id(list_resource)+'?r='+EncURI), Label)).
