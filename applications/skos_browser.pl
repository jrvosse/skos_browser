:- module(skos_browser, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(yui3_beta)).

:- http_handler(skosbrowser(.), http_skos_browser, []).


%%	http_skos_browser(+Request)
%
%	HTTP handler for web page that instantiates a JavaScript
%	column browser widget. It uses the skos apis defined in
%	@skos_concepts

http_skos_browser(_Request) :-
	reply_html_page(cliopatria(default),
			[ title(['SKOS vocabulary browser'])
			],
			[  \html_requires(css('columnbrowser.css')),
			   h2('SKOS vocabulary browser'),
			   div([class('yui-skin-sam'), id(browser)], []),
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
	     [datasource|Includes],
	     [ \skos_api_datasource(DS),
	       \skos_browser(DS, Browser)
	     ]).

skos_api_datasource(DS) -->
	yui3_new(DS,
		 'Y.DataSource.IO',
		 {source:''}
		 ),
	yui3_plug(DS,
		  'Y.Plugin.DataSourceJSONSchema',
		  {schema: {resultListLocator: results,
			    resultFields: [id, label, hasNext, matches, scheme],
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
