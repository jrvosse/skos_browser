YUI.add('columnbrowser', function(Y) {

	var Lang = Y.Lang,
		Widget = Y.Widget,
		Node = Y.Node;

	Widget.ColumnBrowser = ColumnBrowser;
	var NS = Y.namespace('mazzle'); 
	NS.ColumnBrowser = ColumnBrowser;
	
	/* ColumnBrowser class constructor */
	function ColumnBrowser(config) {
		ColumnBrowser.superclass.constructor.apply(this, arguments);
	}

	/* 
	 * Required NAME static field, to identify the Widget class and 
	 * used as an event prefix, to generate class names etc. (set to the 
	 * class name in camel case). 
	 */
	ColumnBrowser.NAME = "columnbrowser";

	/*
	 * The attribute configuration for the ColumnBrowser widget. Attributes can be
	 * defined with default values, get/set functions and validator functions
	 * as with any other class extending Base.
	 */
	ColumnBrowser.ATTRS = {
		datasource: {
			value: null
		},
		columns: {
			value: null
		},
		columnWidth: {
			value: 200,
			validator: Lang.isNumber
		},
		maxNumberItems: {
			value: 100,
			validator: Lang.isNumber
		},
		minQueryLength: {
			value: 2,
			validator: Lang.isNumber
		},
		queryDelay: {
			value: 0.3,
			validator: Lang.isNumber
		},
		selected: {
			value: null
		}
	};

	/* ColumnBrowser extends the base Widget class */
	Y.extend(ColumnBrowser, Widget, {

		initializer: function(config) {
			// internal variables
			this._activeIndex = null;
			this._selectedIndex = null;
			
			// internal references to nodes
			this.columnsBox = null;
			this.columnsNode = null;
			
			this.publish("itemSelect", {});
		},

		destructor : function() {
			// purge itemSelect, optionSelect, offSetSelect, valueChange?
		},

		renderUI : function() {
			this.columnsBox = this.get("contentBox")
				.appendChild(Node.create('<div class="columns-box"></div>'));
			this.columnsNode = this.columnsBox	
				.appendChild(Node.create('<div class="columns"></div>'));
		},

		bindUI : function() {
		},

		syncUI : function() {
			if(this._activeIndex===0||this._activeIndex) {
				var columns = this.get("columns"),
					activeIndex = this._activeIndex,
					selectedIndex = this._selectedIndex,
					selectedItem = this._selectedItem;
			
				// update the status of the columns
				// to "selected" or "hidden"
				for (var i=0; i < columns.length; i++) {
					var list = columns[i].list;
					if(list) {
						if(i==selectedIndex) {
							list.get("boundingBox").addClass("selected");
						} else {
							list.get("boundingBox").removeClass("selected");
						}
						if(i<=activeIndex) {
							list.set("visible", true);
						} else {
							list.set("visible", false);
						}
					}
				}

				// update the active column
				this._updateContentSize();
				//columns[activeIndex].list._node.scrollIntoView();
			} else {
				this._activeIndex = 0;
				this._updateColumn(0);
			}
		},
		
		/** 
		* Public functions to fetch ids and labels from result items
		*/
		itemId : function(item) {
			var id = item.id ? item.id : item;
			return id;
		},
		itemLabel : function(item) {
			var label = item.label ? item.label : item;
			return label;
		},
		
		/**
		* Handles the selection of a resource list item.
		* Fires the itemSelect event
		* 
		* @private
		* @param listItem {Object} the list element node
		* @param resource {Object} the selected resource
		**/
		_itemSelect : function(listItem, oItem, index) {
			var columns = this.get("columns"),
				next = index+1;
			
			this.set("selected", oItem);
			this._selectedItem = oItem;
			this._selectedIndex = index;
			
			if(columns[next]||columns[index].repeat) {
				if(oItem.hasNext) {	
					this._updateColumn(next, this.itemId(oItem));
				} else {
					this.syncUI();
				}
			}
			
			this.fire("itemSelect", oItem, index, listItem);
		},
		_setActiveColumn : function(e, index) {
			this._selectedIndex = index>=1 ? index-1 : null;
			this._activeIndex = index;
			this.syncUI();
		},
			
		/**
		* Creates a new column	based on Y.mazzle.ResourceList
		*
		* @private
		**/ 
		_updateColumn : function(index, parent) {	
			if(!this.get("columns")[index]) {
				this.get("columns")[index] = {};
			}

			var column = this.get("columns")[index];
 			if(!column.list) {
				column.list = this._createColumnList(index);
			}
			if(parent) {
				column.list.set("params.parent", parent);
			}
			column.list.updateContent();
		},
		
		_createColumnList : function(index) {
			var columns = this.get("columns"),
				previous = columns[index-1]||{},
				column = columns[index];
	
			var cfg = {
				width: this.get("columnWidth"),
				maxNumberItems: this.get("maxNumberItems"),
				minQueryLength: this.get("minQueryLength"),
				queryDelay: this.get("queryDelay")
			};

			column.repeat = column.repeat||previous.repeat;
			column.label = column.label || (column.repeat&&previous.label);
				
			// column properties are defined or inherited from previous column			
			cfg.datasource = this.get("datasource");
			cfg.request = column.request||(column.repeat&&previous.request);	
			cfg.params = column.params||(column.repeat&&previous.params);
			cfg.options = column.options||(column.repeat&&previous.options);
			
			var list = new Y.mazzle.ResourceList(cfg);
			if(column.formatter) {
				list.formatItem = column.formatter
			}
			list.render(this.columnsNode);
			
			
			list.on("itemClick", this._itemSelect, this, index);
			list.on("beforeContentUpdate", this._setActiveColumn, this, index);
			return list;
		},
									
		/**
		 * Handles resizing column content by
		 * setting the size of this.colomnsNode to the width of the combined columns
		 **/
		_updateContentSize : function() {
			var columns = this.get("columns"),
				content = this.columnsNode,
				width = 0;
			
			for (var i=0; i < columns.length; i++) {
				var columnList = columns[i].list;
				if(columnList&&columnList.get("visible")) {
					width += columnList.get("boundingBox").get("offsetWidth");
				}
			}
			
			content.setStyle("width", width+"px");
			content.get("parentNode").removeClass("noscroll");
			this._updateColumnsSize();
		},
		
		_updateColumnsSize : function() {
			var columns = this.get("columns"),
				height = this.columnsNode.get("offsetHeight");
			for (var i=0; i < columns.length; i++) {
				if(columns[i].list) {
					columns[i].list.set("height", height+"px");
				}
			}
		}
				
	}); 

}, 'gallery-2010.03.02-18' ,{requires:['node','event','widget','resourcelist','value-change']});