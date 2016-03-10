/**
 * dashboard-api.js V0.1
 * 
 * Copyright 2016, eMQTT
 * 
 */
;
(function(win) {

	"use strict";

	// 取消缓存
	$.ajaxSetup({
		cache : false
	});

	var EF = function() {
	};

	var dApi = function() {
	};

	dApi.prototype = {

		// 配置项参数
		_config : {
			apiUri : "/"
		},

		// API初始化
		init : function(config) {
			$.extend(this._config, config || {});
		},

		_checkException : function() {
			if (this._config.apiUri == null) {
				throw new Error("Config 'apiUri' is null.");
			}
		},

		// var callback = function(ret, err) {};
		_ajax : function(apiEnd, data, callback) {
			this._checkException();
			$.ajax({
				type : "POST",
				url : this._config.apiUri + "api/" + apiEnd,
				dataType : "json",
				data : data,
				success : function(ret) {
					if ((apiEnd == 'remove_user'
						|| apiEnd == 'add_user')
						&& typeof ret == "object"
						&& ret.stauts == "failure") {
						callback(undefined, ret);
					} else {
						callback(ret, undefined);
					}
				},
				error : function(err) {
					callback(undefined, err);
				}
			});
		},
		
		// bnode
		bnode : function(callback) {
			this._ajax("bnode", null, callback);
		},

		// nodes
		nodes : function(callback) {
			this._ajax("node", null, callback);
		},
		
		// stats
		stats : function(callback) {
			this._ajax("stats", null, callback);
		},

		// metrics
		metrics : function(callback) {
			this._ajax("metrics", null, callback);
		},

		// listeners
		listeners : function(callback) {
			this._ajax("listeners", null, callback);
		},

		// clients
		clients : function(params, callback) {
			this._ajax("clients", params, callback);
		},

		// sessions
		sessions : function(callback) {
			this._ajax("sessions", null, callback);
		},

		// topics
		topics : function(callback) {
			this._ajax("topics", null, callback);
		},

		// subscriptions
		subscriptions : function(callback) {
			this._ajax("subscriptions", null, callback);
		},

		// users
		users : function(callback) {
			this._ajax("users", null, callback);
		},

		// user_remove
		user_remove : function(username, callback) {
			this._ajax("remove_user", {user_name : username}, callback);
		},

		// user_add
		user_add : function(user, callback) {
			this._ajax("add_user", user, callback);
		},

		// user_current
		user_current : function(callback) {
			this._ajax("current_user", null, callback);
		},
		
		// user_update
		user_update : function(callback) {
			this._ajax("update_user", null, callback);
		},

		// logout
		logout : function(callback) {
			this._ajax("logout", null, callback);
		},
		
		// routes
		routes : function(ret, err) {
			this._ajax("routes", null, callback);
		}
	};

	win.DashboardApi = dApi;

})(window);