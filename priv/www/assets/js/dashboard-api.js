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
			apiUri : "/",
			protocol : "http",
			hostname : "",
			port : "18083",
			userName : null,
			password : null
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
		_ajax : function(apiEnd, data, callback, isAddAuth) {
			this._checkException();
			var isAuth = (typeof isAddAuth == "boolean") ? isAddAuth : false;
			var url = this._config.apiUri + "api/" + apiEnd;
			if (isAuth) {
				url = this._config.protocol + "://" + this._config.userName +
						":" + this._config.password + "@" +
						this._config.hostname + ":" + this._config.port + "/" +
						"api/" + apiEnd;
			}
			$.ajax({
				type : "POST",
				url : url,
				dataType : "json",
				data : data,
				success : function(ret) {
					if ((apiEnd == 'remove_user'
						|| apiEnd == 'add_user')
						&& typeof ret == "object"
						&& ret.status == "failure") {
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
		sessions : function(params, callback) {
			this._ajax("sessions", params, callback);
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
			this._config.userName = "logout";
			this._config.password = "logout";
			this._ajax("current_user", null, callback, true);
			// clearAuthenticate();
		},
		
		// routes
		routes : function(callback) {
			this._ajax("routes", null, callback);
		}
	};
	
	var xmlHttp = false;
    //创建HttpRequest对象
    function createXmlHttpRequest() {
        try {
            xmlHttp = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
            try {
                xmlHttp = new ActiveXObject("Microsoft.XMLHTTP");
            } catch (e2) {
                xmlHttp = false;
            }
        }
        if (!xmlHttp && typeof XMLHttpRequest != "udefined") {
            xmlHttp = new XMLHttpRequest();
        }
    };

	function clearAuthenticate() {
		//先创建XMLHttpRequest的对象
		createXmlHttpRequest();
		try {
			// IE浏览器实现注销功能
			if (navigator.userAgent.indexOf("MSIE") > 0) {
				document.execCommand("ClearAuthenticationCache");
			}
			// Firefox实现注销功能  
			else if (navigator.userAgent.indexOf("Firefox") > 0) {
				xmlHttp.open("GET", "/", true,
						"logout", "logout");
				xmlHttp.send("");
			}
			// Google Chrome等浏览器实现注销功能
			else {
				xmlHttp.open("GET", "/", false,
						"logout", "logout");
				xmlHttp.send(null);
			}
			//xmlHttp.abort();
		} catch (e) {
			alert("There was an error");
		}
	};

	win.DashboardApi = dApi;

})(window);
