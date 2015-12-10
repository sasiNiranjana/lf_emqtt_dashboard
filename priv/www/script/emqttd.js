/*
	emqttd dashboard Javascript.
 */

// 取消缓存设置

Date.prototype.format = function(format) {
	var o = {
		"M+" : this.getMonth()+1, //month
		"d+" : this.getDate(), //day
		"h+" : this.getHours(), //hour
		"m+" : this.getMinutes(), //minute
		"s+" : this.getSeconds(), //second
		"q+" : Math.floor((this.getMonth()+3)/3), //quarter
		"S" : this.getMilliseconds() //millisecond
	}

	if(/(y+)/.test(format)) {
		format = format.replace(RegExp.$1, (this.getFullYear()+"").substr(4 - RegExp.$1.length));
	}

	for(var k in o) {
		if(new RegExp("("+ k +")").test(format)) {
			format = format.replace(RegExp.$1, RegExp.$1.length==1 ? o[k] : ("00"+ o[k]).substr((""+ o[k]).length));
		}
	}
	return format;
}

$.ajaxSetup({
	cache : false
});

function loading(module, fun) {
	var loadingAjax = jQuery('#contents');
	loadingAjax.empty().append('<div class="loading"></div>');
	loadingAjax.load(module, fun);
}

function tabClass(e) {
	closeTask();
	$('#tab_nav>li').each(function() {
		$(this).removeClass('active-tab');
	});
	$('#tab_nav_2>li').each(function() {
		$(this).removeClass('active-tab');
	});
	$(e).addClass('active-tab');
}

function closeTask() {
	if (overview != null) {
		overview.closeTask();
	}
}

var overview = null;
var clients = null;
var session = null;
var topic = null;
var subpub = null;
var user = null;
var httpApi = null;
function showCurrentUser(){
	//load current_user
	var option = {
			url : 'api/current_user',
			type : 'POST',
			dataType : 'json',
			success : function(d) {
				jQuery('#current_user').text(d.username);
			},
			error : function(e) {
			}
		};
	jQuery.ajax(option);
}

function createXMLObject()
{
	try
	{
		if(window.XMLHttpRequest)
		{
			xmlhttp = new XMLHttpRequest();
		}
		 //code for IE5、IE6
		 else if(window.ActiveXObject)
		 {
		 xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
		 }
		 }
		 catch(e)
		 {
		 xmlhttp=false;
		 }
		 return xmlhttp;
		 }

function clearAuth()
{
	try
	{
		if(navigator.userAgent.indexOf("MSIE")>0) //IE浏览器实现注销功能
		{
			document.execCommand("ClearAuthenticationCache");
		}
		else if(isFirefox=navigator.userAgent.indexOf("Firefox")>0) //Firefox实现注销功能
		{
			var xmlhttp = createXMLObject();
			xmlhttp.open("GET",".force_logout_offer_login_mozilla",true,"logout","logout");
			xmlhttp.send("");
			xmlhttp.abort();
		}
		else //Google Chrome等浏览器实现注销功能
		{
			var options = {
			url : 'api/logout',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
			},
			error : function(e) {
			}
			};
		jQuery.ajax(options);

		}
	}
	catch(e)
	{
		alert("there was an error");
		return false;
	}
	window.parent.location.href='/';
}


function addClick() {
	showCurrentUser();
	// 注册单击事件
	$('#logout').click(function(){
		clearAuth();
	});
	$('#tab_nav>li').each(function(index) {
		switch (index) {
		case 0:
			$(this).click(function() {
				tabClass(this);
				overview = new Overview();
			});
			break;
		case 1:
			$(this).click(function() {
				tabClass(this);
				clients = new Clients();
			});
			break;
		case 2:
			$(this).click(function() {
				tabClass(this);
				session = new Session();
			});
			break;
		case 3:
			$(this).click(function() {
				tabClass(this);
				topic = new Topic();
			});
			break;
		case 4:
			$(this).click(function() {
				tabClass(this);
				subpub = new Subpub();
			});
			break;
		default:
			break;
		}
	});
	$('#tab_nav_2>li').each(function(index) {
		switch (index) {
		case 0:
			$(this).click(function() {
				tabClass(this);
				user = new User();
			});
			break;
		case 1:
			$(this).click(function() {
				tabClass(this);
				httpApi = new HttpApi();
			});
			break;
		default:
			break;
		}
	});
}

function Overview() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};
	jQuery('#model_title').text("Overview");

	// 加载overview模块
	loading('overview.html', function() {
		var sysInfo = jQuery('#sys_basic_info');
		// 在sysInfo范围内查询
		_t.elements.sysName = jQuery('#sys_name', sysInfo);
		_t.elements.sysVersion = jQuery('#sys_version', sysInfo);
		_t.elements.sysUptime = jQuery('#sys_uptime', sysInfo);
		_t.elements.sysTime = jQuery('#sys_time', sysInfo);
		_t._broker();
		_t._nodes();
		_t._stats();
		_t._metrics();
		_t._listeners();
		_t.timetasks = setInterval(function(){
			_t._nodes();
			_t._metrics();
			_t._stats();
		}, 10000)
	});
}

Overview.prototype = {

	// 加载系统基本信息
	_broker : function() {
		var _t = this;
//		var options = {
//			url : 'api/broker',
//			type : 'POST',
//			dataType : 'json',
//			data : {},
//			success : function(d) {
//				_t.elements.sysName.text(d.sysdescr);
//				_t.elements.sysVersion.text(d.version);
//				_t.elements.sysUptime.text(d.uptime);
//				_t.elements.sysTime.text(d.datetime);
//			},
//			error : function(e) {
//				console.log('api/broker->error');
//			}
//		};
//		jQuery.ajax(options);
		
		_t.client = new Paho.MQTT.Client(location.hostname, 8083, 'dashboard_' + new Date().getTime());
		var c = _t.client;
		var options = {
			url : 'api/bnode',
			type : 'POST',
			dataType : 'json',
			success : function(d) {
				c.connect({
			onSuccess : function() {
				//console.log("The client connect success.");
				c.subscribe("\$SYS/brokers/"+ d.node +"/+");
			},
			userName : "dashboard",
			password : ""
		});

			},
		error : function(e) {
			//console.log('api/node->error');
		}
	};
	jQuery.ajax(options);

		c.onConnectionLost = onConnectionLost;
		c.onMessageArrived = onMessageArrived;
		// called when the client loses its connection
		function onConnectionLost(responseObject) {
			if (responseObject.errorCode !== 0) {
				//console.log("onConnectionLost: " + responseObject.errorMessage);
			}
		}
		// called when a message arrives
		function onMessageArrived(message) {
			//console.log("onMessageArrived: " + message.destinationName + "-:-" + message.payloadString);
			var topic = message.destinationName;
			var lastNum = topic.lastIndexOf("/");
			var endStr = topic.substring(lastNum);
			if (endStr == "/sysdescr"){
				_t.elements.sysName.text(message.payloadString);
			} else if (endStr == "/version") {
				_t.elements.sysVersion.text(message.payloadString);
			} else if (endStr == "/uptime") {
				_t.elements.sysUptime.text(message.payloadString);
			} else {
				_t.elements.sysTime.text(message.payloadString);
			}

		}
	},

	// 加载nodes
	_nodes: function() {
		var _t = this;
		var options = {
			url : 'api/node',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				if (d.length == 0) {
					var lTable = jQuery('#nodes');
					lTable.hide();
					lTable
							.parent()
							.append(
									'<p style="padding: 12px;">... no node...</p>');
				} else {
					var tby = jQuery('#nodes tbody').empty();
					for (var i = 0; i < d.length; i++) {
						var lis = d[i];
						tby.append('<tr><td>' + lis['name'] + '</td><td >'
								+ lis['process_used']+ ' / ' + lis['process_available'] + '</td><td>'
								+ lis['load1'] + ' / ' +  lis['load5'] + ' / ' + lis['load15']
								+ '</td><td>'
								+ lis['used_memory'] + ' / ' + lis['total_memory'] + '</td><td>' + lis['max_fds'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				//console.log('api/node->error');
			}
		};
		jQuery.ajax(options);
	},


	// 加载stats
	_stats : function() {
		var _t = this;
		var options = {
			url : 'api/stats',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(data) {
                for(var key in data) {
                    $('#' + key.split('/').join('_')).text(data[key]);
                }
			},
			error : function(e) {
				//console.log('api/stats->error');
			}
		};
		jQuery.ajax(options);
	},

	// 加载metrics
	_metrics : function() {
		var _t = this;
		var options = {
			url : 'api/metrics',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(data) {
                for(var key in data) {
                    $('#' + key.split('/').join('_')).text(data[key]);
                }
			},
			error : function(e) {
				//console.log('api/metrics->error');
			}
		};
		jQuery.ajax(options);
	},

	// 加载listeners
	_listeners : function() {
		var _t = this;
		var options = {
			url : 'api/listeners',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				if (d.length == 0) {
					var lTable = jQuery('#listeners');
					lTable.hide();
					lTable
							.parent()
							.append(
									'<p style="padding: 12px;">... no listeners ...</p>');
				} else {
					var tby = jQuery('#listeners tbody').empty();
					for (var i = 0; i < d.length; i++) {
						var lis = d[i];
						tby.append('<tr><td>' + lis['protocol'] + '</td><td>'
								+ lis['port'] + '</td><td class="ta-right">'
								+ lis['max_clients']
								+ '</td><td class="ta-right">'
								+ lis['current_clients'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				//console.log('api/listeners->error');
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {
		clearInterval(this.timetasks);
		if(this.client) {
			try {
				this.client.disconnect();
			}catch (error) {

		}

		}
	}

};

function Clients() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};
	jQuery('#model_title').text("Clients");

	// 加载模块
	loading('clients.html', function() {
		_t._clients();
	});
}

Clients.prototype = {
	// 加载clients
	_clients : function() {
		var _t = this;
		var options = {
			url : 'api/clients',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				$('#clients_count').text(d.length);
				if (d.length == 0) {
					var cTable = jQuery('#clients');
					cTable.hide();
					cTable.parent().append(
							'<p style="padding: 12px;">... no clients ...</p>');
				} else {
					var tby = jQuery('#clients tbody').empty();
					for (var i = 0; i < d.length; i++) {
						var cli = d[i];
						tby.append('<tr><td>' + cli['clientId'] + '</td><td>' + cli['username'] + '</td><td>' + cli['ipaddress'] + '</td><td>' + cli['port'] + '</td><td>' + cli['clean_sess'] + '</td><td>' + cli['proto_ver'] + '</td><td>' + cli['keepalive'] + '</td><td>' + cli['connected_at'] + '</td> </tr>');
					}
				}
			},
			error : function(e) {
				//console.log('api/clients->error');
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {

	}

};

function Session() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};
	jQuery('#model_title').text("Sessions");

	// 加载模块
	loading('session.html', function() {
		_t._session();
	});
}

Session.prototype = {

	// 加载session
	_session : function() {
		var _t = this;
		var options = {
			url : 'api/sessions',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				$('#session_count').text(d.length);
				if (d.length == 0) {
					var sTable = jQuery('#session');
					sTable.hide();
					sTable.parent().append(
							'<p style="padding: 12px;">... no session ...</p>');
				} else {
					var tby = jQuery('#session tbody').empty();
					for (var i = 0; i < d.length; i++) {
						var ses = d[i];
						tby.append('<tr><td>' + ses['clientId']
								+ '</td><td>' + ses['clean_sess'] + '</td><td>' + ses['max_inflight'] + '</td><td>' + ses['inflight_queue'] + '</td><td>' + ses['message_queue'] + '</td><td>' + ses['message_dropped'] + '</td><td>' + ses['awaiting_rel'] + '</td><td>' + ses['awaiting_ack'] + '</td><td>' + ses['awaiting_comp'] + '</td><td>' + ses['created_at'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				//console.log('api/session->error');
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {

	}

};

function Topic() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};
	jQuery('#model_title').text("Topics");

	// 加载模块
	loading('topic.html', function() {
		_t._topic();
        //Don't load topic tree
		//_t._loadTree();
	});
}

Topic.prototype = {

	// 加载topic
	_topic : function() {
		var _t = this;
		var options = {
			url : 'api/topics',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				$('#topic_count').text(d.length);
				if (d.length == 0) {
					var sTable = jQuery('#topic');
					sTable.hide();
					sTable.parent().append(
							'<p style="padding: 12px;">... no topic ...</p>');
				} else {
					var tby = jQuery('#topic tbody').empty();
					for (var i = 0; i < d.length; i++) {
						var top = d[i];
						tby.append('<tr><td>' + top['topic'] + '</td><td>'
								+ top['node'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				//console.log('api/topic->error');
			}
		};
		jQuery.ajax(options);
	},

	_loadTree : function() {
		setup("body");
		var options = {
			url : 'api/topics',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				for (var i = 0; i < d.length; i++) {
					addNode(d[i]['topic'], 1);
				}
			},
			error : function(e) {
				//console.log('api/topic->error');
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {

	}

};

function Subpub() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};
	jQuery('#model_title').text("Subscriptions");

	// 加载模块
	loading('subpub.html', function() {
		_t._subpub();
	});
}

Subpub.prototype = {

	// 加载subpub
	_subpub : function() {
		var _t = this;
		var options = {
			url : 'api/subscriptions',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				$('#subpub_count').text(d.length);
				if (d.length == 0) {
					var sTable = jQuery('#subpub');
					sTable.hide();
					sTable.parent().append(
							'<p style="padding: 12px;">... no subpub ...</p>');
				} else {
					var tby = jQuery('#subpub tbody').empty();
					for (var i = 0; i < d.length; i++) {
						var sub = d[i];
						tby.append('<tr><td>' + sub['clientId']
								+ '</td><td>'
								+ sub['subscriptions'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				//console.log('api/subscriber->error');
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {

	}

};

function User() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};
	jQuery('#model_title').text("Admins");

	// 加载模块
	loading('users.html', function() {
		_t._users();
		$('#add_user_btn').click(function() {
			_t.addUser();
		});
	});
}

User.del = function(userName) {
	user.delUser(userName);
}

User.edit = function(eo) {
	var uo = $('#user_name');
	var tg = $('#tag');
	$(eo).closest('tr').find('td').each(function(i, e) {
		if (i == 0) {
			uo.val($(e).text());
		} else if (i == 1) {
			tg.val($(e).text());
		}
	});
	
	$('#user_title').text('Edit a Admin');
	$('#add_user_span').show();
	$('#add_user_btn').attr('value', 'Update');
	uo.attr('disabled', true);
}

User.add = function() {
	$('#user_title').text('Add a Admin');
	$('#add_user_span').hide();
	$('#add_user_btn').attr('value', 'Add');
	
	var uo = $('#user_name');
	var po = $('#password');
	var p2 = $("#passwd_2")
	var tg = $('#tag');
	uo.attr('disabled', false);
	uo.val('');
	po.val('');
	p2.val('');
	tg.val('');
}

User.prototype = {

	// 加载 users
	_users: function() {
		var _t = this;
		var options = {
			url : 'api/users',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				showCurrentUser();
				$('#users_count').text(d.length);
				if (d.length == 0) {
					var sTable = jQuery('#users');
					sTable.hide();
					sTable.parent().append(
							'<p style="padding: 12px;">... no subpub ...</p>');
				} else {
					var tby = jQuery('#users tbody').empty();
					for (var i = 0; i < d.length; i++) {
						var u = d[i];
						tby.append('<tr><td>' + u['name']
								+ '</td><td>' + u['tag']
								+ '</td><td>' + '<a href="javascript:void(0);" onclick="User.edit(this)">edit</a>'
								+ '&nbsp;<a href="javascript:void(0);" onclick="User.del(\''+u['name']+'\')">delete</a></td></tr>');
					}
				}
			},
			error : function(e) {
				//console.log('api/users->error');
			}
		};
		jQuery.ajax(options);
	},

	delUser : function(userName) {
		var _t = this;
		if (confirm('Are you sure? This object cannot be recovered after deletion')) {
			var options = {
				url : 'api/remove_user',
				type : 'POST',
				dataType : 'json',
				data : {user_name: userName},
				success : function(d) {
					if (d == 1) {
						_t._users();
					} else {
						alert("failure");
					}
				},
				error : function(e) {
					//console.log('api/remover_user->error');
				}
			};
			jQuery.ajax(options);
		}
	},
	
	addUser : function() {
		var _t = this;
		
		var uo = $('#user_name');
		var po = $('#password');
		var p2 = $("#passwd_2")
		var tg = $('#tag');
		uo.val(jQuery.trim(uo.val()));
		po.val(jQuery.trim(po.val()));
		p2.val(jQuery.trim(p2.val()));
		tg.val(jQuery.trim(tg.val()));
		if (uo.val() == '') {
			alert("username is required");
			return;
		}
		
		if (po.val() == '') {
			alert("password is required");
			return;
		}

		if (po.val() != p2.val()) {
			alert("passwords do not match");
			return;
		}
	
		var options = {
			url : 'api/add_user',
			type : 'POST',
			dataType : 'json',
			data : {user_name: uo.val(), password: po.val(), tag: tg.val()},
			success : function(d) {
				if (d == 1) {
					uo.val("");
					po.val("");
					p2.val("");
					tg.val("");
					_t._users();
				} else {
					alert("failure");
				}
			},
			error : function(e) {
				//console.log('api/user_user->error');
			}
		};
		jQuery.ajax(options);
	},
	
	// 关闭任务（定时任务等）
	closeTask : function() {

	}

};

function HttpApi() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};
	jQuery('#model_title').text("HTTP API");

	// 加载模块
	loading('http_api.html', function() {});
}
