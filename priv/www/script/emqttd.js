/*
	Emqttd Dashboard Javascript.
 */

// 取消缓存设置
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
function addClick() {
	// 注册单击事件
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
}

function Overview() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};

	// 加载overview模块
	loading('overview.html', function() {
		var sysInfo = jQuery('#sys_basic_info');
		// 在sysInfo范围内查询
		_t.elements.sysName = jQuery('#sys_name', sysInfo);
		_t.elements.sysVersion = jQuery('#sys_version', sysInfo);
		_t.elements.sysUptime = jQuery('#sys_uptime', sysInfo);
		_t.elements.sysTime = jQuery('#sys_time', sysInfo);
		_t._broker();
		// 定时任务
		_t.timetask = setInterval(function() {
			_t._broker();
		}, 1000);
		_t._stats();
		_t._metrics();
		_t._listeners();
	});
}

Overview.prototype = {

	// 加载系统基本信息
	_broker : function() {
		var _t = this;
		var options = {
			url : 'api/broker',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				_t.elements.sysName.text(d.sysdescr);
				_t.elements.sysVersion.text(d.version);
				_t.elements.sysUptime.text(d.uptime);
				_t.elements.sysTime.text(d.datetime);
			},
			error : function(e) {
				console.log('api/broker->error');
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
			success : function(d) {
				jQuery('#subscribers_max').text(d['subscribers/max']);
				jQuery('#topics_count').text(d['topics/count']);
				jQuery('#clients_count').text(d['clients/count']);
				jQuery('#topics_max').text(d['topics/max']);
				jQuery('#queues_count').text(d['queues/count']);
				jQuery('#sessions_count').text(d['sessions/count']);
				jQuery('#sessions_max').text(d['sessions/max']);
				jQuery('#queues_max').text(d['queues/max']);
				jQuery('#clients_max').text(d['clients/max']);
				jQuery('#subscribers_count').text(d['subscribers/count']);
			},
			error : function(e) {
				console.log('api/stats->error');
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
			success : function(d) {
				$('#bytes_received').text(d['bytes/received']);
				$('#bytes_sent').text(d['bytes/sent']);
				$('#messages_dropped').text(d['messages/dropped']);
				$('#messages_received').text(d['messages/received']);
				$('#messages_retained_count')
						.text(d['messages/retained/count']);
				$('#messages_sent').text(d['messages/sent']);
				$('#messages_stored_count').text(d['messages/stored/count']);
				$('#packets_connack').text(d['packets/connack']);
				$('#packets_connect').text(d['packets/connect']);
				$('#packets_disconnect').text(d['packets/disconnect']);
				$('#packets_pingreq').text(d['packets/pingreq']);
				$('#packets_pingresp').text(d['packets/pingresp']);
				$('#packets_publish_received').text(
						d['packets/publish/received']);
				$('#packets_publish_sent').text(d['packets/publish/sent']);
				$('#packets_received').text(d['packets/received']);
				$('#packets_sent').text(d['packets/sent']);
				$('#packets_suback').text(d['packets_suback']);
				$('#packets_subscribe').text(d['packets/subscribe']);
				$('#packets_unsuback').text(d['packets/unsuback']);
				$('#packets_unsubscribe').text(d['packets/unsubscribe']);
			},
			error : function(e) {
				console.log('api/metrics->error');
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
				console.log('api/listeners->error');
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {
		clearInterval(this.timetask);
	}

};

function Clients() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};

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
						tby.append('<tr><td>' + cli['clientId'] + '</td><td>'
								+ cli['session'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				console.log('api/clients->error');
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
			url : 'api/session',
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
						tby.append('<tr><td>' + ses['mqtt_session']
								+ '</td><td>' + ses['clientId'] + '</td><td>'
								+ ses['ipaddress'] + '</td><td>'
								+ ses['session'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				console.log('api/session->error');
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

	// 加载模块
	loading('topic.html', function() {
		_t._topic();
		_t._loadTree();
	});
}

Topic.prototype = {

	// 加载topic
	_topic : function() {
		var _t = this;
		var options = {
			url : 'api/topic',
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
				console.log('api/topic->error');
			}
		};
		jQuery.ajax(options);
	},

	_loadTree : function() {
		setup("body");
		var options = {
			url : 'api/topic',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				for (var i = 0; i < d.length; i++) {
					addNode(d[i][topic], 1);
				}
			},
			error : function(e) {
				console.log('api/topic->error');
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
			url : 'api/subscriber',
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
						tby.append('<tr><td>' + sub['mqtt_subscriber']
								+ '</td><td>' + sub['topic'] + '</td><td>'
								+ sub['qos'] + '</td></tr>');
					}
				}
			},
			error : function(e) {
				console.log('api/subscriber->error');
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {

	}

};
