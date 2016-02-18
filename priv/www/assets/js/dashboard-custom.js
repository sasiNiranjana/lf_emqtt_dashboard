Date.prototype.format = function(format) {
	var o = {
		"M+" : this.getMonth() + 1, // month
		"d+" : this.getDate(), // day
		"h+" : this.getHours(), // hour
		"m+" : this.getMinutes(), // minute
		"s+" : this.getSeconds(), // second
		"q+" : Math.floor((this.getMonth() + 3) / 3), // quarter
		"S" : this.getMilliseconds() // millisecond
	}

	if (/(y+)/.test(format)) {
		format = format.replace(RegExp.$1, (this.getFullYear() + "")
				.substr(4 - RegExp.$1.length));
	}

	for ( var k in o) {
		if (new RegExp("(" + k + ")").test(format)) {
			format = format.replace(RegExp.$1, RegExp.$1.length == 1 ? o[k]
					: ("00" + o[k]).substr(("" + o[k]).length));
		}
	}
	return format;
};

function loading(module, fun) {
	show_loading_bar({
		pct: 100,
		delay: 0.5,
		finish: function(pct) {
			var loadingAjax = $('#context');
			//loadingAjax.empty().append(
			//	'<div class="page-loading-overlay">'
			//	+ '<div class="loader-2"></div></div>');
			loadingAjax.load(module, fun);
		}
	});
}

var dashApi = null;
function initWebPage(url) {
	dashApi = new DashboardApi();
	dashApi.init();

	// 注册事件
	regEvent();
	// 展现主体部分
	var strs = url.split('#');
	if (strs.length == 1) {
		setMenuClass('overview');
		showOverview();
	} else if (strs[1] == '/clients') {
		setMenuClass('clients');
		showClients();
	} else if (strs[1] == '/sessions') {
		setMenuClass('sessions');
		showSessions();
	} else if (strs[1] == '/topics') {
		setMenuClass('topics');
		showTopics();
	} else if (strs[1] == '/subscriptions') {
		setMenuClass('subscriptions');
		showSubscriptions();
	} else if (strs[1] == '/websocket') {
		setMenuClass('websocket');
		showWebsocket();
	} else if (strs[1] == '/users') {
		setMenuClass('users');
		showUsers();
	} else if (strs[1] == '/http_api') {
		setMenuClass('http_api');
		showHttpApi();
	} else {
		setMenuClass('overview');
		showOverview();
	}
};

/**
 * 设置菜单选中样式
 * 
 * @param modName
 */
function setMenuClass(modName) {
	if (modName == 'overview') {
		$('#title_bar').hide();
	} else {
		$('#title_bar').show();
	}
	if (modName == 'websocket') {
		if (!window.WebSocket) {
			var msg = "WebSocket not supported by this browser.";
			alert(msg);
			throw new Error(msg);
		}
	}
	$('#main-menu>li').each(function() {
		$(this).removeClass('active');
		var mod = $(this).attr('module');
		if (mod == modName) {
			$(this).addClass('active');
		}
	});
};

/**
 * 注册事件
 */
function regEvent() {
	showCurrentUser();
	// 注册单击事件
	$('#logout').on('click', function(ev) {
		ev.preventDefault();
		clearAuth();
	});

	$('#main-menu>li').each(function(index) {
		var mod = $(this).attr('module');
		if (mod == 'overview') {
			$(this).click(function() {
				setMenuClass('overview');
				showOverview();
			});
		} else if (mod == 'clients') {
			$(this).click(function() {
				setMenuClass('clients');
				showClients();
			});
		} else if (mod == 'sessions') {
			$(this).click(function() {
				setMenuClass('sessions');
				showSessions();
			});
		} else if (mod == 'topics') {
			$(this).click(function() {
				setMenuClass('topics');
				showTopics();
			});
		} else if (mod == 'subscriptions') {
			$(this).click(function() {
				setMenuClass('subscriptions');
				showSubscriptions();
			});
		} else if (mod == 'websocket') {
			$(this).click(function() {
				setMenuClass('websocket');
				showWebsocket();
			});
		} else if (mod == 'users') {
			$(this).click(function() {
				setMenuClass('users');
				showUsers();
			});
		} else if (mod == 'http_api') {
			$(this).click(function() {
				setMenuClass('http_api');
				showHttpApi();
			});
		}
	});
};

function clearAuth() {
	dashApi.logout(function(ret, err) {
		if (ret) {
			
		} else {
			console.log(err);
		}
	});
	window.location.href = '/';
}

function showCurrentUser() {
	dashApi.user_current(function(ret, err) {
		if (ret) {
			$('#current_user').text(ret.username);
		} else {
			console.log(err);
		}
	});
};

function showOverview() {
	loading('overview.html', function() {
		// 加载系统基本信息
		overview.broker();
		
		// 加载nodes
		overview.nodes();
		
		// 加载stats
		overview.stats();
		
		// 加载metrics
		overview.metrics();
		
		// 加载listeners
		overview.listeners();
		
		overview.timetask = setInterval(function() {
			overview.nodes();
			overview.stats();
			overview.metrics();
		}, 10000);
	});
};

(function(w) {
	var ov = {};
	var c = null, tt = null;
	
	ov.broker = function() {
		if (c != null) {
			return;
		}
		c = new Paho.MQTT.Client(location.hostname, 8083, 'Dashboard_' + new Date().getTime());
		c.onConnectionLost = onConnectionLost;
		c.onMessageArrived = onMessageArrived;
		// called when the client loses its connection
		function onConnectionLost(responseObject) {
			if (responseObject.errorCode !== 0) {
			}
		}
		// called when a message arrives
		function onMessageArrived(message) {
			var topic = message.destinationName;
			var lastNum = topic.lastIndexOf("/");
			var endStr = topic.substring(lastNum);
			if (endStr == "/sysdescr"){
				$('#sys_name').text(message.payloadString);
			} else if (endStr == "/version") {
				$('#sys_version').text(message.payloadString);
			} else if (endStr == "/uptime") {
				$('#sys_uptime').text(message.payloadString);
			} else {
				$('#sys_time').text(message.payloadString);
			}
		}
		dashApi.bnode(function(ret, err) {
			if (ret) {
				c.connect({
					onSuccess : function() {
						c.subscribe("\$SYS/brokers/"+ ret.node +"/+");
					},
					userName : "dashboard",
					password : ""
				});
			} else {
				console.log(err);
			}
		});
	}
	
	ov.nodes = function() {
		dashApi.nodes(function(ret, err) {
			if (ret) {
				$('#nodes_count').text(ret.length);
				var tby = $('#nodes tbody').empty();
				if (ret.length > 0) {
					for (var i = 0; i < ret.length; i++) {
						var obj = ret[i];
						tby.append('<tr>' +
								'<td>' + obj['name'] + '</td>' +
								'<td>' + obj['process_used'] + ' / ' + obj['process_available'] + '</td>' +
								'<td>' + obj['load1'] + ' / ' + obj['load5'] + ' / ' + obj['load15'] + '</td>' +
								'<td>' + obj['used_memory'] + ' / ' + obj['total_memory'] + '</td>' +
								'<td>' + obj['max_fds'] + '</td>' +
								'</tr>');
					}
				} else {
					tby.append(
							'<tr><td colspan="8">' +
							'<p style="padding: 12px;">... no nodes ...</p>' +
							'</td></tr>');
				}
			} else {
				console.log(err);
			}
		});
	};
	
	ov.stats = function() {
		dashApi.stats(function(ret, err) {
			if (ret) {
				for(var key in ret) {
                    $('#' + key.split('/').join('_')).text(ret[key]);
                }
			} else {
				console.log(err);
			}
		});
	};
	
	ov.metrics = function() {
		dashApi.metrics(function(ret, err) {
			if (ret) {
				for(var key in ret) {
                    $('#' + key.split('/').join('_')).text(ret[key]);
                }
			} else {
				console.log(err);
			}
		});
	};
	
	ov.listeners = function() {
		dashApi.listeners(function(ret, err) {
			if (ret) {
				$('#listeners_count').text(ret.length);
				var tby = $('#listeners tbody').empty();
				if (ret.length > 0) {
					for (var i = 0; i < ret.length; i++) {
						var obj = ret[i];
						tby.append('<tr>' +
								'<td>' + obj['protocol'] + '</td>' +
								'<td>' + obj['port'] + '</td>' +
								'<td>' + obj['max_clients'] + '</td>' +
								'<td>' + obj['current_clients'] + '</td>' +
								'</tr>');
					}
				} else {
					tby.append(
							'<tr><td colspan="8">' +
							'<p style="padding: 12px;">... no listeners ...</p>' +
							'</td></tr>');
				}
			} else {
				console.log(err);
			}
		});
	};

	// 关闭任务
	ov.clearTask = function() {
		try {
			clearInterval(tt);
			c.disconnect();
		} catch (error) {}
	};

	ov.client = c;
	ov.timetask = tt;
	w.overview = ov;
})(window);

function showClients() {
	// 标题导航条
	$('#title_bar .description').text("Clients List");
	$('#title_bar .title').text("Clients");
	$('#title_bar .breadcrumb-env').html(
			'<ol class="breadcrumb bc-1">' +
			'<li><i class="fa-home"></i>Overview</li>' +
			'<li class="active"><strong>Clients</strong></li>' +
			'</ol>');
	
	// 加载Clients信息
	loading('clients.html', function() {
		dashApi.clients(function(ret, err) {
			if (ret) {
				$('#clients_count').text(ret.length);
				var tby = $('#clients tbody').empty();
				if (ret.length > 0) {
					for (var i = 0; i < ret.length; i++) {
						var obj = ret[i];
						tby.append('<tr>' +
								'<td>' + obj['clientId'] + '</td>' +
								'<td>' + obj['username'] + '</td>' +
								'<td>' + obj['ipaddress'] + '</td>' +
								'<td>' + obj['port'] + '</td>' +
								'<td>' + obj['clean_sess'] + '</td>' +
								'<td>' + obj['proto_ver'] + '</td>' +
								'<td>' + obj['keepalive'] + '</td>' +
								'<td>' + obj['connected_at'] + '</td>' +
								'</tr>');
					}
				} else {
					tby.append(
							'<tr><td colspan="8">' +
							'<p style="padding: 12px;">... no clients ...</p>' +
							'</td></tr>');
				}
			} else {
				console.log(err);
			}
		});
	});
};

function showSessions() {
	// 标题导航条
	$('#title_bar .description').text("Sessions List");
	$('#title_bar .title').text("Sessions");
	$('#title_bar .breadcrumb-env').html(
			'<ol class="breadcrumb bc-1">' +
			'<li><i class="fa-home"></i>Overview</li>' +
			'<li class="active"><strong>Sessions</strong></li>' +
			'</ol>');
	
	// 加载Sessions信息
	loading('sessions.html', function() {
		dashApi.sessions(function(ret, err) {
			if (ret) {
				$('#sessions_count').text(ret.length);
				var tby = $('#sessions tbody').empty();
				if (ret.length > 0) {
					for (var i = 0; i < ret.length; i++) {
						var obj = ret[i];
						tby.append('<tr>' +
								'<td>' + obj['clientId'] + '</td>' +
								'<td>' + obj['clean_sess'] + '</td>' +
								'<td>' + obj['max_inflight'] + '</td>' +
								'<td>' + obj['message_queue'] + '</td>' +
								'<td>' + obj['message_dropped'] + '</td>' +
								'<td>' + obj['awaiting_rel'] + '</td>' +
								'<td>' + obj['awaiting_ack'] + '</td>' +
								'<td>' + obj['awaiting_comp'] + '</td>' +
								'<td>' + obj['created_at'] + '</td>' +
								'</tr>');
					}
				} else {
					tby.append(
							'<tr><td colspan="9">' +
							'<p style="padding: 12px;">... no sessions ...</p>' +
							'</td></tr>');
				}
			} else {
				console.log(err);
			}
		});
	});
};

function showTopics() {
	// 标题导航条
	$('#title_bar .description').text("Topics List");
	$('#title_bar .title').text("Topics");
	$('#title_bar .breadcrumb-env').html(
			'<ol class="breadcrumb bc-1">' +
			'<li><i class="fa-home"></i>Overview</li>' +
			'<li class="active"><strong>Topics</strong></li>' +
			'</ol>');
	
	// 加载Topics信息
	loading('topics.html', function() {
		dashApi.topics(function(ret, err) {
			if (ret) {
				$('#topics_count').text(ret.length);
				var tby = $('#topics tbody').empty();
				if (ret.length > 0) {
					for (var i = 0; i < ret.length; i++) {
						var obj = ret[i];
						tby.append('<tr>' +
								'<td>' + obj['topic'] + '</td>' +
								'<td>' + obj['node'] + '</td>' +
								'</tr>');
					}
				} else {
					tby.append(
							'<tr><td colspan="9">' +
							'<p style="padding: 12px;">... no topics ...</p>' +
							'</td></tr>');
				}
			} else {
				console.log(err);
			}
		});
	});
};

function showSubscriptions() {
	// 标题导航条
	$('#title_bar .description').text("Subscriptions List");
	$('#title_bar .title').text("Subscriptions");
	$('#title_bar .breadcrumb-env').html(
			'<ol class="breadcrumb bc-1">' +
			'<li><i class="fa-home"></i>Overview</li>' +
			'<li class="active"><strong>Subscriptions</strong></li>' +
			'</ol>');
	
	// 加载Subscriptions信息
	loading('subscriptions.html', function() {
		dashApi.subscriptions(function(ret, err) {
			if (ret) {
				$('#subscriptions_count').text(ret.length);
				var tby = $('#subscriptions tbody').empty();
				if (ret.length > 0) {
					for (var i = 0; i < ret.length; i++) {
						var obj = ret[i];
						tby.append('<tr>' +
								'<td>' + obj['clientId'] + '</td>' +
								'<td>' + obj['subscriptions'] + '</td>' +
								'</tr>');
					}
				} else {
					tby.append(
							'<tr><td colspan="9">' +
							'<p style="padding: 12px;">... no subscriptions ...</p>' +
							'</td></tr>');
				}
			} else {
				console.log(err);
			}
		});
	});
};

function showWebsocket() {
	// 标题导航条
	$('#title_bar .description').text("MQTT Over Websocket");
	$('#title_bar .title').text("Websocket");
	$('#title_bar .breadcrumb-env').html(
			'<ol class="breadcrumb bc-1">' +
			'<li><i class="fa-home"></i>Overview</li>' +
			'<li class="active"><strong>Websocket</strong></li>' +
			'</ol>');
	
	// 加载WebSocket信息
	loading('websocket.html', function() {
		$('#host').val(location.hostname);
		$('#port').val(8083);
		$('#client_id').val('C_' + new Date().getTime());
		
		var client = null;
		$('#connect_btn').click(function() {
			if(client == null || !client.isConnected()) {
				client = wSocket.newClient();
				wSocket.connect(client);
			} else {
				alert("Don't click");
			}
		});

		$('#disconnect_btn').click(function() {
			if(client != null && client.isConnected()) {
				wSocket.disconnect(client);
			} else {
				alert("Don't click");
			}
		});

		$('#subscribe_btn').click(function() {
			wSocket.subscribe(client);
		});
		
		$('#send_btn').click(function() {
			wSocket.sendMessage(client);
		});
	});
};

(function(w) {
	// called when the client loses its connection
	function onConnectionLost(responseObject) {
		if (responseObject.errorCode !== 0) {
			console.log("onConnectionLost: " + responseObject.errorMessage);
		}
	}

	// called when a message arrives
	function onMessageArrived(message) {
		console.log("onMessageArrived: " + message.payloadString);
		var nowStr = (new Date()).format("yyyy-MM-dd hh:mm:ss");
		$('#receive_message').append('<div>onMessageArrived: ' 
			+ message.payloadString + '<cite> ' 
			+ nowStr + '</cite></div>');
	}
	
	var wSocket = {};
	
	wSocket.newClient = function() {
		var host = $('#host').val();
		var port = $('#port').val();
		var clientId = $('#client_id').val();
		return new Paho.MQTT.Client(host, Number(port), clientId);
	};
	
	wSocket.connect = function(client) {
		// set callback handlers
		client.onConnectionLost = onConnectionLost;
		client.onMessageArrived = onMessageArrived;
		
		var options = {
			onSuccess : function() {
				console.log("The client connect success.");
				$('#connect_state').text('CONNECTED');
				$('#connect_btn').addClass("disabled").removeClass("btn-success").addClass("btn-gray");
				$('#disconnect_btn').removeClass("disabled").removeClass("btn-gray").addClass("btn-success");
			}
		};
		var userName = $('#user_name').val();
		var password = $('#password').val();
		var keepAlive = $('#keep_alive').val();
		if (userName != "") {
			options.userName = userName;
		}
		if (password != "") {
			options.password = password;
		}
		if (keepAlive != "") {
			options.keepAliveInterval = Number(keepAlive);
		}
		client.connect(options);
	};
	
	wSocket.disconnect = function(client) {
		client.disconnect();
		console.log("The client disconnect success.");
		$('#connect_state').text('DISCONNECTED');
		$('#connect_btn').removeClass("disabled").removeClass("btn-gray").addClass("btn-success");
		$('#disconnect_btn').addClass("disabled").removeClass("btn-success").addClass("btn-gray");
	};
	
	wSocket.subscribe = function(client) {
		var topic = $('#subscription').val();
		client.subscribe(topic);
		var nowStr = (new Date()).format("yyyy-MM-dd hh:mm:ss");
		$('#subscriptions_list').append('<div>Subscribe Topic: ' 
			+ topic + '<cite> ' 
			+ nowStr + '</cite></div>');
	};
	
	wSocket.sendMessage = function(client) {
		var topic = $('#topic').val();
		var msg = $('#message').val();
		var message = new Paho.MQTT.Message(msg);
		message.destinationName = topic;
		client.send(message);
		var nowStr = (new Date()).format("yyyy-MM-dd hh:mm:ss");
		$('#send_message_list').append('<div>Send Message: ' 
				+ msg + '<cite> ' 
				+ nowStr + '</cite></div>');
	};
	
	w.wSocket = wSocket;
})(window);

function showUsers() {
	// 标题导航条
	$('#title_bar .description').text("Users List");
	$('#title_bar .title').text("Users");
	$('#title_bar .breadcrumb-env').html(
			'<ol class="breadcrumb bc-1">' +
			'<li><i class="fa-home"></i>Overview</li>' +
			'<li class="active"><strong>Users</strong></li>' +
			'</ol>');
	
	// 加载Users信息
	loading('users.html', function() {
		User.showTable();
		
		if ($('#modal_confirm_del_user').length <= 0) {
			$.ajax({
				url : 'user_modal.html',
				type : 'GET',
				dataType : 'html',
				success : function(ret) {
					$('body').append(ret);
				}
			});
		}
		
		$('#user_add_btn').on('click', function(ev) {
			ev.preventDefault();
			var m = $('#modal_user_add');
			m.modal('show');
			User.setAddDate(m);
		});
	});
};

var User = {
		showTable : function() {
			dashApi.users(function(ret, err) {
				if (ret) {
					$('#users_count').text(ret.length);
					var tby = $('#users tbody').empty();
					if (ret.length > 0) {
						for (var i = 0; i < ret.length; i++) {
							var obj = ret[i];
							tby.append('<tr>' +
									'<td>' + (i + 1) + '</td>' +
									'<td>' + obj['name'] + '</td>' +
									'<td>' + obj['tag'] + '</td>' +
									'<td>' +
									'<a href="javascript:;" onclick="User.editPage(\''+obj.name+'\',\''+obj.tag+'\')" class="btn btn-success btn-sm btn-icon icon-left">' +
									'	Edit' +
									'</a>' +
									'<a href="javascript:;" onclick="User.delPage(\''+obj.name+'\')" class="btn btn-danger btn-sm btn-icon icon-left">' +
									'	Delete' +
									'</a>' +
									'</td>' +
									'</tr>');
						}
					} else {
						tby.append(
								'<tr><td colspan="9">' +
								'<p style="padding: 12px;">... no users ...</p>' +
								'</td></tr>');
					}
				} else {
					console.log(err);
				}
			});
		},
		
		delPage : function(username) {
	    	var m = $('#modal_confirm_del_user');
			m.modal('show');
			m.find('user_del_name').val(username);
		},
	    
	    delSubmit : function() {
	    	var m = $('#modal_confirm_del_user');
	    	var username= m.find('user_del_name').val();
	    	dashApi.user_remove(username, function(ret, err) {
	    		if (ret) {
	    			m.find('user_del_name').val('');
					m.modal('hide');
					User.showTable();
	    		} else {
					console.log(err);
				}
			});
		},
		
		setEditDate : function(m, u) {
			var user = {
					username : '',
					remark : ''
			};
			$.extend(user, u || {});
			m.find('#user_edit_name').val(user.username);
			m.find('#user_edit_remark').val(user.remark);
			m.find('#user_edit_pwd').val('');
			m.find('#user_edit_pwd_1').val('');
		},
		
		setAddDate : function(m, u) {
			var user = {
					username : '',
					remark : ''
			};
			$.extend(user, u || {});
			m.find('#user_add_name').val(user.username);
			m.find('#user_add_remark').val(user.remark);
			m.find('#user_add_pwd').val('');
			m.find('#user_add_pwd_1').val('');
		},
		
		editPage : function(username, remark) {
	        var m = $('#modal_user_edit');
			m.modal('show');
			User.setEditDate(m,
				{username : username,
				remark : remark});
	    },
		
		editSubmit : function() {
			var user = {};
			var m = $('#modal_user_edit');
			user.user_name = m.find('#user_edit_name').val();
			user.tag = m.find('#user_edit_remark').val();
			user.password = m.find('#user_edit_pwd').val();
			user.pwd_1 = m.find('#user_edit_pwd_1').val();
			if (user.user_name == '') {
				alert("Username is required.");
				return;
			}
			if (user.password == '') {
				alert("Password is required.");
				return;
			}
			if (user.password != user.pwd_1) {
				alert("Passwords do not match.");
				return;
			}
			dashApi.user_add(user, function(ret, err) {
				if (ret) {
					User.setEditDate(m);
					m.modal('hide');
					User.showTable();
	    		} else {
					console.log(err);
				}
			});
	    },
		
		addSubmit : function() {
			var user = {};
			var m = $('#modal_user_add');
			user.user_name = m.find('#user_add_name').val();
			user.tag = m.find('#user_add_remark').val();
			user.password = m.find('#user_add_pwd').val();
			user.pwd_1 = m.find('#user_add_pwd_1').val();
			if (user.user_name == '') {
				alert("Username is required.");
				return;
			}
			if (user.password == '') {
				alert("Password is required.");
				return;
			}
			if (user.password != user.pwd_1) {
				alert("Passwords do not match.");
				return;
			}
			dashApi.user_add(user, function(ret, err) {
				if (ret) {
					User.setAddDate(m);
					m.modal('hide');
					User.showTable();
	    		} else {
					console.log(err);
				}
			});
		}
};

function showHttpApi() {
	// 标题导航条
	$('#title_bar .description').text("HTTP API List");
	$('#title_bar .title').text("HTTP API");
	$('#title_bar .breadcrumb-env').html(
			'<ol class="breadcrumb bc-1">' +
			'<li><i class="fa-home"></i>Overview</li>' +
			'<li class="active"><strong>HTTP API</strong></li>' +
			'</ol>');

	// 加载HTTP API信息
	loading('http_api.html', function() {});
};