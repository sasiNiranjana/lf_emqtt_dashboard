/*
	Emqttd Dashboard Javascript.
 */

// 取消缓存设置
$.ajaxSetup({
	cache : false
});

function loading(module, fun) {
	var loadingAjax = jQuery('#contents');
	loadingAjax.empty().append(
			'<div styles="margin: 0 10px;' + 'margin-bottom: 20px;">'
					+ '<img src="images/Icons/Load/load-13.gif"'
					+ ' alt="load"></div>');
	loadingAjax.load(module, fun);
}

function tabClass(e) {
	$('#tab_nav>li').each(function() {
		$(this).removeClass('active_tab');
	});
	$(e).addClass('active_tab');
}

var overview = null;
var clients = null;
var session = null;
var topic = null;
var subpub = null;
$(document).ready(function() {
	overview = new Overview();
	
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
});

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

	// 关闭任务（定时任务等）
	closeTask : function() {
		
	}

};

function Clients() {
	var _t = this;
	// 网页标签元素
	_t.elements = {};

	// 加载模块
	loading('clients.html', function() {
		
	});
}

Clients.prototype = {

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
		
	});
}

Session.prototype = {

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
		
	});
}

Topic.prototype = {

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
		
	});
}

Subpub.prototype = {

	// 关闭任务（定时任务等）
	closeTask : function() {
		
	}

};