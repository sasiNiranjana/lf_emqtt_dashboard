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
					+ '<img src="Images/Icons/Load/load-13.gif"'
					+ ' alt="load"></div>');
	loadingAjax.load(module, fun);
}

var overview = null;
$(document).ready(function() {
	overview = new Overview();
	
	// 注册单击事件
	
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
		var options = {
			url : 'api/broker',
			type : 'POST',
			dataType : 'json',
			data : {},
			success : function(d) {
				this.elements.sysName.text(d.sysdescr);
				this.elements.sysVersion.text(d.version);
				this.elements.sysUptime.text(d.uptime);
				this.elements.sysTime.text(d.datetime);
			},
			error : function(e) {
			}
		};
		jQuery.ajax(options);
	},

	// 关闭任务（定时任务等）
	closeTask : function() {
		
	}

};
