/*
	Emqttd Dashboard Javascript.
 */

// 取消缓存设置
$.ajaxSetup({
	cache : false
});

$(document).ready(function() {

	setInterval(function() {
		broker();
	}, 1000);

});

/**
 * 加载系统运行基本信息
 */
function broker() {
	var sysInfo = jQuery('#sys_basic_info');
	// 在sysInfo范围内查询
	var sysName = jQuery('#sys_name', sysInfo);
	var sysVersion = jQuery('#sys_version', sysInfo);
	var sysUptime = jQuery('#sys_uptime', sysInfo);
	var sysTime = jQuery('#sys_time', sysInfo);

	var options = {
		url : 'http://192.168.1.7:18083/api/broker',
		type : 'POST',
		dataType : 'json',
		data : {},
		success : function(d) {
			sysName.text(d.sysdescr);
			sysVersion.text(d.version);
			sysUptime.text(d.uptime);
			sysTime.text(d.datetime);
		},
		error : function(e) {}
	};
	jQuery.ajax(options);
}