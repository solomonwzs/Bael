$(function()
{
	var json={
		action:'test',
		person:{
			id:0,
			name:'nobody'
		}
	};
	$.post('/ajax_test', {'json': JSON.stringify(json)}, function(){}, 'json');
//	$.ajax({
//		url:'/ajax_test',
//		type:'POST',
//		dataType:{"json":JSON.stringify(json)},
//		data:json,
//		success:function(res)
//		{
//			alert(res);
//		},
//		error:function()
//		{
//		}
//	});
});
