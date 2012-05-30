$(function(){
        $('.bael_nav0').each(function(){
                var $li=$(this).find('li');
                var len=$li.size();
                for (var i=0; i<len; i++){
                        var $obj=$($li[i]);
                        if (i===0){
                                $obj.addClass('selected');
                        }
                        $obj.css('width', Math.floor($(this).width()/len)-22);
                }
        });

        $('.bael_nav1').each(function(){
                var $li=$(this).find('li');
                var len=$li.size();
                for (var i=0; i<len; i++){
                        var $obj=$($li[i]);
                        if (i===0){
                                $obj.css('border-left','none').addClass('selected');
                        }
                        $obj.css('width', Math.floor($(this).width()/len)-1);
                }
        });

        $('.bael_nav2').each(function(){
                var $li=$(this).find('li');
                var len=$li.size();
                for (var i=0; i<len; i++){
                        var $obj=$($li[i]);
                        if (i===0){
                                $obj.addClass('selected');
                        }
                        $obj.css('width', Math.floor($(this).width()/len)-1);
                }
        });
});
